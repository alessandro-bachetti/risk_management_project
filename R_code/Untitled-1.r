#####################################################
# 1. SETUP E LIBRERIE
#####################################################
rm(list = ls())
library(rstudioapi)
library(nnet)
library(car) # corr matrix
library(caret) 
library(dplyr) 
library(readr)
library(MASS)
library(ggplot2)
library(patchwork)
library(corrplot)
library(vcd)       # Per V-Cramer
library(factoextra) # Per PCA visualization
library(pROC)       # Per ROC/AUC

current_path <- getActiveDocumentContext()$path
setwd(dirname(current_path))

# Caricamento dati
df <- read.csv("project_risk_raw_dataset.csv", row.names = 1)

# Funzione per la moda
getmode <- function(v) {
  uniqv <- unique(na.omit(v))
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

#####################################################
# 2. DATA CLEANING & IMPUTATION + OVERALL EDA
#####################################################

# Ciclo for per controllo e sostituzione NA (Moda/Mediana) + scaling quantitative
target_col <- "Risk_Level"
for (col in names(df)) {
  if (col != target_col) {
    if (is.numeric(df[[col]])) {
      df[[col]][is.na(df[[col]])] <- median(df[[col]], na.rm = TRUE)
      df[[col]] <- as.numeric(scale(df[[col]])) # Scaling 
    } else {
      df[[col]] <- as.factor(df[[col]])
      df[[col]][is.na(df[[col]])] <- getmode(df[[col]])
    }
  }
}

df[[target_col]] <- factor(df[[target_col]], levels = c("Low", "Medium", "High", "Critical"))



# Numercial variables plots
num_vars <- names(df)[sapply(df, is.numeric)]

numeric_plots <- lapply(num_vars, function(var) {
  ggplot(df, aes(x = .data[[var]])) +
    geom_histogram(bins = 30, fill = "skyblue", color = "black") +
    theme_minimal() +
    labs(title = var)
})

# Combine all plots into a grid
final_numeric_plot <- wrap_plots(numeric_plots, ncol = 5)
#ggsave("numeric_plots.png", final_numeric_plot, width = 20, height = 15, units = "in", dpi = 300) 

box_plots <- lapply(num_vars, function(var) {
  ggplot(df, aes(x = .data[[var]])) +
    geom_boxplot(fill = "lightgreen") +
    theme_minimal() +
    labs(title = var)
})

# Combine into a grid
final_boxplot <- wrap_plots(box_plots, ncol = 5)
#ggsave("All_Boxplots.png", final_boxplot, width = 20, height = 15, dpi = 300)


# Categorical plots
cat_vars <- names(df)[sapply(df, is.factor)]
cat_vars <- cat_vars[cat_vars != target_col]

categorical_plots <- lapply(cat_vars, function(var) {
  ggplot(df, aes(x = .data[[var]])) +
    geom_bar(fill = "coral") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    labs(title = var)
})


# Combine all plots into a grid
final_cat_plot <- wrap_plots(categorical_plots, ncol = 5)
ggsave("categorical_plots.png", final_cat_plot, width = 20, height = 15, units = "in", dpi = 300)


#####################################################
# 3. FEATURE SELECTION (NUMERICHE: CORR, ANOVA, PCA)
#####################################################


# 3.1 Pulizia a priori: Correlazione (> 0.7)
cor_mat <- cor(df[num_vars])
corrplot(cor_mat, type = "upper", method = "ellipse", tl.cex = 0.7)
high_corr <- findCorrelation(cor_mat, cutoff = 0.7, names = TRUE)
print(high_corr)
num_vars_filtered <- setdiff(num_vars, high_corr)

# 3.2 Selezione ANOVA (Top 5) 
p_vals_anova <- sapply(num_vars_filtered, function(x) {
  summary(aov(df[[x]] ~ df[[target_col]]))[[1]][["Pr(>F)"]][1]
})
top_anova <- names(sort(p_vals_anova)[1:5])
print(top_anova)
# 3.3 Selezione PCA (Top 5 basate su contribuzione PC1 e PC2)
pca_res <- princomp(df[num_vars_filtered], cor = TRUE)
print(summary(pca_res), loading = TRUE)

# Control variable colors using their contributions to the principle axis
fviz_pca_var(pca_res, col.var = "contrib",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE # Avoid text overlapping
) + theme_minimal() + ggtitle("Variables - PCA")


fviz_contrib(pca_res, choice = "var", axes = 1, top = 10) 
fviz_contrib(pca_res, choice = "var", axes = 2, top = 10)
fviz_contrib(pca_res, choice = "var", axes = 3, top = 10)

contributions <- rowSums(pca_res$loadings[, 1:2]^2)
top_pca <- names(sort(contributions, decreasing = TRUE)[1:5])
print(top_pca)

final_num_vars <- unique(c(top_anova, top_pca))
print(final_num_vars)

#####################################################
# 4. FEATURE SELECTION (CATEGORICHE: V-CRAMER, CHI-SQ)
#####################################################

# 4.1 Pulizia V-Cramer (rimozione ridondanti tra loro)

# Create empty matrix
n <- length(cat_vars)
cramer_matrix <- matrix(NA, nrow = n, ncol = n,
                        dimnames = list(cat_vars, cat_vars))

# Loop through all pairs
for(i in 1:n){
  for(j in 1:n){
    if (i != j) {
      tab <- table(df[[cat_vars[i]]], df[[cat_vars[j]]])
      cramer_matrix[i, j] <- assocstats(tab)$cramer
    } else {
      cramer_matrix[i, j] <- 1   # variable with itself
    }
  }
}

corrplot(cramer_matrix,
         title = "Cramér's V",
         tl.srt = 45,
         type = "upper",
         method = "ellipse",
         col = colorRampPalette(c("white", "blue"))(200),
         tl.cex = 0.6,
         addCoef.col = "black",
         number.cex = 0.4,
         diag = FALSE,
         cl.lim = c(0, 1),
         # Margini: c(basso, sinistra, alto, destra)
         mar = c(0, 0, 2, 0))

to_keep_cat <- cat_vars
if(length(cat_vars) > 1) {
  for(i in 1:(length(cat_vars)-1)) {
    for(j in (i+1):length(cat_vars)) {
      v_val <- assocstats(table(df[[cat_vars[i]]], df[[cat_vars[j]]]))$cramer
      if(!is.na(v_val) && v_val > 0.3) to_keep_cat <- setdiff(to_keep_cat, cat_vars[j])
    }
  }
}

print(to_keep_cat)

# 4.2 Selezione Chi-Quadro (Top 5 rispetto al Target)
p_vals_chi <- sapply(to_keep_cat, function(x) {
  chisq.test(table(df[[x]], df[[target_col]]))$p.value
})
top_categorical <- names(sort(p_vals_chi)[1:5])
print(top_categorical)

# Dataset finale per i modelli
final_vars <- c(target_col, final_num_vars, top_categorical)
df_model <- df[, final_vars]

#####################################################
# 5. MODELLI MULTINOMIALI (4 CLASSI)
#####################################################
set.seed(123)
trainIndex <- createDataPartition(df_model[[target_col]], p = .7, list = FALSE)
data_train <- df_model[trainIndex,]
data_test  <- df_model[-trainIndex,]

# Logit Multinomiale
m_logit <- multinom(Risk_Level ~ ., data = data_train, trace = FALSE)
acc_logit <- confusionMatrix(predict(m_logit, data_test), data_test[[target_col]])$overall['Accuracy']


# LDA Multinomiale
m_lda <- lda(Risk_Level ~ ., data = data_train)
acc_lda <- confusionMatrix(predict(m_lda, data_test)$class, data_test[[target_col]])$overall['Accuracy']

# Plot LDA Multinomiale
lda_p <- predict(m_lda, data_test)
lda_df <- data.frame(Risk = data_test$Risk_Level, lda_p$x)
ggplot(lda_df, aes(x=LD1, y=LD2, color=Risk)) + geom_point() + stat_ellipse() + theme_minimal()

#####################################################
# 6. MODELLI BINARI (Low/Med=0, High/Crit=1)
#####################################################
df_model$Risk_Binary <- ifelse(df_model$Risk_Level %in% c("High", "Critical"), 1, 0)
df_model$Risk_Binary <- factor(df_model$Risk_Binary, levels = c(0, 1))

data_train_bin <- df_model[trainIndex,]
data_test_bin  <- df_model[-trainIndex,]

# 6.1 Logit Binaria
m_logit_bin <- glm(Risk_Binary ~ . - Risk_Level, data = data_train_bin, family = binomial)
prob_logit <- predict(m_logit_bin, data_test_bin, type = "response")
pred_logit_bin <- factor(ifelse(prob_logit > 0.5, 1, 0), levels = c(0, 1))

print(confusionMatrix(pred_logit_bin, data_test_bin$Risk_Binary))
acc_logit_bin <- confusionMatrix(pred_logit_bin, data_test_bin$Risk_Binary)$overall['Accuracy']
nullmod <- glm(Risk_Binary~1, data = data_train_bin,family=binomial(link="logit"))
mf_r2 = 1-(-2)*logLik(m_logit_bin)/(-2*logLik(nullmod))
print(mf_r2)
vif(m_logit_bin)

# 6.2 LDA Binaria
m_lda_bin <- lda(Risk_Binary ~ . - Risk_Level, data = data_train_bin)
pred_lda_bin <- predict(m_lda_bin, data_test_bin)
acc_lda_bin <- confusionMatrix(pred_lda_bin$class, data_test_bin$Risk_Binary)$overall['Accuracy']

# 6.3 Analisi ROC/AUC (Logit Binaria)
roc_obj <- roc(data_test_bin$Risk_Binary, prob_logit)
auc_val <- auc(roc_obj)
#####################################################
# 7. CONFRONTO E OUTPUT
#####################################################
cat("\n--- PERFORMANCE SUMMARY ---\n")
results <- data.frame(
  Modello = c("Logit Multinom", "LDA Multinom", "Logit Binario", "LDA Binario"),
  Accuracy = round(c(acc_logit, acc_lda, acc_logit_bin, acc_lda_bin), 4)
)
print(results)
cat("\nAUC Modello Binario:", round(auc_val, 4), "\n")

# Plot ROC
plot(roc_obj, main = paste("ROC Curve (AUC =", round(auc_val, 3), ")"), col = "blue")

