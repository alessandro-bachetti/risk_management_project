#####################################################
# 1. SETUP E LIBRERIE
#####################################################
rm(list = ls())
library(rstudioapi)
library(nnet)
library(caret) 
library(dplyr) 
library(readr)
library(MASS)
library(ggplot2)
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
# 2. DATA CLEANING & IMPUTATION
#####################################################

# Ciclo for per imputazione (Moda/Mediana)
target_col <- "Risk_Level"
for (col in names(df)) {
  if (col != target_col) {
    if (is.numeric(df[[col]])) {
      df[[col]][is.na(df[[col]])] <- median(df[[col]], na.rm = TRUE)
      df[[col]] <- as.numeric(scale(df[[col]])) # Scalatura immediata
    } else {
      df[[col]] <- as.factor(df[[col]])
      df[[col]][is.na(df[[col]])] <- getmode(df[[col]])
    }
  }
}

df[[target_col]] <- factor(df[[target_col]], levels = c("Low", "Medium", "High", "Critical"))

#####################################################
# 3. FEATURE SELECTION (NUMERICHE: CORR, ANOVA, PCA)
#####################################################
num_vars <- names(df)[sapply(df, is.numeric)]

# 3.1 Pulizia a priori: Correlazione (> 0.7)
cor_mat <- cor(df[num_vars])
high_corr <- findCorrelation(cor_mat, cutoff = 0.7, names = TRUE)
num_vars_filtered <- setdiff(num_vars, high_corr)

# 3.2 Selezione ANOVA (Top 5)
p_vals_anova <- sapply(num_vars_filtered, function(x) {
  summary(aov(df[[x]] ~ df[[target_col]]))[[1]][["Pr(>F)"]][1]
})
top_anova <- names(sort(p_vals_anova)[1:5])

# 3.3 Selezione PCA (Top 5 basate su contribuzione PC1 e PC2)
pca_res <- princomp(df[num_vars_filtered], cor = TRUE)
contributions <- rowSums(pca_res$loadings[, 1:2]^2)
top_pca <- names(sort(contributions, decreasing = TRUE)[1:5])

final_num_vars <- unique(c(top_anova, top_pca))

#####################################################
# 4. FEATURE SELECTION (CATEGORICHE: V-CRAMER, CHI-SQ)
#####################################################
cat_vars <- names(df)[sapply(df, is.factor)]
cat_vars <- cat_vars[cat_vars != target_col]

# 4.1 Pulizia V-Cramer (rimozione ridondanti tra loro)
to_keep_cat <- cat_vars
if(length(cat_vars) > 1) {
  for(i in 1:(length(cat_vars)-1)) {
    for(j in (i+1):length(cat_vars)) {
      v_val <- assocstats(table(df[[cat_vars[i]]], df[[cat_vars[j]]]))$cramer
      if(!is.na(v_val) && v_val > 0.7) to_keep_cat <- setdiff(to_keep_cat, cat_vars[j])
    }
  }
}

# 4.2 Selezione Chi-Quadro (Top 5 rispetto al Target)
p_vals_chi <- sapply(to_keep_cat, function(x) {
  chisq.test(table(df[[x]], df[[target_col]]))$p.value
})
top_categorical <- names(sort(p_vals_chi)[1:5])

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
acc_logit_bin <- confusionMatrix(pred_logit_bin, data_test_bin$Risk_Binary)$overall['Accuracy']

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


# Plot LDA Multinomiale
lda_p <- predict(m_lda, data_test)
lda_df <- data.frame(Risk = data_test$Risk_Level, lda_p$x)
ggplot(lda_df, aes(x=LD1, y=LD2, color=Risk)) + geom_point() + stat_ellipse() + theme_minimal()
