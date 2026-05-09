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
#ggsave("categorical_plots.png", final_cat_plot, width = 20, height = 15, units = "in", dpi = 300)


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
         tl.srt = 45,
         type = "upper",
         method = "ellipse",
         col = colorRampPalette(c("white", "blue"))(200),
         tl.cex = 0.8,
         addCoef.col = "black",
         number.cex = 0.5,
         diag = FALSE,
         is.corr = FALSE,
         # Margini: c(basso, sinistra, alto, destra)
         mar = c(0, 0, 3, 0))

mtext("Cramér's V", side = 3, line = -10, cex = 1.2, adj = 0.55)

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
# 5. update
#####################################################
set.seed(123)
trainIndex <- createDataPartition(df_model[[target_col]], p = .7, list = FALSE)
data_train <- df_model[trainIndex,]
data_test  <- df_model[-trainIndex,]

# Stima del modello Ordered Logit
# Hess = TRUE è importante per poter calcolare gli standard error e i p-value
ordinal_model <- polr(Risk_Level ~ ., 
                      data = data_train, 
                      Hess = TRUE)

print("--- Sommario del Modello Ordered Logit ---")
summary_model <- summary(ordinal_model)
print(summary_model)

# La funzione polr non calcola i p-value di default, li calcoliamo a mano:
ctable <- coef(summary_model)
p_values <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable_with_p <- cbind(ctable, "p value" = p_values)

print("--- Coefficienti, t-value e p-value ---")
print(round(ctable_with_p, 4))


# 5. PREVISIONE E VALUTAZIONE
# Prediciamo le classi sul test set
predictions <- predict(ordinal_model, newdata = data_test)
# Valutiamo le performance con una matrice di confusione
print("--- Matrice di Confusione ---")
confusionMatrix(predictions, data_test$Risk_Level)
