# --- 1. LIBRERIE NECESSARIE ---
# Assicurati che i pacchetti siano installati e caricati
library(nnet)
library(caret) 
library(dplyr) 
library(readr)
library(MASS)
library(ggplot2)

# Prepare the environment
rm(list = ls())
library(rstudioapi)
current_path <- getActiveDocumentContext()$path
setwd(dirname(current_path))
print(getwd())

# Funzione per calcolare la moda (per categoriche)
getmode <- function(v) {
   uniqv <- unique(na.omit(v))
   uniqv[which.max(tabulate(match(v, uniqv)))]
}

# --- 2. CARICAMENTO DATI E PULIZIA BASE ---
df <- read_csv("project_risk_raw_dataset.csv")

# Rimozione Project_ID 
df <- df[,-1]
# Definizione Target: "Low" come baseline di riferimento
target_col <- "Risk_Level"
df[[target_col]] <- factor(df[[target_col]], 
                           levels = c("Low", "Medium", "High", "Critical"))



# --- 3. RIMOZIONE na E SCALATURA DEI VALORI NUMERICI ---

num_cols_originali <- names(df)[sapply(df, is.numeric)]

for (col in names(df)) {
  if (col != target_col) {
    if (is.numeric(df[[col]])) {
      #Sostituzione Numerica (Mediana)
      if (any(is.na(df[[col]]))) {
        df[[col]][is.na(df[[col]])] <- median(df[[col]], na.rm = TRUE)
      }
      #Scaling (Standardizzazione : (x-mean)/sd)
      df[[col]] <- scale(df[[col]])
      
    } else {
      #Sostituzione Categorica (Moda)
      df[[col]] <- as.factor(df[[col]])
      if (any(is.na(df[[col]]))) {
        df[[col]][is.na(df[[col]])] <- getmode(df[[col]])
      }
    }
  }
}

# --- 4. FEATURE SELECTION (Usando i dati SCALATI) ---
num_cols_scalate <- names(df)[sapply(df, is.numeric)]

### Correlation Matrix da fare e pulizia delle le variabili a priori su base corr###
# CORRELATION MATRIX

library(corrplot)
library(ggcorrplot)

cor_mat <- cor(df[,num_cols_scalate])
corr_plot <- ggcorrplot(cor_mat, lab = TRUE, lab_size = 3, type = "lower", outline.col = "white", colors = c("blue", "white", "red")) + coord_fixed() 
#ggsave("correlation_matrix.png", corr_plot, width = 15, height = 15, dpi = 300)

corrplot(cor_mat, type = "upper", method = "ellipse", tl.cex = 0.9)


p_vals_num <- sapply(num_cols_scalate, function(x) {
  summary(aov(df[[x]] ~ df[[target_col]]))[[1]][["Pr(>F)"]][1]
})

#pca per confronto ad ANOVA/Sostituzione di ANOVA -> Uso pca per selection 
#PCA
pca_data <- princomp(data_num, cor = TRUE, scores = TRUE)

print(summary(pca_data), loading = TRUE)
top_numeric <- names(sort(p_vals_num)[1:6])

cat_cols <- names(df)[sapply(df, is.factor)]
cat_cols <- cat_cols[cat_cols != target_col]

###vedi  con V-Cramer TRA LE VARIABILI Qualitative A MO' DI CORR-MATRIX per pulizia
### e chi-quadro per selezione

p_vals_cat <- sapply(cat_cols, function(x) {
  tryCatch({
    chisq.test(table(df[[x]], df[[target_col]]))$p.value
  }, error = function(e) { 1 })
})
top_categorical <- names(sort(p_vals_cat)[1:6])

final_vars <- c(target_col, top_numeric, top_categorical)

# CREAZIONE DATASET MODELLO: Usiamo il subsetting base R
df_model <- df[, final_vars]

cat("\nVariabili finali selezionate per il modello:", length(final_vars) - 1, "\n")
print(final_vars)

# --- 5. SPLIT TRAIN/TEST ---
set.seed(123)
trainIndex <- createDataPartition(df_model[[target_col]], p = .7, list = FALSE, times = 1)
data_train <- df_model[trainIndex,]
data_test  <- df_model[-trainIndex,]

# =========================================================================
# MODELLO A: REGRESSIONE LOGISTICA MULTINOMIALE (con dati scalati)
# =========================================================================

cat("\n\n#####################################################")
cat("\n## MODELLO A: LOGISTICA MULTINOMIALE (dati SCALATI) ##")
cat("\n#####################################################\n")

# A1. Addestramento (Baseline: Low)
model_log <- multinom(Risk_Level ~ ., data = data_train, trace = FALSE)

# A2. Test di Wald (P-Values)
z_stats <- summary(model_log)$coefficients / summary(model_log)$standard.errors
p_values <- (1 - pnorm(abs(z_stats), 0, 1)) * 2

cat("\n--- P-Values dei Coefficienti (Baseline: Low) ---\n")
cat("Nota: I coefficienti numerici sono ora comparabili grazie alla scalatura.\n")
print(head(p_values))

# A3. Predizione e Matrice di Confusione
predictions_log <- predict(model_log, newdata = data_test)
cm_log <- confusionMatrix(predictions_log, data_test[[target_col]])

cat("\n--- Matrice di Confusione Logistica Multinomiale ---\n")
print(cm_log)
acc_log <- cm_log$overall['Accuracy']

# =========================================================================
# MODELLO B: LINEAR DISCRIMINANT ANALYSIS (LDA) (con dati scalati)
# =========================================================================

cat("\n\n#####################################################")
cat("\n## MODELLO B: LINEAR DISCRIMINANT ANALYSIS (dati SCALATI) ##")
cat("\n#####################################################\n")

#questo non so che fare, integra con codice Silvia

# B1. Preparazione Dati per LDA: crea le dummy esplicite
formula_lda <- as.formula(paste("~", paste(c(top_numeric, top_categorical), collapse = " + ")))
X_matrix <- model.matrix(formula_lda, data = df_model)[,-1] 
df_lda_ready <- data.frame(X_matrix)
df_lda_ready$Risk_Level <- df_model[[target_col]]

# Splittiamo di nuovo i dati per includere le dummy
data_train_lda <- df_lda_ready[trainIndex,]
data_test_lda  <- df_lda_ready[-trainIndex,]

# B2. Addestramento LDA
model_lda <- lda(Risk_Level ~ ., data = data_train_lda)

cat("\n--- Coefficienti (Pesi) dei Discriminanti Lineari ---\n")
cat("I pesi sono ora significativi e non distorti dalla scala.\n")
print(model_lda$scaling)

# B3. Predizione e Matrice di Confusione
predictions_lda <- predict(model_lda, data_test_lda)
cm_lda <- confusionMatrix(predictions_lda$class, data_test_lda$Risk_Level)

cat("\n--- Confusion Matrix LDA ---\n")
print(cm_lda)
acc_lda <- cm_lda$overall['Accuracy']

# B4. Visualizzazione LDA (il risultato sarà più accurato)
lda_plot_data <- data.frame(Risk_Level = data_test_lda$Risk_Level, predictions_lda$x)

plot_lda <- ggplot(lda_plot_data, aes(x = LD1, y = LD2, color = Risk_Level)) +
  geom_point(alpha = 0.7, size = 2) +
  stat_ellipse(level = 0.95) + 
  theme_minimal() +
  labs(title = "Separazione delle Classi di Rischio (LDA) - Dati Scalati",
       subtitle = paste0("LD1 spiega il ", round(model_lda$svd[1]^2/sum(model_lda$svd^2)*100, 1), "% della varianza"),
       x = "LD1 (Primo Discriminante)", y = "LD2 (Secondo Discriminante)") +
  scale_color_brewer(palette = "Set1")

print(plot_lda)


# --- 9. CONFRONTO FINALE ---
cat("\n\n============================================\n")
cat("CONFRONTO RISULTATI FINALI (Accuratezza):\n")
cat("============================================\n")
cat("Accuratezza Logistica Multinomiale:", round(acc_log, 4), "\n")
cat("Accuratezza LDA:", round(acc_lda, 4), "\n")
cat("============================================\n")

#logit binaria e lda binaria e confronto risultati