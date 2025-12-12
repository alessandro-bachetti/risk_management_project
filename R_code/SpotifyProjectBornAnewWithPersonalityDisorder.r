library(tidyverse)
library(nnet)       # Per il modello multinomiale
library(MASS)       # Per la selezione Stepwise (stepAIC)
library(car)        # Per il test di Multicollinearità (VIF)
library(DescTools)  # Per il Pseudo R-Squared
library(openxlsx)
library(rstudioapi)
library(ggplot2)
library(patchwork)
library(corrplot)
library(ggcorrplot)

rm(list = ls())
current_path <- getActiveDocumentContext()$path
setwd(dirname(current_path))
print(getwd())

data <- read.table("project_risk_raw_dataset.csv", header = TRUE,
                   sep = ",", quote = "\"", fill = TRUE, comment.char = "",
                   na.strings = c("NA", "na"), stringsAsFactors = TRUE)
#remove ID
data <- data[, -1]
#data$Tech_Environment_Stability <- NULL Ho dubbi solo su questo N/A
#data$Change_Control_Maturity <- NULL
#data$Risk_Management_Maturity <- NULL

#scaling delle variabili numeriche
numeric_cols <- names(data)[sapply(data, is.numeric)]
factor_cols <- names(data)[sapply(data, is.factor)]
data[numeric_cols] <- scale(data[numeric_cols])

# Numercial variables plots
numeric_plots <- lapply(numeric_cols, function(var) {
  ggplot(data, aes(x = data[, var])) +
    geom_histogram(bins = 30, fill = "skyblue", color = "black") +
    theme_minimal() +
    labs(title = var)
})

# Combine all plots into a grid
final_numeric_plot <- wrap_plots(numeric_plots, ncol = 5)
plot(final_numeric_plot)
#ggsave("numeric_plots.png", final_numeric_plot, width = 20, height = 15, units = "in", dpi = 300) # nolint

box_plots <- lapply(numeric_cols, function(var) {
  ggplot(data, aes(x = var)) +
    geom_boxplot(fill = "lightgreen") +
    theme_minimal() +
    labs(title = var)
})

# Combine into a grid
final_boxplot <- wrap_plots(box_plots, ncol = 5)
plot(final_boxplot)
#ggsave("All_Boxplots.png", final_boxplot, width = 20, height = 15, dpi = 300)


# Categorical plots
categorical_plots <- lapply(factor_cols, function(var) {
  ggplot(data, aes_string(x = var)) +
    geom_bar(fill = "coral") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    labs(title = var)
})


# Combine all plots into a grid
final_cat_plot <- wrap_plots(categorical_plots, ncol = 5)
plot(final_cat_plot)
#ggsave("categorical_plots.png", final_cat_plot, width = 20, height = 15, units = "in", dpi = 300) # nolint

# CORRELATION MATRIX

data_num <- data[, numeric_cols]

cor_mat <- cor(data_num)
corr_plot <- ggcorrplot(cor_mat, lab = TRUE, lab_size = 3, type = "lower", outline.col = "white", colors = c("blue", "white", "red")) + coord_fixed() # nolint
#ggsave("correlation_matrix.png", corr_plot, width = 15, height = 15, dpi = 300)

corrplot(cor_mat, type = "upper", method = "ellipse", tl.cex = 0.9)

# Impostiamo "Low" come riferimento
data$Risk_Level <- relevel(data$Risk_Level, ref = "Low")


# A. Creiamo il "Modello Pieno" con tutte le variabili
full_model <- multinom(Risk_Level ~ ., data = data,
                       trace = FALSE, contrasts = NULL, model = TRUE)

# B. Eseguiamo la selezione Stepwise
# direction = "both" prova sia ad aggiungere che a togliere variabili
# trace = 0 nasconde il log lungo, mettilo a 1 se vuoi vedere i passaggi
step_model <- stepAIC(full_model, direction = "both", trace = 0)

print(formula(step_model))

# --- 3. MODELLO FINALE ---
# Il 'step_model' è già il modello ottimizzato, ma lo rifacciamo pulito
final_model <- step_model

# --- 4. TEST DIAGNOSTICI SUL MODELLO ---

# TEST A: Goodness of Fit (Pseudo R-Squared)
# Quanto bene il modello spiega la variabilità dei dati? (0 a 1)
r2 <- PseudoR2(final_model, which = "McFadden")

print(r2)
# Interpretazione: 0.2-0.4 è buono per dati reali. < 0.1 è scarso.


# TEST C: Likelihood Ratio Test (Significatività Globale)
# Confronta il nostro modello con un modello nullo (a caso)
null_model <- multinom(Risk_Level ~ 1, data = data, trace = FALSE)
lr_test <- anova(null_model, final_model)


print(lr_test)
# Se Pr(>Chi) < 0.05, il modello è statisticamente valido.
