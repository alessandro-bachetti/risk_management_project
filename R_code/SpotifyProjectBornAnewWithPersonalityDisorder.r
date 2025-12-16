#---this is the ultimate .r file of the project---#
#l'utilizzo dei modelli AI sarà limitato alle funzioni complesse da implementare

#---Import delle librerie necessarie---#
library(rstudioapi)
library(ggplot2)

#---Definizione della directory di lavoro---#
rm(list = ls())
current_path <- getActiveDocumentContext()$path
setwd(dirname(current_path))
print(getwd())

#---Import del dataset in formato .csv---#

data <- read.table("project_risk_raw_dataset.csv", header = TRUE,
                   sep = ",", quote = "\"", fill = TRUE, comment.char = "",
                   na.strings = c("NA", "na"), stringsAsFactors = TRUE)

#---Operazioni sul dataframe---#

data <- data[,-1]

num_var <- names(data)[sapply(data, is.numeric)]
cat_var <- names(data)[sapply(data, function(x) { is.factor(x) | is.character(x)})]

data[num_var] <- scale(data[num_var])

summ <- summary(data)

#---Analisi Esplorativa dei Dati---#


numeric_plots <- lapply(num_var, function(var) {
  ggplot(data, aes(x = data[, var])) +
    geom_histogram(bins = 30, fill = "skyblue", color = "black") +
    theme_minimal() +
    labs(title = var)
})


final_numeric_plot <- wrap_plots(numeric_plots, ncol = 5)
plot(final_numeric_plot)
#ggsave("numeric_plots.png", final_numeric_plot, width = 20, height = 15, units = "in", dpi = 300) # nolint

box_plots <- lapply(num_var, function(var) {
  ggplot(data, aes(x = data[,var])) +
    geom_boxplot(fill = "lightgreen") +
    theme_minimal() +
    labs(title = var)
})


final_boxplot <- wrap_plots(box_plots, ncol = 5)
plot(final_boxplot)
#ggsave("All_Boxplots.png", final_boxplot, width = 20, height = 15, dpi = 300)



categorical_plots <- lapply(cat_var, function(var) {
  ggplot(data, aes_string(x = data[, var])) +
    geom_bar(fill = "coral") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    labs(title = var)
})


###---!!! fix dello scaling dei plot!!!---###
final_cat_plot <- wrap_plots(categorical_plots, ncol = 5)
plot(final_cat_plot)
#ggsave("categorical_plots.png", final_cat_plot, width = 20, height = 15, units = "in", dpi = 300) # nolint

###---Analisi numerica delle variabili quantitative per feature-selection---###

cor_mat <- cor(data[,num_var])
corr_plot <- ggcorrplot(cor_mat, lab = TRUE, lab_size = 3, type = "lower", outline.col = "white", colors = c("blue", "white", "red")) + coord_fixed() # nolint
#ggsave("correlation_matrix.png", corr_plot, width = 15, height = 15, dpi = 300)

corrplot(cor_mat, type = "upper", method = "ellipse", tl.cex = 0.9)
             