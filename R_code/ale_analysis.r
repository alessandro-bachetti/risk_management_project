#####################################
# 1. DATA CLEANING AND PREPARATION
#####################################
library(tidyverse)
library(nnet)       # Per il modello multinomiale
library(MASS)       # Per la selezione Stepwise (stepAIC)
library(car)        # Per il test di Multicollinearità (VIF)
library(DescTools)  # Per il Pseudo R-Squared
library(openxlsx)
### PROVARE A RIDURRE CODICE CON TIDYVERSE

# Prepare the environment
rm(list = ls())
library(rstudioapi)
current_path <- getActiveDocumentContext()$path
setwd(dirname(current_path))
print(getwd())

# Load data

data <- read.csv("project_risk_raw_dataset.csv",  row.names = 1)

# Check the data
head(data)
str(data)
summary(data)

# Missing values
sum(is.na(data))
unique(data$Tech_Environment_Stability)
unique(data$Change_Control_Maturity)
unique(data$Risk_Management_Maturity)

placeholders <- c("N/A", "NA", "None", "")

data$Tech_Environment_Stability[
  data$Tech_Environment_Stability %in% placeholders
] <- NA

data$Change_Control_Maturity[
  data$Change_Control_Maturity %in% placeholders
] <- NA

data$Risk_Management_Maturity[
  data$Risk_Management_Maturity %in% placeholders
] <- NA

sum(is.na(data))

table(data$Tech_Environment_Stability, useNA = "ifany")
table(data$Change_Control_Maturity, useNA = "ifany")
table(data$Risk_Management_Maturity, useNA = "ifany")

# Critical: 355 High: 642 Low: 697 Medium: 1007
# The result of removing missing values was either a small (1000 obs)
# or an imbalanced dataset
# that is why we opted for removing the 3 columns

data$Tech_Environment_Stability <- NULL
data$Change_Control_Maturity <- NULL
data$Risk_Management_Maturity <- NULL


data_clean <- na.omit(data)
sum(is.na(data_clean))
nrow(data_clean)
table(data_clean$Risk_Level)


# Numeric variables
num_vars <- names(data_clean)[sapply(data_clean, is.numeric)]

# Categorical variables (character or factor type)
cat_vars <- names(data_clean)[sapply(data_clean, function(x) is.character(x) | is.factor(x))]

# Convert them to factors
data_clean[cat_vars] <- lapply(data_clean[cat_vars], as.factor)

str(data_clean)

# check which scaling to apply

data_clean[num_vars] <- scale(data_clean[num_vars])

str(data_clean)

# Impostiamo "Low" come riferimento
data_clean$Risk_Level <- relevel(data_clean$Risk_Level, ref = "Low")


# A. Creiamo il "Modello Pieno" con tutte le variabili
full_model <- multinom(Risk_Level ~ ., data = data_clean, trace = FALSE, contrasts = NULL, model = TRUE)

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
# Ci dice "Quanto bene il modello spiega la variabilità dei dati?" (0 a 1)
r2 <- PseudoR2(final_model, which = "McFadden")
cat("\n--- Bontà del Modello (McFadden R2) ---\n")
print(r2)
# Interpretazione: 0.2-0.4 è eccellente per dati reali. < 0.1 è scarso.

# TEST B: Test di Multicollinearità (VIF)
# Controlliamo se le variabili rimaste sono troppo simili tra loro
# Nota: VIF si calcola meglio su un modello lineare proxy, poiché multinom è complesso
proxy_model <- lm(as.numeric(Risk_Level) ~ ., data = df_scaled[, -which(names(df_scaled) == "Risk_Level")])
vif_values <- vif(proxy_model)

# TEST C: Likelihood Ratio Test (Significatività Globale)
# Confronta il nostro modello con un modello nullo (che tira a indovinare)
null_model <- multinom(Risk_Level ~ 1, data = data_clean, trace = FALSE)
lr_test <- anova(null_model, final_model)

cat("\n--- Likelihood Ratio Test (Il modello serve a qualcosa?) ---\n")
print(lr_test)
# Se Pr(>Chi) < 0.05, il tuo modello è statisticamente valido.