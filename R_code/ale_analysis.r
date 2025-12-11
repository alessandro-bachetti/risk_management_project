rm(list=ls())
library(rstudioapi) # load it
# the following line is for getting the path of your current open file
current_path <- getActiveDocumentContext()$path 
# The next line set the working directory to the relevant one:
setwd(dirname(current_path ))
# you can make sure you are in the right directory
print( getwd() )

library(tidyverse)
library(nnet)       # Per il modello multinomiale
library(MASS)       # Per la selezione Stepwise (stepAIC)
library(car)        # Per il test di Multicollinearità (VIF)
library(DescTools)  # Per il Pseudo R-Squared

# --- 1. PREPARAZIONE DATI ---
df <- readr::read_csv("project_risk_raw_dataset.csv", na=c("N/A", "NA", "None", ""))
# Categorical variables (character or factor type)
cat_vars <- names(df)[sapply(df, function(x) is.character(x) | is.factor(x))]

# Convert them to factors
df[cat_vars] <- lapply(df[cat_vars], as.factor)
df <- na.omit(df) # Rimuoviamo i NA per semplicità
df$Project_ID <- NULL # Rimuoviamo l'ID

# Standardizzazione (Necessaria per la regressione logistica)
nums <- unlist(lapply(df, is.numeric))
df_scaled <- df
df_scaled[,nums] <- scale(df[,nums])

# Impostiamo "Low" come riferimento
df_scaled$Risk_Level <- relevel(df_scaled$Risk_Level, ref = "Low")

# --- 2. SELEZIONE DELLE VARIABILI (STEPWISE AIC) ---
cat("\n--- Inizio Selezione Variabili (Stepwise Backward) ---\n")

# A. Creiamo il "Modello Pieno" con tutte le variabili
full_model <- multinom(Risk_Level ~ ., data = df_scaled, trace = FALSE, contrasts = NULL)

# B. Eseguiamo la selezione Stepwise
# direction = "both" prova sia ad aggiungere che a togliere variabili
# trace = 0 nasconde il log lungo, mettilo a 1 se vuoi vedere i passaggi
step_model <- stepAIC(full_model, direction = "both", trace = 0)

cat("Variabili selezionate automaticamente:\n")
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

# TEST C: Likelihood Ratio Test (Significatività Globale)
# Confronta il nostro modello con un modello nullo (che tira a indovinare)
null_model <- multinom(Risk_Level ~ 1, data = df_scaled, trace = FALSE)
lr_test <- anova(null_model, final_model)

cat("\n--- Likelihood Ratio Test (Il modello serve a qualcosa?) ---\n")
print(lr_test)
# Se Pr(>Chi) < 0.05, il tuo modello è statisticamente valido.