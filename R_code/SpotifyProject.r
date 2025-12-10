#####################################
# 1. DATA CLEANING AND PREPARATION 
#####################################

### PROVARE A RIDURRE CODICE CON TIDYVERSE

# Prepare the environment
rm(list=ls())
library(rstudioapi)
current_path <- getActiveDocumentContext()$path
setwd(dirname(current_path))
print(getwd())

# Load data
library(openxlsx)
data=read.csv("project_risk_raw_dataset.csv",  row.names = 1)
View(data)

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
# The result of removing missing values was either a small (1000 obs) or an imbalanced dataset 
# that is why we opted for removing the 3 columns 

data$Tech_Environment_Stability <- NULL
data$Change_Control_Maturity <- NULL
data$Risk_Management_Maturity <- NULL
# data[,names(data_clean)c("Risk_Management_Maturity","Change_Control_Maturity","")]

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
View(data_clean)

# check which scaling to apply

data_clean[num_vars] <- scale(data_clean[num_vars])

str(data_clean)

#####################################
# 2. EDA 
#####################################

library(ggplot2)
library(patchwork)

print(num_vars)
str(num_vars)

# Numercial variables plots
numeric_plots <- lapply(num_vars, function(var) {
  ggplot(data_clean, aes(x = .data[[var]])) + 
    geom_histogram(bins = 30, fill = "skyblue", color = "black") +
    theme_minimal() +
    labs(title = var)
})

# Combine all plots into a grid 
final_numeric_plot <- wrap_plots(numeric_plots, ncol = 5)
ggsave("numeric_plots.png", final_numeric_plot, width = 20, height = 15, units = "in", dpi = 300)

box_plots <- lapply(num_vars, function(var) {
  ggplot(data_clean, aes_string(x = var)) +
    geom_boxplot(fill = "lightgreen") +
    theme_minimal() +
    labs(title = var)
})

# Combine into a grid
final_boxplot <- wrap_plots(box_plots, ncol = 5)
ggsave("All_Boxplots.png", final_boxplot, width = 20, height = 15, dpi = 300)


# Categorical plots
categorical_plots <- lapply(cat_vars, function(var) {
  ggplot(data_clean, aes_string(x = var)) +
    geom_bar(fill = "coral") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    labs(title = var)
})


# Combine all plots into a grid 
final_cat_plot <- wrap_plots(categorical_plots, ncol = 5)
ggsave("categorical_plots.png", final_cat_plot, width = 20, height = 15, units = "in", dpi = 300)

# CORRELATION MATRIX

data_num <- data_clean[, num_vars]

library(corrplot)
library(ggcorrplot)

cor_mat <- cor(data_num)
corr_plot <- ggcorrplot(cor_mat, lab = TRUE, lab_size = 3, type = "lower", outline.col = "white", colors = c("blue", "white", "red")) + coord_fixed()
ggsave("correlation_matrix.png", corr_plot, width = 15, height = 15, dpi = 300)

corrplot(cor_mat, type = "upper", method = "ellipse", tl.cex = 0.9)

### 3. PCA ###

pca_data <- princomp(data_num, cor=TRUE, scores=TRUE)

print(summary(pca_data),loading=TRUE)

# Scree plot
library(factoextra)
fviz_eig(pca_data, ncp = ncol(pca_data$loadings))

#Biplot 
fviz_pca_biplot(pca_data, repel = TRUE, 
                col.var = "steelblue",
                col.ind = "orange") 
# Contributions of variables to PCs
fviz_contrib(pca_data, choice = "var", axes = 1, top = 10)  # PC1 - how big the project is/ project demographics
fviz_contrib(pca_data, choice = "var", axes = 2, top = 10)  # PC2 - seasonal factor
fviz_contrib(pca_data, choice = "var", axes = 3, top = 10)  # PC3 - 
fviz_contrib(pca_data, choice = "var", axes = 4, top = 10)  # PC4 - 
fviz_contrib(pca_data, choice = "var", axes = 5, top = 10)  # PC5
fviz_contrib(pca_data, choice = "var", axes = 6, top = 10)  # PC6
fviz_contrib(pca_data, choice = "var", axes = 7, top = 10)  # PC7
fviz_contrib(pca_data, choice = "var", axes = 8, top = 10)  # PC8
fviz_contrib(pca_data, choice = "var", axes = 9, top = 10)  # PC9
fviz_contrib(pca_data, choice = "var", axes = 10, top = 10) # PC10


# Control variable colors using their contributions to the principle axis
fviz_pca_var(pca_data, col.var="contrib",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE # Avoid text overlapping
) + theme_minimal() + ggtitle("Variables - PCA")

#####################################
# 5. LINEAR REGRESSION MODEL 
#####################################

data_clean$Risk_Binary <- ifelse(
  data_clean$Risk_Level %in% c("Critical", "High"), 1, 0
)
data_clean$Risk_Binary <- factor(data_clean$Risk_Binary, levels = c(0,1))
data_clean$Risk_Binary <- as.numeric(as.character(data_clean$Risk_Binary))

table(as.factor(data_clean$Risk_Binary))

lm_full <- lm(Risk_Binary ~ . - Risk_Level,
              data = data_clean)
summary(lm_full)

# residuals
e=lm_full$residuals


# Normality - NO
shapiro.test(e)
library(tseries)
jarque.bera.test(e)

### Visual check
qqnorm(e)
qqline(e)

# Homoscedasticity - NO
library(lmtest)
bptest(lm_full)

#Visual check
plot(fitted(lm_full), e)
abline(h = 0, col = "red")

# Autocorrelation - NO
dwtest(lm_full)

#####################################
### 6. LOGISTIC REGRESSION ###
#####################################



library(nnet)
glm_model <- glm(Risk_Binary ~ . - Risk_Level,
                 data = data_clean,
                 family = binomial)
summary(glm_model)

nullmod <- glm(Risk_Binary~1, data = data_clean,family=binomial(link="logit"))
mf_r2 = 1-(-2)*logLik(glm_model)/(-2*logLik(nullmod))
print(mf_r2)


library(car)
vif(glm_model)

pred_probs <- predict(glm_model, type = "response")
head(round(pred_probs, 3), n = 10) 

# Predicted outcomes with threshold 0.5
yhat <- as.numeric(pred_probs > 0.5)

# Confusion matrix
table(Predicted = yhat, Actual = data_clean$Risk_Binary)

# ROC and AUC
library(pROC)
roc_curve <- roc(data_clean$Risk_Binary ~ pred_probs)
plot(roc_curve, main = "ROC Curve", xlab = "1 - Specificity", ylab = "Sensitivity")
auc(roc_curve)

#  H0:  beta1 = beta2 = beta3 = beta4 = beta5 = beta6 = 0
## Test LR
LR = glm_model$null.deviance - glm_model$deviance
p_value_LR = pchisq(LR,6,lower.tail=FALSE); p_value_LR

## Analisi: Partial Effects

betalogit <- coef(glm_model)  # includes intercept
X <- model.matrix(glm_model)  # this automatically adds intercept + all predictors
dens_hat <- pred_probs * (1 - pred_probs)  
PE <- dens_hat * X[, -1] %*% diag(betalogit[-1])  # vectorized way

# APE: average partial effect
# la media di tutti gli effetti di un 
# regressore x sulle unita y_i

APE <- colMeans(dens_hat * X[, -1]) * betalogit[-1]


## I coefficienti beta del modello lineare 
# non approssimano gli APE
cbind(APE,lm_full$coefficients[2:ncol(X)])


#####################################
### 7. LDA ###
#####################################
# see if the categories are well distinct

library(MASS)
library(caret)
library(ggvis)
library(class)
library(gmodels)

lda_model <- lda(Risk_Level ~ ., data = data_clean)
lda_model
plot(lda_model)
plot(lda_model,dimen=2)


pred <- predict(lda_model)
scores=pred$x


plot(scores[,1], scores[,2],
     col = as.numeric(data_clean$Risk_Level),
     pch = 19,
     xlab = "LD1", ylab = "LD2")
legend("topright", legend = levels(data_clean$Risk_Level),
       col = 1:3, pch = 19)
head(pred$class)

table(pred$class)
confusionMatrix(pred$class,data_clean$Risk_Level)


#####################################
### 8. CLUSTER ANALYSIS ###
#####################################

dist_E <- dist(data_clean)
auto_co <- hclust(dist_E, method = "complete")
auto_si <- hclust(dist_E, method = "single")
auto_av <- hclust(dist_E, method = "average")
auto_ce <- hclust(dist_E, method = "centroid")
auto_w <- hclust(dist_E, method = "ward.D")

par(mfrow=c(2,3))
plot(auto_si)
plot(auto_co)
plot(auto_av)
plot(auto_ce)
plot(auto_w)
par(mfrow=c(1,1))

# fix import library

par(mfrow=c(2,2))
plot(silhouette(cutree(auto_co,2),dist_E))
plot(silhouette(cutree(auto_co,3),dist_E))
plot(silhouette(cutree(auto_co,4),dist_E))
plot(silhouette(cutree(auto_co,5),dist_E))

par(mfrow=c(1,1))


# K-means method
library(factoextra)
library(NbClust)



# Elbow method
fviz_nbclust(data_num, kmeans, method = "wss") +
  geom_vline(xintercept = 4, linetype = 2) + # add line for better visualisation
  labs(subtitle = "Elbow method")

fviz_nbclust(data_num, kmeans, method = "silhouette") +
  labs(subtitle = "Silhouette method")

model2 <- kmeans(data_num, centers = 2)

BSS2 <- model2$betweenss
TSS2 <- model2$totss
BSS2 / TSS2 * 100

# optimal number of clusters and technique
intern <- clValid(data_num, nClust = 2:24, 
                  clMethods = c("hierarchical","kmeans"), validation = "internal")
summary(intern) %>% kable() %>% kable_styling()

# hierarchical complete 2 clusters
member_co_2 <- cutree(auto_co, 2)
data_co2 <- cbind(data_clean, member_co_2)

fviz_cluster(
  list(data = data_clean, cluster = member_co_2),
  ellipse.type = "norm"
)

### 7. SUMMARY ###

