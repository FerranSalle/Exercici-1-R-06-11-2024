library(knitr)
library(ggplot2)
library(caret)
rm(list=ls())
datos <- read.csv("C:/Users/ferra/Desktop/R/06112024/bank0_train.txt", header=TRUE, sep=";")



##-- Visualizar los datos y descriptiva con summary
View(datos)             # Ver los datos
summary(datos)          # Descriptiva de todas las variables
str(datos)              # Estructura de los datos


############################################################
# Descriptiva bivariante
############################################################
##-- Variables categoricas
sapply(datos,class)
var.cat <- which(sapply(datos,class)=="factor" & names(datos)!="y")  # variables que son categoricas (factores)
##--Mosaicplot para las categoricas
for(vc in var.cat)  mosaicplot(datos[,vc]~datos$y,main=names(datos)[vc],col=2:3,las=1)


# Ensure 'y' is a factor variable
datos$y <- as.factor(datos$y)


# Define categorical and numeric variables
var.cat <- which(sapply(datos, class) == "factor" & names(datos) != "y")
var.num <- which(sapply(datos, class) %in% c("numeric", "integer"))

# Mosaic plot for categorical variables
for(vc in var.cat) {
  mosaicplot(datos[, vc] ~ datos$y, main = names(datos)[vc], col = 2:3, las = 1)
}

# Conditional density plots for numeric variables
for(vn in var.num) {
  cdplot(datos$y ~ datos[, vn], main = names(datos)[vn], n = 512)
}


# Building a Binary Classification Model
#Logistic Regression
mod.glm <- glm(y ~ ., data = datos, family = binomial)
summary(mod.glm)

#Model Evaluation
library(AUC)
pr <- predict(mod.glm, datos, type = "response") # predicted probabilities of y=1
roc_curve <- roc(pr, datos$y) # create ROC curve
plot(roc_curve) # plot ROC curve
AUC::auc(roc_curve) # calculate AUC

#Calculate the Odds Ratios
exp(coef(mod.glm))

#Validation of the model
test <- read.csv("C:/Users/ferra/Desktop/R/06112024/bank0_test.txt", header=TRUE, sep=";",stringsAsFactors = TRUE)

# Ensure 'y' is a factor variable
test$y <- as.factor(test$y)

pr <- predict(mod.glm, test)   # probabilidades predichas
boxplot(pr~test$y)             # Como son estas probabilidades en ambos grupos de respuesta
roc.curve <- roc(pr,test$y)    # Calculo de la curva ROC
plot(roc.curve)                # Dibujo de la curva ROC
AUC::auc(roc.curve)            # AUC de la curva ROC

##-- Sensibilidad y especificidad para un punto de corte concreto
s <- AUC::sensitivity(pr,test$y)
e <- AUC::specificity(pr,test$y)
a <- AUC::accuracy(pr,test$y)
df <- data.frame(cutpoints=s$cutoffs,sens=s$measure,esp=e$measure,acc=a$measure)
View(round(df,3))
