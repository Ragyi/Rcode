# MIS772 2015 T2
# Title: Assignment 1 
# Author: Ragy Ibrahim
# Date: 21/07/2015

getwd()

#Loading Data
LGA <- read.csv(file = "Vic 2013 LGA Profiles NoPc.csv", header = TRUE)
LGAGeo <- read.csv(file = "Geolocations.csv", header = TRUE)
View(LGA)

#Split Data into Numeric and Categorical Variables

LGA_numeric <- LGA[sapply(LGA,is.numeric)]
LGA_categoric <- LGA[sapply(LGA, is.factor)]

# Impute missing data with column mean
table(is.na(LGA_numeric))
table(is.na(LGA_categoric))

for(i in 1:ncol(LGA_numeric)){
  LGA_numeric[is.na(LGA_numeric[,i]), i] <- mean(LGA_numeric[,i], na.rm = TRUE)}

table(is.na(LGA_numeric))

LGA <- data.frame(c(LGA_categoric, LGA_numeric))
table(is.na(LGA))

#Data Exploration 
str(LGA_numeric)
str(LGA_categoric)

attach(LGA)
#Create Data Frame
quality_health <- data.frame(WellBeing.1, WellBeing.3)
summary(quality_health)

#Feature Engneering, creating a new variable, Life Expectancy
data <- transform (quality_health, c = WellBeing.1 + WellBeing.3)
data <- transform(data, d = c/2)
data$d -> life_expectancy
data

#Examine Distribution
summary(life_expectancy)
plot(density(life_expectancy))
plot(hist(life_expectancy))
boxplot(life_expectancy)

#Definig Quality of Health, according to the scaled distribution
# <81.5 Low, >81.5 High 
quality_of_health <- ifelse(life_expectancy < 81.5, "Poor", "Good")
quality_of_health
table(quality_of_health)

#Insert into Data Frame
LGA["LifeExpectancy"] <- life_expectancy
LGA["QualityofHealth"] <- quality_of_health
LGA_categoric["QualityofHealth"] <- quality_of_health
LGA_numeric["LifeExpectancy"] <- life_expectancy

#Create a Copy of LGA Dataset
LGA.final <- data.frame(LGA, stringsAsFactors = TRUE)
LGA.final <- LGA.final[ , -which(names(LGA.final) %in% c("WellBeing.1", "WellBeing.2", "WellBeing.3", "WellBeing.4", "Injury.9", "Injury.10", "Injury.11", "Injury.12", "Injury.13", "Injury.14", "Injury.15", "Injury.16"))]
LGA.final.numeric <- LGA.final[sapply(LGA.final,is.numeric)]
LGA.final$QualityofHealth <- as.factor(LGA.final$QualityofHealth)
str(LGA.final$QualityofHealth)

detach(LGA)
attach(LGA.final)

#Load Libraries
library(plyr)
library(leaps)
library(caret)
library(mlbench)
library(e1071)
library(ada)
library(Metrics)
library(compiler)
library(plyr)
library(psych)
library(car)
library(rgl)
library(rattle)
library(rpart)

#Partition Dataset 
LGA.final.train <- createDataPartition(LGA.final$QualityofHealth, p = 0.75, list = FALSE)
training <- LGA.final[LGA.final.train, ]
testing <- LGA.final[-LGA.final.train, ]

#Subset for predictors and dependant variables

training.predictors <- training[,1:378]
training.depend.class <- training[,380]
training.depend.num <- training[,379]

testing.predictors <- testing[,1:378]
testing.depend.class <- testing[,380]
testing.depend.num <- testing[,379] 

#Subsetting for numeric variables within Training and Testing
training.numeric <- training[sapply(training,is.numeric)]
testing.numeric <- testing[sapply(testing, is.numeric)]

#Subset for Categorical variables
training.categorical <- training[sapply(training, is.factor)]
testing.categorical <- testing[sapply(testing, is.factor)]

--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
  # Feature Selection - Most important step in the process
--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Method 1 - Filter
  
# Step 1 - Seperate the dependant variable from the predictors
  
  training.numeric.predict <- training.numeric[,1:354]
  training.numeric.depend <- training.numeric[,355]

# Step 2 - Create a vector containing the predictor names
  
  pred.names <- attributes(training.numeric.predict)$names

# Step 3  - Build Filter Control - Random Forest with predictors pre-filtered
  
  filterCtrl <- sbfControl(functions = rfSBF,
                           method = "cv",
                           number = 10,
                           repeats = 10)
# Step 4 - Fit Model to data
  
  set.seed(1986)
  
  rfWithFilter <- sbf(training[,1:378], 
                      training$QualityofHealth, 
                      sbfControl = filterCtrl)
  rfWithFilter
  rfWithFilter$fit
  rfWithFilter$results
  predictors(rfWithFilter)
  
  rf.predict <- predict(rfWithFilter, newdata = testing[,1:378])
  rf.matrix <- confusionMatrix(rf.predict$pred, testing[,380])
  rf.matrix
  

------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Correlations Test
--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

#Correlation Test
correlation <- data.frame(round(cor(LGA.final.numeric, LifeExpectancy), digits = 2))
View(correlation)

#Rename Column
correlation <- rename(correlation, c(" " = "Variables", "round.cor.LGA.final.numeric..LifeExpectancy...digits...2." = "CorCo"))
View(correlation)
  
#Choose variables
highcor <- subset(correlation, CorCo > 0.60 | CorCo < -0.60)
highcor #Examine variable definitions and eleminate duplicates

--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
  # Principle Component Analysis and Linear Regression
--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
  
pca4 <- prcomp( ~., data = training.numeric, scale = TRUE)

loadings <- pca4$rotation

axes <- predict(pca4, newdata = training.numeric)
head(axes, 4)

pca.predict <- data.frame(dat$PC1, dat$PC2)

set.seed(1986)

dat <- cbind(training.numeric, axes)
lmmodel <- lm(LifeExpectancy ~ PC1 + PC2, data = dat)
summary(lmmodel)


panel.lm = function(x, y, ...) {
  tmp <- lm(y ~ x, na.action = na.omit)
  abline(tmp, col="red")
  points(x, y, ...) 
}
coplot(LifeExpectancy ~ PC1 | PC2, pch=19, data=dat, col="blue", panel = panel.lm)

scatter3d(x=training.numeric$LifeExpectancy, z=dat$PC1, y=dat$PC2)
scatter3d(x=dat$PC2, y=training.numeric$LifeExpectancy, z=dat$PC1)
scatter3d(y=dat$PC1, z=dat$PC2, x=training.numeric$LifeExpectancy)

rgl.snapshot(filename = "Linear Model.png")

--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
  # Model Building
--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

# Set Parameter Tuning Metrics

fitControl.1 <- trainControl(method = "repeatedcv",
                             number = 10,
                             repeats = 20)

fitControl.2 <- trainControl(method = "repeatedcv",
                             number = 10,
                             repeats = 20,
                             classProbs = TRUE, 
                             summaryFunction = twoClassSummary)

# Linear Regression

set.seed(1986)

lmFit <- train(LifeExpectancy ~ PC1 + PC2,
                data = dat, 
                method = "lm", 
                metric = "Rsquared")
lmFit
summary(lmFit)

#Naive Bayes
set.seed(1986)

nbFit <- train(training.numeric[,1:354],
               training$QualityofHealth,
                 method = "nb",
                 trControl = fitControl.1,
                 verbose = FALSE, 
               metric = "Kappa")
nbFit
plot(nbFit)
summary(nbFit)
nbFit$finalModel

nb.predict <- predict(nbFit, newdata = testing[,1:378])
nb.matrix <- confusionMatrix(nb.predict, testing[,380])
nb.matrix

#Boosted Tree Model

#Adjusting Grid Parameters and reruning GBM model

gbmGrid <-  expand.grid(interaction.depth = c(1,5,10),
                       n.trees = (1:20)*50,
                       shrinkage = 0.1,
                       n.minobsinnode = 10)

nrow(gbmGrid)

set.seed(1986)
gbmFit3 <- train(training.predictors,
                 training.depend.class,
                 method = "gbm",
                 trControl = fitControl.2,
                 verbose = FALSE,
                 tuneGrid = gbmGrid,
                 metric = "ROC")
gbmFit3

gbmFit3$results
gbmFit3$bestTune
gbmFit3$finalModel

#Best model within 2 pct of best - Gives the less complex model with a max 2% drop in accuracy 
whichTwoPct <- tolerance(gbmFit3$results, metric = "ROC",
                         tol = 2, maximize = TRUE)
cat("best model within 2 pct of best:\n")

gbmFit3$results[whichTwoPct,1:6]

#Model Accuracy Test
trellis.par.set(caretTheme())
plot(gbmFit3, metric = "ROC")

trellis.par.set(caretTheme())
plot(gbmFit3, metric = "ROC", plotType = "level",
     scales = list(x = list(rot = 90)))

gbm.predict <- predict(gbmFit3, newdata = testing[,1:378])
gbm.matrix <- confusionMatrix(gbm.predict, testing[,380])
gbm.matrix
# Support vector Machine Model
set.seed(1986)

svmFit <- train(training.numeric[,1:354],
                training$QualityofHealth,
                method = "svmRadial",
                trControl = fitControl.2,
                preProc = c("center", "scale"),
                tuneLength = 8,
                metric = "ROC")
svmFit

trellis.par.set(caretTheme())
plot(svmFit, metric = "ROC")

svmFit$results
svmFit$bestTune
svmFit$finalModel

svm.predict <- predict(svmFit, newdata = testing[,1:378])

# Regularised Discremenant Analysis Model

# Model 1

nnetFit <- train(training[,1:378],
                training$QualityofHealth,
                method = "nnet",
                trControl = fitControl.2,
                metric = "ROC")
nnetFit
plot(nnetFit)


nnet.predict <- predict(nnetFit, newdata = testing[,1:378])
nnet.matrix <- confusionMatrix(nnet.predict, testing[,380])

--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#Model Comparisons
--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
  

# Ranking Predictors for random forest model
important.features.gbm3 <- varImp(gbmFit3, scale = TRUE)
plot(important.features.gbm3, main= "GBM", top = 20)
print(important.features.gbm3)

  
important.features.svm <- varImp(svmFit, scale = TRUE)
plot(important.features.svm, main = "SVM", top = 20)
print(important.features.svm)


important.features.nb <- varImp(nbFit, scale = TRUE)
plot(important.features.nb, main = "NB", top = 20)
print(important.features.nb)

#Compare Models
resamps <- resamples(list(GBM = gbmFit3,
                          SVM = svmFit, 
                          NNET = nnetFit))
resamps
summary(resamps)

trellis.par.set(caretTheme())
dotplot(resamps, metric = "ROC")

#Visualise the Comparisions

trellis.par.set(caretTheme())
bwplot(resamps, layout = c(3, 1))

trellis.par.set(caretTheme())
xyplot(resamps, what = "BlandAltman")

splom(resamps)

#Diffrence between Models

difValues <- diff(resamps)
difValues
summary(difValues)

trellis.par.set(caretTheme())
bwplot(difValues, layout = c(3, 1))

trellis.par.set(caretTheme())
dotplot(difValues, metric = "Kappa")


--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# THE END! 
--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
  
