library (caret)
library(mlbench)
data(Sonar)

View(Sonar)

summary(Sonar)


#Modelling

set.seed(1996)

inTrain <- createDataPartition(y = Sonar$Class, 
                               p = 0.75, 
                               list = FALSE)
#Bt default, createDataPartition does a stratifed random split of the data (bootstap resampling)

str(inTrain)
nrow(inTrain)

# Now to split the data into training and testing
training <- Sonar[inTrain, ]
testing <- Sonar [-inTrain, ]

nrow(testing)
nrow(training)

#Applying a Partial Least Squared Model (PLS)

fit <- train(Class ~ .,
             data = training, 
             method = "pls",
             preProess = c("centre", "scale"))

fit
plot(fit)
fit$bestTune
fit$finalModel
fit$control

fit.1 <- train(Class ~.,
             data = training, 
             method = "pls", 
             tuneLength = 15, 
             preProcess = c("center", "scale"))

fit.1
plot(fit.1)
fit$bestTune
fit$finalModel

"To modify the resampling method, a trainControl function is used. The option method controls
the type of resampling and defaults to "boot". 
Another method, "repeatedcv", is used to specify repeated 
K-fold cross-validation (and the argument repeats controls the number of repetitions). 
K is controlled by the number argument and defaults to 10."


cntrl <- trainControl(method = "repeatedcv", # Repeated K-fold cross-validation
                      repeats = 3) # Number of Folds

fit.2 <- train(Class ~ ., 
              data = training, 
              method = "pls", 
              tuneLength = 15, 
              trControl = cntrl,
              preProcess = c("center", "scale"))

fit.2
plot(fit.2)
fit.2$bestTune
fit.2$finalModel


"The methods for measuring performance. If unspecied, overall accuracy and the Kappa
statistic are computed. For regression models, root mean squared error and R2 are computed.
Here, the function will be altered to estimate the area under the ROC curve, the sensitivity
and specicity"

"Finally, to choose different measures of performance, additional arguments are given to trainControl.
The summaryFunction argument is used to pass in a function that takes the observed and predicted
values and estimate some measure of performance. Two such functions are already included in the
package: defaultSummary and twoClassSummary. The latter will compute measures specific to two-class
problems, such as the area under the ROC curve, the sensitivity and specicity. Since the ROC
curve is based on the predicted class probabilities (which are not computed automatically), another
option is required. The classProbs = TRUE option is used to include these calculations

Lastly, the function will pick the tuning parameters associated with the best results. Since we are
using custom performance measures, the criterion that should be optimized must also be specified.
In the call to train, we can use metric = "ROC" to do this"


cntrl.1 <- trainControl(method = "repeatedcv", 
                      repeats = 3,
                     classProbs = TRUE, 
                     summaryFunction = twoClassSummary)

fit.3 <- train(Class ~., 
             data = training, 
             method = "pls",
             tuneLength = 15,
             trControl = cntrl.1,
             preProcess = c("center", "scale"), 
             metric = "ROC")

fit.3
plot(fit.3) # shows the relationship between the number of PLS components and the resampled estimate of the area under the ROC curve.
attributes(fit.3)

fit.3$finalModel
fit.3$results

# Make new predictions based on testing data

predictions <- predict(fit.3, newdata = testing)

# Construct Confusion Matrix for results

confusion <- confusionMatrix(data = predictions, testing$Class)
confusion

________________________________________________________________________________________________________________________________________________________________


fitControl <- trainControl(method = "repeatedcv", 
                           number = 10, # 10 K-folds 
                           repeats = 10) # Repeated 10 times


# Fit a Stochastic Gradient Boosted Tree model

gbmFit <- train(Class ~., 
                data = training, 
                method = "gbm",
                trControl = fitControl, 
                verbose = FALSE)
gbmFit
plot(gbmFit)
gbmFit$bestTune
gbmFit$finalModel

"For a gradient boosting machine (GBM) model, there are three main tuning parameters:
  
  number of iterations, i.e. trees, (called n.trees in the gbm function)
  complexity of the tree, called interaction.depth
  learning rate: how quickly the algorithm adapts, called shrinkage
  the minimum number of training set samples in a node to commence splitting (n.minobsinnode)"

# Create an alternate tuning Grid

gbmGrid <- expand.grid(interaction.depth = c(1,5,9),
                       n.trees = (1:30)*50,
                       shrinkage = 0.1, 
                       n.minobsinnode = 20)
head(gbmGrid)

set.seed (1996)

gbmFit2 <- train(Class ~ .,
                 data = training, 
                 method = "gbm", 
                 trControl = fitControl, 
                 verbose = FALSE,
                 tuneGrid = gbmGrid)
gbmFit2
plot
gbmFit2$finalModel

# Plot the results

trellis.par.set(caretTheme())
plot.train(gbmFit2)
plot.train(gbmFit2, metric = "Kappa") # Plot using Kappa as the metric

# Using ROC to Optimise model

fitControl2 <- trainControl(method = "repeatedcv", 
                            number = 10,
                            repeats = 10, 
                            classProbs = TRUE, 
                            summaryFunction = twoClassSummary)

gbmFit3 <- train(Class ~ .,
                 data = training,
                 method = 'gbm',
                 trControl = fitControl2,
                 tuneGrid = gbmGrid, 
                 metric = "ROC",
                 verbose = FALSE)

gbmFit3
plot.train(gbmFit3)


# Choosing the Final Model (Using Tolerance within 2% of the best model)

TwoPerc <- tolerance(gbmFit3$results, metric = "ROC",
                     tol = 10, maximize = FALSE)

gbmFit3$results[TwoPerc, ]

# Extracting Predictions and Class Probabilities

gbmpredictions <- predict(gbmFit3, newdata = testing)
gbmpredictions

gbmpredictions.probs <- predict(gbmFit3, newdata = testing, type = "prob")
head(gbmpredictions.probs)

# Build SVM Model

set.seed(1996)

svmFit <- train(Class ~.,
                data = training,
                method = "svmRadial",
                trControl = fitControl2,
                preProcess = c("center", "scale"),
                metric = "ROC", 
                tuneLength = 8)

svmFit
plot(svmFit)
svmFit$modelInfo
svmFit$finalModel


# Build a Regulised Discriminant Analysis Model

set.seed(1996)
rdaFit <- train(Class ~ ., 
                data = training,
                method = "rda",
                tuneLength = 4, 
                metric ="ROC",
                trControl = fitControl2)


