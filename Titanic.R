
train <- read.csv(file.path('/Users/ragyibrahim/documents/r-code/kaggle/titanic/train.csv'))
test <- read.csv(file.path('/Users/ragyibrahim/documents/r-code/kaggle/titanic/test (1).csv'))
View(train)

summary(train$Sex)
prop.table(table(train$Sex))
prop.table(table(train$Sex, train$Survived), 1)

summary(train$Age) # were seeing 177 NA's

# Create a new dimension 'Child' to indicate whether a passanger was under the age of 18

train$Child <- 0
train$Child[train$Age < 18] <- 1

test$Child <- 0
test$Child[test$Age < 18] <- 1

# Create a table that shows us the proportion of survivors that were Children

children.table.sum <- aggregate(Survived ~ Child + Sex, 
                                data = train, FUN =  sum)
children.table.sum

children.table.length <- aggregate(Survived ~ Child + Sex, 
                                   data = train, FUN =  length)
children.table.length

children.table <- aggregate(Survived ~ Child + Sex, 
                            data = train, FUN =  function(x) {sum(x)/length(x)})
children.table

train$Fare2 <- '+30'
train$Fare2[train$Fare < 30 & train$Fare >= 20 ] <- '20-30'
train$Fare2[train$Fare < 20 & train$Fare >= 10 ] <- '10-20'
train$Fare2[train$Fare < 10] <- '-10'

test$Fare2 <- '+30'
test$Fare2[test$Fare < 30 & test$Fare >= 20 ] <- '20-30'
test$Fare2[test$Fare < 20 & test$Fare >= 10 ] <- '10-20'
test$Fare2[test$Fare < 10] <- '-10'

fair.table <- aggregate(Survived ~ Fare2 + Pclass + Sex, 
                        data = train, FUN = function(x){sum(x)/length(x)})
fair.table

--------------------------------------------------------------------------------------------------------------

# Decision Trees
  
library(rpart)

fit <- rpart(Survived ~  Pclass + Sex + Age + Parch + Fare + SibSp + Embarked, 
             data = train, method = 'class')
plot(fit)
text(fit)

# Fancy Decision Trees

library(rattle)
library(rpart.plot)
library(RColorBrewer)

fancyRpartPlot(fit)

predictions <- predict(fit, test, type = 'class')
table(predictions)

submit <- data.frame(PassengerId = test$PassengerId, 
                     Survived = predictions)
submit

# Crazy big decision tree

fit2 <- rpart(Survived ~ Pclass + Sex + Age + Parch + Fare + SibSp + Embarked, 
              data = train, method = 'class', control = rpart.control(minsplit = 2, cp =0)) # Cp = Complexity Parameter - Any split that does not decrease the overall lack of fit by a factor of cp is not attempted.
fancyRpartPlot(fit2)

# Interactive Decision Tree

fit3 <- rpart(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked, data=train,
             method="class", control=rpart.control(your controls))
new.fit <- prp(fit3, snip = TRUE)$obj
fancyRpartPlot(new.fit)


--------------------------------------------------------------------------------------------------------------

# Feature Engineering
  
# Names - Group by Titles
  
train$Name[1]

# We need to perform the same actions in both the test and the training datasets, so we need to combined them

test$Survived <- NA
combi <- rbind(train, test)
View(combi)
str(combi)

#Convert Names to Strings

combi$Name <- as.character(combi$Name)
str(combi)
combi$Name[1]

# There's a comma after the last name and a full-stop after the title

strsplit(combi$Name[1], split = '[,.]')

strsplit(combi$Name[1], split = '[,.]')[[1]]

strsplit(combi$Name[1], split = '[,.]')[[1]][2]

# We use the function sapply to run through every row and apply the funciton strsplit to every cell - 
# Then we store the the resultling vector into a new column called 'Title' 

combi$Title <- sapply(combi$Name, FUN=function(x) {strsplit(x, split='[,.]')[[1]][2]})
head(combi$Title)

# Use the sub function to remove the unwanted space before the title

combi$Title <- sub(' ', '', combi$Title)
head(combi$Title)

table(combi$Title)

# Combine rare titles 

'Mlle' -> combi$Title[combi$Title %in% c('Mme', 'Mlle')]
table(combi$Title)

'Sir' -> combi$Title[combi$Title %in% c('Capt', 'Don', 'Major', 'Sir')]
'Lady' -> combi$Title[combi$Title %in% c('Dona', 'Lady', 'the Countess', 'Jonkheer')]

table(combi$Title)

# CHange back to factor 

combi$Title <- factor(combi$Title)
combi$Title



# Family Size

combi$FamilySize <- combi$SibSp + combi$Parch + 1 
head(combi$FamilySize)
#Combining the Surname with the family size

combi$Surname <- sapply(combi$Name, FUN = function(x) {strsplit(x, split = '[,.]')[[1]][1]})
head(combi$Surname)

# Sapply is a string operater and therefore requries a string to work

combi$FamilyID <- paste(as.character(combi$FamilySize), combi$Surname, sep = '')
head(table(combi$FamilyID))

# Appending small families

'Small' <-  combi$FamilyID[combi$FamilySize <= 2]
head(table(combi$FamilyID))

famIDs <- data.frame(table(combi$FamilyID))
View(famIDs)

famIDs <- famIDs[famIDs$Freq <= 2, ]
View(famIDs)

'Small' -> combi$FamilyID[combi$FamilyID %in% famIDs$Var1]
head(Small)

combi$FamilyID <- factor(combi$FamilyID)
combi$FamilyID

# Because we built the factors on a single dataframe, and then split it apart after we built them, 
# R will give all factor levels to both new dataframes, even if the factor doesn’t exist in one. 
# It will still have the factor level, but no actual observations of it in the set. 

train <- combi[1:891, ]
test <- combi[892:1309, ]



--------------------------------------------------------------------------------------------------------------

# Fit a model to our new engineered features
  
fit4 <- rpart (Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked + Title + FamilySize + FamilyID, 
               data = train, method = 'class')
fancyRpartPlot(fit4)


--------------------------------------------------------------------------------------------------------------
  
# Random Forest Model 
  
  # Given that random forest algorithm does not deal well with NAs, 
  # we can use decision trees to replace the missing value in the Age dimension
  
Age.fit <- rpart(Age ~ Pclass + Sex + SibSp + Parch + Fare + Embarked + Title + FamilySize,
                   data = combi[!is.na(combi$Age), ], method = 'anova')

combi$Age[is.na(combi$Age)] <- predict(Age.fit, combi[is.na(combi$Age), ])

summary(combi)

which(combi$Embarked == "")

combi$Embarked[c(62, 830)] = 'S'
combi$Embarked <- factor(combi$Embarked)

which(is.na(combi$Fare))

combi$Fare[1044] <- median(combi$Fare, na.rm = TRUE)
  
summary(combi)  

# Random Forest can only digest categorical variables with no more than 32 levels


combi$FamilyID2 <- combi$FamilyID
combi$FamilyID2 <- as.character(combi$FamilyID2)
'Small' -> combi$FamilyID2[combi$FamilySize <= 3]
combi$FamilyID2 <- factor(combi$FamilyID2)

str(combi$FamilyID2)

# Split dataset into training and testing 

train <- combi[1:891, ]
test <- combi[892:1309, ]
  

--------------------------------------------------------------------------------------------------------------

library(randomForest)

set.seed(415)

random.fit <- randomForest(as.factor(Survived) ~ Pclass + Sex + Age + Fare + Title + FamilySize +FamilyID2, 
                           data = train, importance = TRUE, ntree = 5000)

varImpPlot(random.fit, col = 'blue')

plot.randomForest(random.fit)
summary(random.fit)


--------------------------------------------------------------------------------------------------------------
  
  # Write a submission
  
  Prediction.random <- predict(random.fit, test)
submit <- data.frame(PassengerId = test$PassengerId, Survived = Prediction.random)
write.csv(submit, file = "randomforest.csv", row.names = FALSE)

--------------------------------------------------------------------------------------------------------------

install.packages('party')
library(party)

# Conditional Infrence Trees

fit.cforest <- cforest(as.factor(Survived) ~ Pclass + Sex + Age + Fare + Title + FamilySize +FamilyID2, 
                       data = train, controls = cforest_unbiased(ntree = 2000, mtry = 3))


--------------------------------------------------------------------------------------------------------------

# Write a submission
  
Prediction.cforest <- predict(fit.cforest, test, OOB = TRUE, type = 'response')
submit <- data.frame(PassengerId = test$PassengerId, Survived = Prediction.cforest)
write.csv(submit, file = "cforest.csv", row.names = FALSE)

--------------------------------------------------------------------------------------------------------------
  
combi$Survived <- as.character(combi$Survived)
train <- combi[1:891, ]
test <- combi[892:1309, ]
  
    
# Using the Caret Package to build multiple models
  
library(caret)

cntrl <- trainControl(method = "repeatedcv", 
                      repeats = 3,
                      classProbs = TRUE, 
                      summaryFunction = twoClassSummary)

fit.caret.1 <- train(Survived ~., 
             data = train, 
             method = "svmPoly",
             tuneLength = 15,
             trControl = cntrl)

fit.caret.1
attributes(fit.caret.1)

fit.caret.1$finalModel
fit.caret.1$results
fit.caret.1$modelInfo

plot(fit.caret.1) # shows the relationship between the number of PLS components and the resampled estimate of the area under the ROC curve.
