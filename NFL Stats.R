# Install Devtools library
install.packages('devtools')
library('devtools')

#Install NFL Scraping package
devtools::install_github(repo = "maksimhorowitz/nflscrapR", force=TRUE)

#Load required packages
library(nflscrapR)
library(ggplot2)
library(ggjoy)
library(dplyr)
library(pheatmap)
library(Hmisc)
library(mice)
library(caret)
library(VIM)

# Collect Play by Play data for NFL Season 2017

pbp_2017 <- season_play_by_play(2017)

min_rush_count = 200

rush_coutn <- pbp_2017 %>% filter(PlayType == 'Run') %>% 
  group_by(Rusher) %>%
  summarise(rush_coutn = n(), 
             total_yards = sum(Yards.Gained),
             mean_yards = round(mean(Yards.Gained), 2)) %>%
              filter(rush_coutn >= min_rush_count) %>%
              arrange(desc(rush_coutn))
              
rushing_stats <- pbp_2017 %>% 
  filter(PlayType == "Run" & Rusher %in% rush_coutn$Rusher & Yards.Gained <= 50) %>%
  filter(down != 4 & !is.na(down)) %>%
  filter(!is.na(RunLocation))

# Compare distribution of rushes for eligible players

ggplot(rushing_stats, aes(x = Yards.Gained, y = Rusher, fill=Rusher)) +
  geom_joy(scale = 3) +
  theme_joy() +
  scale_fill_manual(values=rep(c('gray', 'lightblue'), length(rushing_stats$Rusher)/2)) +
  scale_y_discrete(expand = c(0.01, 0)) +
  scale_x_continuous(expand = c(0, 0)) +
  theme(legend.position="none") +
  labs(x="Yards gained per play" ,y="")
  
# Pre-Processing
  pbp_2017_numeric = pbp_2017[sapply(pbp_2017,is.numeric)]
  pbp_2017_categorical = pbp_2017[sapply(pbp_2017, is.factor)]
# Impute missing values
imputed_Data <- mice(pbp_2017_numeric, m=5, maxit = 50, method = 'pmm', seed = 500)
  
#Create data partion
training_class <- createDataPartition(pbp_2017_numeric$Yards.Gained, times = 2, p=0.7)
training <- pbp_2017_numeric[training, ]
testing <- pbp_2017_numeric[-pbp_2017_numeric_train, ]

--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
  # Model Building
--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
  
# Set Parameter Tuning Metrics
  
fitControl <- trainControl(method = "repeatedcv",
                               number = 10,
                               repeats = 10)

# Linear Regression

set.seed(1986)

lmFit <- train(Yards.Gained ~ .,
               data = training, 
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
