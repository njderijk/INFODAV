#0. Load libaries
library(MASS)
library(ISLR)
library(tidyverse)
library(pROC)
library(rpart)
library(rpart.plot)
library(e1071) 
library(caTools) 
library(class)
library(SmartEDA)
library(caret)
library(janitor)
library(mlbench)
library(tree)
library(glmnet)
library(mlr)
library(parallelMap)
library(parallel)
library(knitr)
library(randomForest)


#1. Dataset + short description

# This dataset pertains to qualities and characteristics of red wines. There are 11 predictors (which are
# physiochemical inputs) and 1 output variable (a quality rating from 3-8).
# There is no data about quality features such as grape types, branding, selling price and so on due to "privacy 
# and logisitic issues".

fh = read_csv("data/framingham.csv")

fh <- fh %>%
  clean_names() %>%
  na.omit()

fh$ten_year_chd <- as.factor(fh$ten_year_chd)

#2. Perform EDA
View(fh)
head(fh)
tail(fh)
ExpData(fh, type=1) # Check overview of data - No NAs, all numeric variables
ExpData(fh, type=2) # Check structure of data
str(fh)
table(fh$ten_year_chd) # Check balance of response variable (Note: very unbalanced)
SmartEDA::ExpReport(fh, op_file = "eda_fh.html")

# Pre-processing and set creation
idx <- sample(seq(1, 2), size = nrow(fh), replace = TRUE, prob = c(.7, .3))
train <- fh[idx == 1,]
test <- fh[idx == 2,]

# Fit a random forest model
# For this assignment I used a classification algorithm known as random forest. This is a type of classifcation tree
# which uses an ensemble learning method. This method operates by constructing multiple decision trees and it outputs the
# averwage prediction of the trees.

set.seed(124)

# Create a task
traintask <- makeClassifTask(data = train, target = "ten_year_chd") 
testtask <- makeClassifTask(data = test, target = "ten_year_chd")

# Detect cores
parallelStartSocket(cpus = detectCores())

# Set validation strategy
rdesc <- makeResampleDesc("CV",iters=5L)

# Make randomForest learner
rf.lrn <- makeLearner("classif.randomForest")
rf.lrn$par.vals <- list(ntree = 25L, importance=TRUE)
r_rf <- resample(learner = rf.lrn, task = traintask, resampling = rdesc, measures = list(tpr,fpr,fnr,fpr,acc), show.info = T)

# Hyperparameter tuning
getParamSet(rf.lrn)

# Set parameter space
params <- makeParamSet(makeIntegerParam("mtry",lower = 1,upper = 10),makeIntegerParam("nodesize", lower = 1,upper = 50), makeIntegerParam("ntree", lower = 1,upper = 20))

# Set validation strategy
rdesc <- makeResampleDesc("CV",iters=10L)

# Set optimization technique
ctrl <- makeTuneControlRandom(maxit = 10L)

# Start tuning
tune <- tuneParams(learner = rf.lrn, task = traintask, resampling = rdesc, measures = list(acc), par.set = params, control = ctrl, show.info = T)

print(c("Optimal parameters:", tune$x))
print(c("Accuracy: ", tune$y))

RF_model_tuned = randomForest(ten_year_chd ~ ., data=train, type="class", mtry=2, nodesize=7, ntree=17)
tune_pred = predict(RF_model_tuned, newdata=test[-16], type="class")

confusionMatrix(data=tune_pred, reference=test$ten_year_chd)

