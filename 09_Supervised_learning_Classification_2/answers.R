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

#1. 
cardio <- read_csv("data/cardiovascular_treatment.csv")

idx <- sample(seq(1, 2), size = nrow(cardio), replace = TRUE, prob = c(.8, .2))
train <- cardio[idx == 1,]
test <- cardio[idx == 2,]


lr_mod <- lm(response ~ ., data = as.data.frame(train), family="binomial")

pred_lr <- predict(lr_mod, test[-7])
pred_classes <- ifelse(pred_lr > 0.5, "1", "0")

cm <- table(true = test$response, predicted = pred_classes)
cm
#2. 
#Accuracy
acc <- (cm[1] + cm[4]) / (cm[1] + cm[4] + cm[2] + cm[3])
acc

#Sensitivity
sens <- cm[1] / (cm[1] + cm[3])
sens

#FP Rate
fpr <- cm[3] / (cm[3] + cm[4])
fpr

#PPV
ppv <- cm[1] / (cm[1] + cm[2])
ppv

#NPV
npv <- cm[4] / (cm[4] + cm[2])
npv

# The performance of this model is not very good. Accuracy is one of the foremost metrics which (primarily) decides how well a model
# performs. However, with this low amount of data available, it becomes clear why this model performs badly. It is just about better
# than a coin flip. Furthermore, the amount of false positives and false negatives are relatively high, which makes the model 
# unusable in a medical environment.

#3. 
lda_mod <- lda(response ~ ., data = cardio)
lda_pred <- predict(lda_mod, test[-7])

pred_classes_lda <- ifelse(lda_pred[["x"]][,1] > 0, "1", "0")

cm_lda <- table(true = test$response, predicted = pred_classes_lda)
cm_lda

#Accuracy
acc_lda <- (cm_lda[1] + cm_lda[4]) / (cm_lda[1] + cm_lda[4] + cm_lda[2] + cm_lda[3])
acc_lda

#Sensitivity
sens_lda <- cm_lda[1] / (cm_lda[1] + cm_lda[3])
sens_lda

#FP Rate
fpr_lda <- cm_lda[3] / (cm_lda[3] + cm_lda[4])
fpr_lda

#PPV
ppv_lda <- cm_lda[1] / (cm_lda[1] + cm_lda[2])
ppv_lda

#NPV
npv_lda <- cm_lda[4] / (cm_lda[4] + cm_lda[2])
npv_lda

# Values that have improved significantly: Accuracy, PPV, NPV
# Values that have worsened: Sensitivity FPR

# From a medical perspective, the model has increased in usability due to a higher accuracy, PPV, and NPV. However, a notably lower
# sensitivity and marginally lower FPR indicate that if the model were to be used, it would have to be in specific cases such as
# illnesses that are not life threatening. Having many false positives might rack up immense costs for the hospitals and patients.
# The bottom line: the model has improved but is still not usable for medical use.


#4. 
new_patients <- read_csv("data/new_patients.csv")

pred_lr_2 <- predict(lr_mod, new_patients)
pred_lda_2 <- predict(lda_mod, new_patients)

pred_classes_lr <- ifelse(pred_lr_2 > 0.5, "1", "0")
cm_2 <- table(true = new_patients$response, predicted = pred_classes_lr)

pred_classes_lda <- ifelse(pred_lda_2[["x"]][,1]> 0, "1", "0")
cm_3 <- table(true = new_patients$response, predicted = pred_classes_lda)

cm_2
cm_3

# With this new data, the LR model has a lot more false positives while the LDA model has more false negatives and it outperforms
# the LR model slightly in accuracy. Not a lot can be said since the models were trained on a small amount of data and will
# therefore not perform consistently.

#5. 

lr1_mod <- lm(response ~ severity + age + bb_score, data = cardio)
lr2_mod <- lm(response ~ age + I(age^2) + gender + bb_score * prior_cvd * dose, data = cardio)

lr1_pred <- predict(lr1_mod, train)
lr2_pred <- predict(lr2_mod, train)

lr1_classes <- ifelse(lr1_pred > 0.5, "1", "0")
lr2_classes <- ifelse(lr2_pred > 0.5, "1", "0")

#6. 
roc_lr1 <- roc(lr1_classes, lr1_pred)
roc_lr2 <- roc(lr2_classes, lr2_pred)

ggroc(roc_lr1)
ggroc(roc_lr2)

# I can't figure out what I have done wrong since both plots are identical depsite the formulas being different. Tried the predictions
# for both the training and testing sets.

#7.
roc_lr1
roc_lr2

# Both AUC values are the same. This is reflected accurately in the plots. The minimum AUC value for these objects is 1, and the 
# perfect value would be 0. You would get an identical plot to what I display in ggroc(roc_lr1)

#8. 

# fit lda model, i.e. calculate model parameters
lda_iris <- lda(Species ~ ., data = iris)

# use those parameters to compute the first linear discriminant
first_ld <- -c(as.matrix(iris[, -5]) %*% lda_iris$scaling[,1])

# plot
tibble(
  ld = first_ld,
  Species = iris$Species
) %>% 
  ggplot(aes(x = ld, fill = Species)) +
  geom_histogram(binwidth = .5, position = "identity", alpha = .9) +
  scale_fill_viridis_d(guide = ) +
  theme_minimal() +
  labs(
    x = "Discriminant function",
    y = "Frequency", 
    main = "Fisher's linear discriminant function on Iris species"
  ) + 
  theme(legend.position = "top")

View(iris)
head(iris)
tail(iris)
ExpData(iris, type=1) # Check overview of data
ExpData(iris, type=2) # Check structure of data
SmartEDA::ExpReport(iris, op_file = "eda.html")

#9. 
lda_iris_sepal <- lda(Species ~ Sepal.Length + Sepal.Width, data = iris)

#10. 

lda_pred1 <- predict(lda_iris, iris)
lda_pred2 <- predict(lda_iris_sepal, iris)

conf_lda1 <- confusionMatrix(data = lda_pred1$class, reference = iris$Species)
conf_lda2 <- confusionMatrix(data = lda_pred2$class, reference = iris$Species)
conf_lda1[["overall"]][["Accuracy"]] #0.98
conf_lda2[["overall"]][["Accuracy"]] #0.8

# The first model performs better in terms of accuracy.

#11. 
iris_tree_mod <- rpart(Species ~ ., data=iris)
rpart.plot(iris_tree_mod)

#12. 
# Answer: versicolor.

#13.
# Petal>length < 2.5
# Petal.Width < 1.8

p1 <- ggplot(iris, aes(x=Petal.Length, y=Petal.Width)) +
  geom_point() +
  geom_segment(aes(x = 2.5, y = 0, xend = 2.5, yend = 2.5)) +
  geom_segment(aes(x = 0, y = 1.799, xend = 7, yend = 1.799))
p1

# Essentially, three classes are created. Those who fit in the boxes as per the lines' indication. Although some dots appear to be
# right on the line itself, they will be placed in one of the boxes since the dividing factor is Petal.Width < 1.8 and the plot
# is too small to see a difference of 0.001.

#14. 

rpart_control <- rpart.control(minsplit = 2)
iris_tree_full_mod <- rpart (Species ~ ., data=iris, control=rpart_control)
rpart.plot(iris_tree_full_mod)

# I expect this model to perform better than the previous model due to the fact that the complexity has increased slightly (the 
# previous model was very simple). The depth of the tree has not been increased that much, which allows for more specific classifications.


#15. 
rf <- randomForest::randomForest(Species ~ ., nodesize=2, importance=TRUE, data=iris)
rf
importance(rf)

p2 <- ggplot(as.data.frame(importance(rf))) +
  geom_bar(aes(x=MeanDecreaseAccuracy))
p2

# I had no expectations regarding variable importance, so I was astounded that there existed such a discrepancy in the importance
# of these variables.

# With this information, we acquire a new randomforest model.

rf_new <- randomForest(Species ~ Sepal.Length + Sepal.Width, nodesize=2, importance=TRUE, data=iris)
rf_new

rf_pred <- predict(rf_new, iris)

cm_rf <- confusionMatrix(data=rf_pred, reference=iris$Species)
cm_lda <- confusionMatrix(lda_pred1$class, reference=iris$Species)
cm_rf
cm_lda

# The randomForest model performs decent, but not as good enough as the lda model. The lda model outclasses the randomForest in
# every aspect (as can be seen in the summaries from cm_rf and cm_lda)