# Installing Packages 
install.packages("e1071") 
install.packages("caTools") 

library(MASS)
library(class)
library(ISLR)
library(tidyverse)
library(ggplot2)
library(e1071) 
library(caTools) 
library(class)

#1.
p1 <- ggplot(data=Default, aes(x=balance, y=income, colour = default)) +
  geom_point()
p1

# An interesting pattern I can see is that defaults with "no" tend to have a balance under 1500 and also primarily 0.


#2. 

p2 <- ggplot(data=Default, aes(x=balance, y=income, colour = default)) +
  geom_point() +
  facet_grid(cols = vars(student))
p2

# I see two plots for both students and non-students.

#3.
def <- Default %>%
  mutate(student = as.numeric(ifelse(student=="No", 0, 1)))
def

idx <- sample(seq(1, 2), size = nrow(def), replace = TRUE, prob = c(.8, .2))
default_train <- def[idx == 1,]
default_test <- def[idx == 2,]


#4. 

target <- default_train[,1]
test <- default_test[,-1]

knn_5_pred <- knn(default_train[,-1], default_test[,-1], cl=target, k=5)
knn_5_pred

#5. 

p3 <- ggplot(data=default_test, aes(x=balance, y=income, colour = default_test$default)) +
  geom_point()
p3

default_test$pred <- knn_5_pred
p4 <- ggplot(data=default_test, aes(x=balance, y=income, colour = pred)) +
  geom_point()

p3
p4

# I don't see anything of value right away. I find it hard to compare the two scatter plots. Especially because the predicted variable
# is unbalanced.


#6. 
idx <- sample(seq(1, 2), size = nrow(def), replace = TRUE, prob = c(.8, .2))
default_train <- def[idx == 1,]
default_test <- def[idx == 2,]
target <- default_train[,1]
test <- default_test[,-1]
knn_2_pred <- knn(default_train[,-1], default_test[,-1], cl=target, k=2)
knn_2_pred

p5 <- ggplot(data=default_test, aes(x=balance, y=income, colour = default_test$default)) +
  geom_point()

default_test$pred <- knn_2_pred
p6 <- ggplot(data=default_test, aes(x=balance, y=income, colour = pred)) +
  geom_point()

p5
p6

# The KNN model with k = 2, seems to classify more incidents with "Yes" when they are located on the extreme ends of balance and income.
# However, I still find it incredibly hard to interpret two scatter plots.


#7. 
table(true = default_test$default, predicted = knn_2_pred)

# The table would have no incidents in the fields that are contradictory combinations ("No" and "Yes" and vice versa.)
# This would mean true yes's have been identified as yes and vice versa for no. It would look like:

# predicted
# true    No  Yes
# No  1900   0
# Yes   0   100

#8. 

table(true = default_test$default, predicted = knn_5_pred)
#     predicted
# true    No  Yes
# No  1969   10
# Yes   54   10

table(true = default_test$default, predicted = knn_2_pred)
#     predicted
# true    No  Yes
# No  1863   44
# Yes   34   20

# I can conclude that the model with k=2 has made more errors in prediction that the model with k = 5. Furtermore, the model with
# k = 5 is especially bad in correctly predicting true positives.


#9. 

lr_mod <- glm(default ~ balance + income, default_train, family="binomial")
lr_mod

#10. 
lr_pred <- predict.lm(lr_mod, default_test)
lr_pred

predicted_classes <- ifelse(lr_pred > 0.5, "1", "0")

default_test$pred <- as.numeric(predicted_classes)

default_test <- default_test %>%
  mutate(default = as.numeric(ifelse(default=="No", 0, 1))) %>%
  mutate("prediction", ifelse(default == pred, "correct", "wrong"))

p7 <- ggplot(default_test, aes(x=balance, y=income, color=default_test[,7])) +
  geom_point() 
p7

#11. 
coef(lr_mod)

# The coefficient for balance can be interpreted as having a high correlation with default. There is negative linear correlation with
# default. Meaning, that if balance goes up, the probability for default goes down.



#12. 

balance_df <- def %>%
  filter(balance <= 3000, balance >= 0, student == 0) %>%
  select(-default)

# I did not know what was meant by the mean income of the default_train dataset.

sel <- sample(1:nrow(balance_df), 500)

balance_df <- balance_df[sel,]

#13. 
balance_df$balance <- as.numeric(balance_df$balance)
lr_mod_2 <- lm(balance ~ income + student, balance_df)
lr_mod_2
lr_pred_2 <- predict.lm(lr_mod, default_test)


lr_pred_2 <- predict.lm(lr_mod, balance_df)
lr_pred_2

p8 <- ggplot(balance_df, aes(x=balance, y=lr_pred_2)) +
  geom_point()
p8

# Yes, since it can be clearly seen that although there is some variance, the central tendency is that the predictions are not
# too far off and pretty consistent.


#14. 
predicted_values <- ifelse(lr_pred_2 > 0.5, "1", "0")
predicted_values

balance_df$pred <- as.numeric(predicted_values)

table(true = balance_df$balance, predicted = predicted_values)

# I was unclear how this should be performed. How can we address a confusion matrix with the values we have just defined?
# If a prediction is > 0.5, the predicted model has a 'confidence' that it has the right value. But how do we know if this is
# a true positive or false positive?


# 15.
lda_mod <- lda(default ~ balance + income, default_train)

# 16.
lda_mod

# People who default on their loans are in few number, but tend to have a low balance and more importantly, low income. Ergo,
# it can be said that for the training set, income seems to be a 'more important' predictor than balance.
lda_pred <- predict(lda_mod, default_test)
lda_pred


#17. 
predicted_classes_lda <- ifelse(lda_pred[["x"]] > 0.5, "1", "0")

table(true = default_test$default, predicted = predicted_classes_lda)

#     predicted
# true    0    1
# 0     1356   551
# 1       1    53

# This model seems to have a lot of false negatives (about 25% of all predictions). Therefore, it can be said that this model is
# inferior for predicting the class of the Default data set with the used formula. Furthermore, although the "Yes" or 1 of the
# default variable was in such few numbers, it is remarkable that there are that many false positives.


#18.

ttc <- read_csv('data/Titanic.csv')
dim(ttc)
str(ttc)

ttc <- ttc %>%
  mutate(PClass = as.numeric(ifelse(PClass=="1st", 1, ifelse(PClass=="2nd", 2, ifelse(PClass=="3rd", 3, 0))))) %>%
  mutate(Sex = as.numeric(ifelse(Sex=="male", 1, 0))) %>%
  mutate(Survived = as.factor(ttc$Survived)) %>%
  select(-Name) %>%
  na.omit()

idx <- sample(seq(1, 2), size = nrow(ttc), replace = TRUE, prob = c(.8, .2))
ttc_train <- ttc[idx == 1,]
ttc_test <- ttc[idx == 2,]

glm_ttc <- glm(Survived ~ ., ttc_train, family="binomial")

ttc_pred <- predict.glm(glm_ttc, ttc_test, type="response")
ttc_pred

predicted_classes_ttc <- ifelse(ttc_pred > 0.5, "1", "0")

table(true = ttc_test$Survived, predicted = predicted_classes_ttc)

#       predicted
# true  0  1
# 0     80 16
# 1     24 39

ttc_train <- rbind(c(3, 14, 1, NA), ttc_train)
ttc_train<- rbind(c(2, NA, 0, NA), ttc_train)

ttc_pred_2 <- predict.glm(glm_ttc, ttc_train[1:2,])

predicted_classes_ttc_2 <- ifelse(ttc_pred_2 > 0.5, "1", "0")
predicted_classes_ttc_2

# Based on my model, it is impossible to predict if the boy would have lived. However, my model does state that the girl would
# not have survived.