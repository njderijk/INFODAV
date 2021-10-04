library(ISLR)
library(MASS)
library(tidyverse)
library(ggplot2)
library(caret)
library(datarium)

#1. 
lm_ses <- lm(medv ~ lstat, Boston)

#2. 
coef(lm_ses)

#3. 
summary(lm_ses)
# In the summary I can see the residuals, coefficients, square root of estimated variance of the random error, and degrees
# of freedom

#4. 
y_pred <- predict.lm(lm_ses, Boston)

#5. 
p1 <- ggplot(Boston, aes(x=y_pred, y=medv)) +
  geom_point() +
  stat_smooth(method = "lm",
              col = "#ff0000",
              se = FALSE,
              size = 1)
p1

# I can see that the central tendency is upwards with most cases between the values of 20 and 30 on the x-axis and between 5 and
# 25 on the y-axis. If the plot would be a perfect fit, all cases would be on the plotted line.

#6. 
lstat <- seq(from = 0, to = 40, length.out = 1000)

pred_dat <- as.data.frame(lstat, col.names = c("lstat"))
View(pred_dat)

#7. 
y_pred_new <- predict.lm(lm_ses, pred_dat)

#8.
p_scatter <- ggplot(Boston, aes(x=sample(pred_dat$lstat, size = 506, replace = FALSE), y=medv)) +
  geom_point()

p_scatter

#9. 
pred_dat$medv <- y_pred_new

#10. 
p_scatter_l <- ggplot(Boston, aes(x=sample(pred_dat, size = 506, replace = FALSE), y=medv)) +
  geom_point() +
  geom_line(data = pred_dat)

p_scatter_l

# The line represent the data of pred_dat for each pair of values (lstat and medv in pred_dat)

#11. 
y_pred_95 <- predict(lm_ses, pred_dat, interval = "confidence")
# In this object, there are three values for each row. The fitted value and the upper and lower boundaries of this fitted value.
# These upper and lower boundaries represent the confidence interval.

#12. 
df <- as.data.frame(Boston$medv)
df$lstat <- sample(lstat, size = 506, replace = FALSE)
df$lwr <- y_pred_95[2]
df$upr <- y_pred_95[3]

#13. 
p_scatter_l_r <- ggplot(Boston, aes(x=sample(pred_dat, size = 506, replace = FALSE), y=medv)) +
  geom_point() +
  geom_line(data = pred_dat)

p_scatter_l_r

#14.


#15.



#16. 
mse <- function(y_true, y_pred) {
  mse = mean((y_true - y_pred)^2)
  return(mse)
}

#17.
mse(1:10, 10:1)

#18. 
mse(Boston$medv, y_pred) # 38.48297

#19. 
splits <- rep(c("train", "validation", "test"), c(253, 152, 101))

#20. 
rnd <- sample(splits, size = 506, replace = FALSE)

boston_master <- Boston %>%
  mutate(rnd)
  
#21.

boston_train <- boston_master %>%
  filter(rnd == "train")

boston_valid <- boston_master %>%
  filter(rnd == "validation")

boston_test <- boston_master %>%
  filter(rnd == "test")

#22.
model_1 <- lm(medv ~ lstat, boston_train)

#23.
y_pred_train <- predict.lm(model_1, boston_train)

model_1_mse_train <- mse(boston_train$medv, y_pred_train)
model_1_mse_train # 41.8683

#24. 
y_pred_valid <- predict.lm(model_1, boston_valid)
model_1_mse_valid <- mse(boston_valid$medv, y_pred_valid)
model_1_mse_valid # 36.737

#25. 
model_2 <- lm(medv ~ lstat + age + tax, boston_train)
y_pred_train_2 <- predict.lm(model_2, boston_train)
model_2_mse_train <- mse(boston_train$medv, y_pred_train_2)
model_2_mse_train # 38.77587

y_pred_valid_2 <- predict.lm(model_2, boston_valid)
model_2_mse_valid <- mse(boston_valid$medv, y_pred_valid_2)
model_2_mse_valid # 37.99069

#26. 
# I would choose for the second model as its average RMSE (and variance) between train and validation is lower than that of the 
# first model.
# This could indicate that it is a more robust model when exposed to new data. This could be the case at it considers more
# variables.. Furthermore, I did not see a high degree of correlation when inspecting the first model with summary(). 
# This would indicate that more variables are needed to properly predict the value of medv (this assessment coincides with
# the previous statement of the robustness of the model)

#27.
y_pred_test <- predict.lm(model_2, boston_test)
model_2_mse_test <- mse(boston_test$medv, y_pred_test)
model_2_mse_test # 35.70362

# This number tells me that the MSE is lower than that of both the model's MSE's on the train and validation sets. In either case,
# when considering these numbers, this seems to be a good result.


#28. 

k_cv <- function(formula, dataset, k, target) {
  mse_avg_folds = 0
  for(i in 1:k) {
    print(paste("i=", i))
    # Dividing into train and test sets
    colnr <- which( colnames(dataset)==target) # Identify target variable and select its column number
    sample_size = floor(0.8*nrow(dataset)) # Set sample size
    set.seed(sample_size * sample(1:10,1) / sample(1:10,1)) # Set random seed every iteration
    
    # Randomly split data in r
    divide = sample(seq_len(nrow(dataset)) ,size = sample_size) # Sample the dataset in the right proportion
    train = dataset[divide,]
    test = dataset[-divide,]
    
    # Create the model and train it
    model <- lm(formula, train)
    y_pred_train <- predict.lm(model, train)
    print(paste("y_pred_train", y_pred_train))
    
    # Calculate MSE
    mse_train <- mse(train[,colnr], y_pred_train)
    mse_avg_folds  = mse_avg_folds + mse_train
  }
  
  return(mse_avg_folds / k) # Return MSE averaged over folds 
}

# 29. 

# Model 1
formula_model_1 <- train(medv ~ lstat + age + tax, data = Boston, method="lm")
model_1_mse_29 <- k_cv(formula_model_1, Boston, 9, "medv")
model_1_mse_29 # 36.13827

# Model 2
formula_model_2 <- train(medv ~ lstat + I(lstat^2) + age + tax, data = Boston, method="lm")
model_2_mse_29 <- k_cv(formula_model_2, Boston, 9, "medv")
model_2_mse_29 # 26.24429



