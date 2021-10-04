library(ISLR)
library(glmnet)
library(tidyverse)
library(ggplot2)
library(caret)
library(datarium)
library(gtools)
library(arrangements)


#1. 
dim(Hitters) # 322 20, so 322 baseball players before omitting NA values

baseball <- Hitters %>%
  filter(!is.na(Salary))

dim(baseball) # 263 20, so 263 baseball players before omitting NA values

#2. 
idx <- sample(seq(1, 3), size = nrow(iris), replace = TRUE, prob = c(.5, .3, .2))
bb_train <- baseball[idx == 1,]
bb_val <- baseball[idx == 2,]
bb_test <- baseball[idx == 3,]

#3. 

mse <- function(y_true, y_pred) {
  mse = mean((y_true - y_pred)^2)
  return(mse)
}

lm_mse <- function(formula, train_data, test_data) {
  y_name <- formula[["terms"]][[2]]
  print(y_name)
  y_true <- test_data[[y_name]]
  # print(paste("y_true", y_true))
  
  y_pred <- predict(formula, train_data)
  # print(paste("y_pred", y_pred))
  
  print(paste("mse", mse(y_true, y_pred)))
  
  return(mse(y_true, y_pred))
}

#4. 
formula_1 <- train(Salary ~ Hits + Runs, data = bb_train, method="lm")
lm_mse(formula_1, train, test) # 274755.8


formula_2 <- train(Salary ~ Hits + Runs, data = bb_val, method="lm")
lm_mse(formula_2, train, test) # 281841.1

#5. 
source("generate_formulas.R")
# generate_formulas(p = 2, x_vars = c("x1", "x2", "x3", "x4"), y_var = "y")
predictors <- c(colnames(baseball))
predictors <- predictors[-19]

#6. 
# formulas <- paste0("Salary ~ ", combn(predictors, 3, simplify=FALSE))
formulas <- combinations(predictors, k=3, layout="column-major")

#7. 
calculated_mses <- data.frame()

for(i in 1:ncol(formulas)) {
  print(formulas[,i])
  current_formula <- generate_formulas(p = 3, x_vars = formulas[,i], y_var = "Salary")
  # print(current_formula)
  adjusted_formula <- train(formula(current_formula), data = bb_train, method="lm")
  # print(paste("adj frm", adjusted_formula))
  calculated_mses <- rbind(calculated_mses, c(lm_mse(adjusted_formula, bb_train, bb_val), current_formula))
  # print(paste("Lowest MSE:", min(calculated_mses)))
}

min(calculated_mses[,1]) # "267022.946132209"

calculated_mses[calculated_mses$X.300130.680185425.==267022.946132209,] 

# 267022.946132209 Salary ~ PutOuts + Assists + NewLeague

#8. 

# 1 predictor

formulas_1p <- combinations(predictors, k=1, layout="column-major")
calculated_mses_1 <- data.frame()

for(i in 1:ncol(formulas_1p)) {
  # print(formulas_1p[,i])
  current_formula <- generate_formulas(p = 1, x_vars = formulas_1p[,i], y_var = "Salary")
  # print(current_formula)
  adjusted_formula <- train(formula(current_formula), data = bb_train, method="lm")
  # print(paste("adj frm", adjusted_formula))
  calculated_mses_1 <- rbind(calculated_mses_1, c(lm_mse(adjusted_formula, bb_train, bb_val), current_formula))
  # print(paste("Lowest MSE:", min(calculated_mses)))
}

min(calculated_mses_1[,1]) # "269613.440892358"

calculated_mses_1[calculated_mses$X.300130.680185425.==269613.440892358,] #  Salary ~ AtBat


# 2 predictors

formulas_2p <- combinations(predictors, k=2, layout="column-major")
calculated_mses_2 <- data.frame()

for(i in 1:ncol(formulas_2p)) {
  # print(formulas_2p[,i])
  current_formula <- generate_formulas(p = 2, x_vars = formulas_2p[,i], y_var = "Salary")
  # print(current_formula)
  adjusted_formula <- train(formula(current_formula), data = bb_train, method="lm")
  # print(paste("adj frm", adjusted_formula))
  calculated_mses_2 <- rbind(calculated_mses_2, c(lm_mse(adjusted_formula, bb_train, bb_val), current_formula))
  # print(paste("Lowest MSE:", min(calculated_mses)))
}

min(calculated_mses_2[,1]) # "267908.851395671"

calculated_mses_2[calculated_mses_2$X.304696.979445293.==267908.851395671,] # Salary ~ PutOuts + NewLeague


# 4 predictors

formulas_4p <- combinations(predictors, k=4, layout="column-major")
calculated_mses_4 <- data.frame()

for(i in 1:ncol(formulas_4p)) {
  # print(formulas_4p[,i])
  current_formula <- generate_formulas(p = 4, x_vars = formulas_4p[,i], y_var = "Salary")
  # print(current_formula)
  adjusted_formula <- train(formula(current_formula), data = bb_train, method="lm")
  # print(paste("adj frm", adjusted_formula))
  calculated_mses_4 <- rbind(calculated_mses_4, c(lm_mse(adjusted_formula, bb_train, bb_val), current_formula))
  # print(paste("Lowest MSE:", min(calculated_mses)))
}

min(calculated_mses_4[,1]) # "269148.96081363"

calculated_mses_4[calculated_mses_4$X.303795.081650484.==269148.9608136,] # Salary ~ AtBat + Hits + HmRun + Runs

## Best model is with 3 predictors and Salary ~ PutOuts + Assists + NewLeague

#9. 

model <- train(Salary ~ PutOuts + Assists + NewLeague, data = bb_train, method="lm")
pred <- predict(model, bb_test)
mse_test <- mse(bb_test$Salary, pred) # 262107
mse_test

bb_test$type <- "test"
pred <- as.data.frame(pred)
pred %>% 
  rownames_to_column() %>% 
  gather(variable, value, -rowname) %>% 
  spread(rowname, value)

pred$type <- "prediction"

comparison_plot <- ggplot(bb_test, aes(x=Salary, y=pred$pred)) +
  geom_point()
                            
comparison_plot


#10. An input matrix and a response variable.

#11.
x_train <- model.matrix(Salary ~ ., data = bb_train) # had to remove %>% select(-split)
x_train

#12. 
glm <- glmnet(x_train, bb_train$Salary, lambda = 15)
glm

#13. 
coef(glm) # Hits, Runs, Walks, CHits, CRBI, League, PutOuts

#14. 
model_glm <- train(Salary ~ Hits + Runs + Walks + CHits + CRBI + League + PutOuts, data = bb_val, method="glm")
pred_glm <- predict(model_glm, bb_test)
pred_glm

comparison_plot_2 <- ggplot(bb_test, aes(x=Salary, y=pred_glm)) +
  geom_point()

comparison_plot_2

  # MSE on validation set
pred_glm_2 <- predict(model_glm, bb_val)
val_mse <- mse(bb_val$Salary, pred_glm_2)
val_mse # 133616.8

#15. 
glm_lasso <- glmnet(x_train, bb_train$Salary)
glm
glm_lasso

# In addition to providing intercepts for each predictors, the lasso regression model provides information about the L1 norm
# which can be used to decide the ideal lambda value for this set of predictors. Since no lambda was specified, all lambda values
# within a certain range are used. Based on this and plot(glm_lasso), a lambda value can be chosen, ideally the value which minimizes
# the complexity penalization (L1 Norm).

#16. 
cv <- cv.glmnet(x_train, bb_train$Salary, nfolds=15)
cv

# Best lambda value, according to this function, is 31.69

#17.
plot(cv)

# I can see that the log of the complexity measure (lambda) is plotted against the Mean-Squared Error. What can be clearly seen is 
# that there is a low-point for the Mean-Squared Error which is the optimal lambda value. One standard error further is the 1se 
# lambda value. For each point in the plot, a log lambda value is given with the MSE and a given range. The smallest range
# resembles the optimal log lambda value. What this tells us about the bias-variance tradeoff is that a complex model performs 
# reasonably well (log lambda is <= 0). However, performance is increased when complexity is penalized up to a point. So,
# after reaching the optimal lambda value, the MSE shoots up as the model becomes too simple as more complex models are penalized.

#18. 
newX <- model.matrix(Salary ~.,data=bb_test) # Had to fix a categorical variable in bb_test and supply a model.matrix for newx
pred_salary <- predict(cv, newx=newX,  s=31.69, type="link")
pred_salary

comparison_plot_3 <- ggplot(bb_test, aes(x=Salary, y=pred_salary)) +
  geom_point()

comparison_plot_3


#19. 
new_train <- rbind(bb_train, bb_val)

  #a. Linear Regression All Predictors
lr_model_all <- train(Salary ~ ., data = new_train, method="lm")
lr_pred_all <- predict(lr_model_all, bb_test)
lr_mse_all <- mse(bb_test$Salary, lr_pred_all) # 115156.3
lr_mse_all

  #b. Subset Selection Regression Model
ssrm_model <- train(Salary ~ Hits + Runs + Walks + CHits + CRBI + League + PutOuts, data = new_train, method="glm")
ssrm_pred <- predict(ssrm_model, bb_test)
ssrm_mse <- mse(bb_test$Salary, ssrm_pred)
ssrm_mse # 133609.1

  #c. Lasso with Lambda set at 50
lasso50_model <- model.matrix(Salary ~ ., data = new_train) # had to remove %>% select(-split)
lasso50 <- glmnet(lasso50_model, new_train$Salary, lambda = 50)
newX_lambda <- model.matrix(Salary ~.,data=bb_test)
lasso50_pred <- predict(lasso50, newx=newX)
lasso50_mse <- mse(bb_test$Salary, lasso50_pred)

  #d. LASSO with cv-lambda
cv_lasso_model <- model.matrix(Salary ~ ., data = new_train)
cv_lasso <- cv.glmnet(cv_lasso_model, new_train$Salary, nfolds=15)
cv_lasso # min = 3.52
plot(cv_lasso)

newX_cv_lasso <- model.matrix(Salary ~.,data=bb_test) # Had to fix a categorical variable in bb_test and supply a model.matrix for newx
cv_lasso_pred <- predict(cv_lasso, newx=newX,  s=3.52, type="link")
cv_lasso_mse <- mse(bb_test$Salary, cv_lasso_pred)

df_comparison <- data.frame(Method = c("Linear Regression (All predictors)", "Subset Selection Regression Model", "Lasso Lambda 50", "Lasso with CV"), Outcome = c(lr_mse_all, ssrm_mse, lasso50_mse, cv_lasso_mse))

comparison_plot_4 <- ggplot(df_comparison, aes(Method, Outcome)) +
  geom_col() +
  coord_flip()
comparison_plot_4
