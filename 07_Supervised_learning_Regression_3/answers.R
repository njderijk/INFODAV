library(MASS)
library(splines)
library(ISLR)
library(tidyverse)
library(caret)
library(cowplot)

idx <- sample(seq(1, 2), size = nrow(Boston), replace = TRUE, prob = c(.8, .2))
train <- Boston[idx == 1,]
test <- Boston[idx == 2,]


#1. 
pred_plot <- function(lm) {
  pred <- predict(lm, Boston)
  # print(pred)
  
  Boston %>% 
    ggplot(aes(x = lstat, y = medv)) +
    geom_point() +
    geom_line(aes(y = pred), size = 1, color="red") +
    theme_minimal()
  
}
model <- lm(medv ~ ., data=Boston)


#2. 
lin_mod <- lm(medv ~ lstat, data=Boston)
pred_plot(model)
pred_plot(lin_mod)

# I get exactly the same plot (so, same predictions...)

#3. 
pn3_mod <- lm(medv ~ I(lstat^2) + I(lstat^3), data=Boston)
pred_plot(pn3_mod)

#4. 

poly(1:10, 3)
poly(1:10, degree = 3, raw = TRUE)
poly(1:10, degree = 7, raw = TRUE)


#5.
pn3_mod_rt <- lm(medv ~ poly(lstat, degree = 3, raw = TRUE)[,3], data=Boston)
pn3_mod_rf <- lm(medv ~ poly(lstat, degree = 3, raw = FALSE)[,3], data=Boston)
pred_plot(pn3_mod_rt)
pred_plot(pn3_mod_rf)


#6.

pw2_mod <- lm(medv ~ I(lstat <= median(lstat)), data=Boston)
pred_plot(pw2_mod)
coef(pw2_mod)
# (Intercept)                   I(lstat <= median(lstat))TRUE 
# 16.67747                      11.71067 

# A low lstat value neighborhood will have a predictev medv value of: 28.38814 (combined coefficients).


#7. 
pw5_mod <- lm(medv ~ I(cut(lstat, breaks=5)), data=Boston)
pred_plot(pw5_mod)
coef(pw5_mod)


#9. 

piecewise_cubic_basis <- function(vec, knots = 1) {
  
  # If knots is 0, use 3 degrees for creating a vector of polynomials, else, just use the knots and continue with the function.
  if (knots == 0) return(poly(vec, degree = 3, raw = TRUE))
  
  # Cut the vector in equally spaced sections (based on values, not data). The sections is the value of knots + 1.
  cut_vec <- cut(vec, breaks = knots + 1)
  
  # Create an empty matrix with an amount of rows equal to the length of "vec" and it should have 0 columns).
  out <- matrix(nrow = length(vec), ncol = 0)
  
  
  # For every level/section in the levels(cut_vec) statement
  for (lvl in levels(cut_vec)) {
    # A temporary variable becomes "vec"
    tmp <- vec
    # Subset the cut_vec variable for where it is not equal to iteration variable "lvl"
    tmp[cut_vec != lvl] <- 0
    # Append "out" with the polynomial values of "tmp"
    out <- cbind(out, poly(tmp, degree = 3, raw = TRUE))
  }
  
  out
}

vec <- 1:20
knots <- 1
piecewise_cubic_basis(vec, knots)

#10.

pc1_mod <- lm(medv ~ piecewise_cubic_basis(Boston$lstat, 1), data=Boston)
pc2_mod <- lm(medv ~ piecewise_cubic_basis(Boston$lstat, 2), data=Boston)
pc3_mod <- lm(medv ~ piecewise_cubic_basis(Boston$lstat, 3), data=Boston)

pred_plot(pc1_mod)
pred_plot(pc2_mod)
pred_plot(pc3_mod)

# The differences between the plots should be interpeted with the "kinks" in the prediction line. For each knot, 
# there is a kink in the line which indicates the sections in which the predictor variable was split.
# Based on the section size, an alternate median is calculated and will therefore have an alternate prediction value.

#11. 
boston_tpb <- Boston %>%
  select(medv, lstat)

#12. 
boston_tpb <- boston_tpb %>%
  mutate(lstat_s <- lstat^2) %>%
  mutate(lstat_c <- piecewise_cubic_basis(Boston$lstat, 3))


#13.
boston_tpb <- boston_tpb %>%
  mutate(lstat_tpb <- ifelse(lstat < median(lstat), 0, lstat - median(lstat)^3))

#14.
tpb_mod <- lm(medv ~ ., data=boston_tpb)
tpb_mod
# There are 4 predictors.
tpb_mod[["df.residual"]] # 492 DoF

#15.
bs1_mod <- lm(medv ~ bs(lstat, knot=median(lstat)), data=Boston)
bs1_pred <- predict(bs1_mod, Boston)
tpb_pred <- predict(tpb_mod, boston_tpb)

bs1_pred
tpb_pred

#16.
pred_plot(bs1_mod)

#17.
ns3_mod <- lm(medv ~ ns(lstat, df=3), data=Boston)
pred_plot(ns3_mod)

# When comparing this model to bs1_mod, one signficant difference is that this model does not slope up near the end.
# However, near its beginning, it does not seem to take into account the higher values of medv since it "starts" on a lower value.
# Most likely due to a more aggressive 'averaging' strategy.

#18. 

acquire_plot <- function(lm) {
  pred <- predict(lm, Boston)
  plot <- Boston %>% 
    ggplot(aes(x = lstat, y = medv)) +
    geom_point() +
    geom_line(aes(y = pred), size = 1, color="red") +
    theme_minimal()
  
  
  
}

p1 <- acquire_plot(lin_mod)
p2 <- acquire_plot(pn3_mod)
p3 <- acquire_plot(pw5_mod)
p4 <- acquire_plot(pc3_mod)
p5 <- acquire_plot(bs1_mod)
p6 <- acquire_plot(ns3_mod)

plot_grid(p1,p2,p3,p4,p5,p6)
# Titles do not show with cowplot::plot_grid()
