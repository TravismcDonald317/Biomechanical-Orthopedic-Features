library(ggrepel)
library(tidyverse)
library(caret)
library(glmnet)

Biomech <- read.csv("column_3C_weka.csv")
#Lasso Regression Model

y <- Biomech$class
x <- data.matrix(Biomech[, c('pelvic_incidence','pelvic_tilt','lumbar_lordosis_angle','sacral_slope','pelvic_radius','degree_spondylolisthesis')])

#perform k-fold cross-validation to find optimal lambda value
cv_model <- cv.glmnet(x, y, alpha = 1, family = "multinomial")

#find optimal lambda value that minimizes test MSE
best_lambda <- cv_model$lambda.min
best_lambda
plot(cv_model) 

best_model <- glmnet(x, y, alpha = 1, family = "multinomial", lambda = best_lambda)
coef(best_model)

y_predicted <- predict(best_model, s = best_lambda, newx = x)

#These show how effective our Lasso Model is using our lambda value = 0.002617861
rsq <- 1 - cv_model$cvm/var(y_predicted)
plot(cv_model$lambda,rsq)

#Learned that for Hernia
