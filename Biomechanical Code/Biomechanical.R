library(ggrepel)
library(tidyverse)
library(caret)


Biomech <- read.csv("column_3C_weka.csv")
#Split data into 70% and 30% for training and test, respectively
set.seed(123)
training.samples <- Biomech$class %>%
  createDataPartition(p=0.7, list = FALSE)
train_data <- Biomech[training.samples,]
test_data <- Biomech[-training.samples,]

#We need to use Lasso and ridge methods to "remove highly correlated variables
#OR We can use PCA to combine the variables into independent variables


#Normalizing the data
Pre <- train_data %>%
  preProcess(method = c("center","scale"))
train_data_2 <- Pre %>% predict(train_data)
test_data_2  <- Pre %>% predict(test_data)

##Linear Discriminant Analysis (LDA)
#Will be using the MASS package to easily compute the LDA
library(MASS)
model <- lda(class~., data = train_data_2)
predictions <- model %>% predict(test_data_2)
mean(predictions$class == test_data_2$class)

#89.24% correct prediction rate

plot(model)
names(predictions)

lda.data <- cbind(train_data_2, predict(model)$x)
ggplot(lda.data, aes(LD1, LD2)) +
  geom_point(aes(color = class))

##Quadratic Discriminat Analysis (QDA)
#Here I am hoping that QDA will be a better predictor due to the variables being collinear

#model_2 <- qda(class~., data = train_data_2)
#predictions_2 <- model_2 %>% predict(test_data_2)
#mean(predicitions_2$class == test_data_2$class)

#Flexible Distriminant Analysis (FDA)
library(mda)
model_3 <- fda(class~., data = train_data_2)
predicted_3 <- model_3 %>% predict(test_data_2)
mean(predicted_3 == test_data_2$class)




