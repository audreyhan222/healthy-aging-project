library(tidyverse)
library(rsample)
library(glmnet)
set.seed(3)
npha_data <- read_csv("data/NPHA-doctor-visits.csv")


#clean data
npha_data <- npha_data %>% 
  mutate_all(as.factor) %>% filter(`Phyiscal Health` != -1) %>% 
  select(-Age)


dummies <- as.data.frame(model.matrix(~.-1, npha_data))
data_split <- initial_split(dummies, prop = 0.3)
#need to drop response variables

train_x <- training(data_split) %>% select(-1:-3) %>% as.matrix()
train_y <- training(data_split) %>% select(1:3) %>% as.matrix()

test_x <- testing(data_split) %>% select(-1:-3)%>% as.matrix()
test_y <- testing(data_split) %>% select(1:3)%>% as.matrix()


#fit model with lasso

fit <- cv.glmnet(train_x,train_y,  family = "multinomial", type.measure = "class", nfolds = 10)

preds <- predict(fit, newx = test_x, type = "class", s = "lambda.min")

summary(fit)
coef(fit)




library(tidyverse)
library(rsample)
library(glmnet)
set.seed(3)
npha_data <- cleaned_da_data


#clean data



dummies <- as.data.frame(model.matrix(~.-1, npha_data))
data_split <- initial_split(dummies, prop = 0.3)
#need to drop response variables

train_x <- training(data_split) %>% select(-7) %>% as.matrix()
train_y <- training(data_split) %>% select(7) %>% as.matrix()

test_x <- testing(data_split) %>% select(-7)%>% as.matrix()
test_y <- testing(data_split) %>% select(7)%>% as.matrix()


#fit model with lasso

fit <- cv.glmnet(train_x,train_y,  family = "multinomial", type.measure = "class", nfolds = 10)

preds <- predict(fit, newx = test_x, type = "class", s = "lambda.min")

summary(fit)
coef(fit)
