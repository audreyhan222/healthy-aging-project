library(tidyverse)
library(rsample)
library(glmnet)
set.seed(3)
npha_data <- read_csv("data/NPHA-doctor-visits.csv")


#clean data
npha_data <- npha_data %>% 
  mutate(num_doctors = case_when(`Number of Doctors Visited` == 1 ~ 0,
                                 `Number of Doctors Visited` == 2 ~ 0,
                                 `Number of Doctors Visited` == 3 ~ 1)) %>% 
  mutate_all(as.factor) %>% filter(`Phyiscal Health` != -1) %>% 
  select(-Age)


dummies <- as.data.frame(model.matrix(~.-1, npha_data))
data_split <- initial_split(dummies, prop = 0.3)
#need to drop response variables

train_x <- training(data_split) %>% select(-num_doctors1, -1:-3) %>% as.matrix()
train_y <- training(data_split) %>% select(num_doctors1) %>% as.matrix()

test_x <- testing(data_split) %>% select(-num_doctors1, -1:-3)%>% as.matrix()
test_y <- testing(data_split) %>% select(num_doctors1)%>% as.matrix()


#fit model with lasso

fit <- cv.glmnet(train_x,train_y,  family = "binomial", type.measure = "class", nfolds = 10)

preds <- predict(fit, newx = test_x, type = "class")

mean(preds == test_y)

