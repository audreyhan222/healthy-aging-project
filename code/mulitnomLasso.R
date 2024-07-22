library(tidyverse)
library(rsample)
library(glmnet)
set.seed(3)
npha_data <- read_csv("data/NPHA-doctor-visits.csv")


#clean data
npha_data <- npha_data %>% 
  filter(`Phyiscal Health` != -1, `Mental Health` != -1, `Dental Health` != -1, `Trouble Sleeping` != -1, `Prescription Sleep Medication` != -1) %>%
  mutate(`Mental Health` = case_when(`Mental Health` == 5 ~3, `Mental Health` ==4 ~3, .default = `Mental Health`)) %>% 
  mutate(Employment = case_when(Employment == 2 ~1, .default = Employment)) %>% 
  mutate(`Trouble Sleeping` = case_when(`Trouble Sleeping` == 2 ~1, .default = `Trouble Sleeping`)) %>% 
  mutate(`Prescription Sleep Medication` = case_when(`Prescription Sleep Medication` == 2 ~1, .default = `Prescription Sleep Medication`)) %>% 
  mutate(Race = case_when(Race != 1 ~ 0, .default = Race)) %>% 
  mutate_all(as.factor) %>%
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



