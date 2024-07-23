library(tidyverse)
library(rsample)
library(glmnet)
set.seed(3)
npha_data <- read_csv("data/NPHA-doctor-visits.csv")


#clean data

npha_data <- npha_data %>% 
  filter(Race == 1) %>% 
  mutate(`Phyiscal Health` = case_when(`Phyiscal Health` == 2 ~ 1,
                                     `Phyiscal Health` == 5 ~ 4,
                                     .default = `Phyiscal Health`),
         `Dental Health` = case_when(`Dental Health` == 2 ~ 1,
                                   `Dental Health` == 5 ~ 4,
                                   `Dental Health` == 6 ~ 4,
                                   .default = `Dental Health`))

npha_data <- npha_data %>% 
  filter(`Phyiscal Health` != -1, `Mental Health` != -1, `Dental Health` != -1, `Trouble Sleeping` != -1, `Prescription Sleep Medication` != -1) %>%
  mutate(`Mental Health` = case_when(`Mental Health` == 5 ~3, `Mental Health` ==4 ~3, .default = `Mental Health`)) %>% 
  mutate(Employment = case_when(Employment == 2 ~1, .default = Employment)) %>% 
  mutate(`Trouble Sleeping` = case_when(`Trouble Sleeping` == 2 ~1, .default = `Trouble Sleeping`)) %>% 
  mutate(`Prescription Sleep Medication` = case_when(`Prescription Sleep Medication` == 2 ~1, .default = `Prescription Sleep Medication`)) %>% 
  mutate(Race = case_when(Race != 1 ~ 0, .default = Race)) %>% 
  mutate_all(as.factor) %>%
  select(-Age)

npha_data <- npha_data %>% 
  mutate(Physical_Health = `Phyiscal Health`,
         Mental_Health = `Mental Health`,
         Dental_Health = `Dental Health`,
         Employment = Employment,
         Trouble_Sleeping = `Trouble Sleeping`,
         Stress_Sleeping = `Stress Keeps Patient from Sleeping`,
         Pain_Sleeping = `Pain Keeps Patient from Sleeping`,
         Bathroom_Sleeping = `Bathroom Needs Keeps Patient from Sleeping`,
         Unknown_Sleeping = `Uknown Keeps Patient from Sleeping`,
         Medication_Sleeping = `Medication Keeps Patient from Sleeping`,
         Prescription_Sleep_Medication = `Prescription Sleep Medication`,
         Race = Race,
         .keep = "unused") %>% 
  select(-Race)




  
  
summary(npha_data)


data_split <- initial_split(npha_data, prop = 0.3)

train <- training(data_split)
test <- testing(data_split)

train_y <- train$`Number of Doctors Visited`

test_y <- test$`Number of Doctors Visited`
train_dummies <- model.matrix(~.-1, train[-1])
test_dummies <- model.matrix(~.-1, test[-1])
#need to drop response variables



#fit model with lasso

fit <- cv.glmnet(train_dummies,train_y,  family = "multinomial", 
                 type.measure = "class", nfolds = 10, alpha = 0.5)

preds <- predict(fit, newx = test_dummies, type = "class", s = "lambda.min")

pred_train <- predict(fit, newx = train_dummies, type ="class", s = "lambda.min")
mean(preds==as.data.frame(test_y))

preds

