library(tidyverse)
library(rsample)
library(glmnet)
set.seed(3)
npha_data <- read_csv("data/NPHA-doctor-visits.csv")


npha_data <- npha_data %>% 
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

twothreesubset <- npha_data %>% filter(`Number of Doctors Visited` != 2)

twothreesubset <- twothreesubset %>% mutate_all(as.factor)



data_split_1 <- initial_split(twothreesubset, prop = 0.7)
#need to drop response variables

train <- training(data_split_1)
test <- testing(data_split_1)

train_y <- train$`Number of Doctors Visited`

test_y <- test$`Number of Doctors Visited`
train_dummies <- model.matrix(~.-1, train[-1])
test_dummies <- model.matrix(~.-1, test[-1])


#fit model with lasso

#fit <- cv.glmnet(train_dummies,train_y,family = "binomial", alpha =0.5, nfolds = 10, keep = TRUE)

fit <- cv.glmnet(train_dummies,train_y,family = "binomial", alpha =0.5, type.measure = "class", nfolds = 10, keep = TRUE)


preds <- predict(fit, newx = test_dummies, type = "response", s ="lambda.min")

idmin = match(fit$lambda.min, fit$lambda)
fit$lambda
fit$lambda.min

idmin

library(pROC)

roc_curve <- roc(test_y, preds)

plot(roc_curve)

plot(roc.glmnet(fit, newx = test_dummies, newy = test_y))

attr <- roc.glmnet(fit, newx = test_dummies, newy = test_y)
attr

#assess.glmnet(fit, newx = test_dummies, newy = test_y, s = 0.01)
