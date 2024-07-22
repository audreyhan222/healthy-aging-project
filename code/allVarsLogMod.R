library(tidyverse)
library(rsample)

set.seed(13)
npha_data <- read_csv("data/NPHA-doctor-visits.csv")

glimpse(npha_data)


npha_data <- npha_data %>% 
  mutate(num_doctors = case_when(`Number of Doctors Visited` == 1 ~ 0,
                                 `Number of Doctors Visited` == 2 ~ 0,
                                 `Number of Doctors Visited` == 3 ~ 1))

npha_data <- npha_data %>% mutate_all(as.factor) %>% filter(`Phyiscal Health` != -1)

data_split <- initial_split(npha_data, prop = 0.7)

train <- training(data_split)
test <- testing(data_split)
log_mod <- glm(num_doctors ~
                 `Phyiscal Health` + 
                 `Mental Health` + 
                 `Dental Health` + 
                 Employment +
                 `Stress Keeps Patient from Sleeping` +
                 `Medication Keeps Patient from Sleeping` +
                 `Pain Keeps Patient from Sleeping` +
                 `Bathroom Needs Keeps Patient from Sleeping` +
                 `Uknown Keeps Patient from Sleeping` +
                 `Trouble Sleeping` +
                 `Prescription Sleep Medication` + 
                 Race +
                 Gender,  family = binomial, data = train)




pred_prob <- log_mod %>% 
  predict(test, type = "response")


predicted.classes <- ifelse(pred_prob>0.5, "1", "0")

mean(predicted.classes == test$num_doctors)
summary(log_mod)