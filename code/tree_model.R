library(tidyverse)
library(rsample)
library(glmnet)
library(randomForest)
library(randomForestExplainer)
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
         .keep = "unused")


summary(npha_data)
rand_forest_model <- randomForest(`Number of Doctors Visited` ~ .,
                                  data = npha_data,
                                  importance = TRUE, nperm = 3,
                                  na.action = na.omit)
plot(rand_forest_model)
randomForest::importance(rand_forest_model, type=1)

# par(mfrow = c(1, 2))
# varImpPlot(rand_forest_model, type=1, main = "Importance: permutation")
# varImpPlot(rand_forest_model, type=2, main = "Importance: node impurity")

data_split <- initial_split(npha_data, prop = 0.7)
train_data = training(data_split)
test_data = testing(data_split)

rand_forest_model <- randomForest(`Number of Doctors Visited` ~ .,
                                  data = train_data,
                                  importance = TRUE, nperm = 3,
                                  na.action = na.omit,
                                  ntrees = 700)

predicted <- predict(rand_forest_model, newdata=test_data)
predicted
mean(test_data$`Number of Doctors Visited` == predicted)
