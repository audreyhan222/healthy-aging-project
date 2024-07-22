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
  mutate(Physical_Health = `Phyiscal Health`,
         Mental_Health = case_when(`Mental Health` == 5 ~3, `Mental Health` ==4 ~3, .default = `Mental Health`),
         Dental_Health = `Dental Health`,
         Employment = case_when(Employment == 2 ~1, .default = Employment),
         Trouble_Sleeping = case_when(`Trouble Sleeping` == 2 ~1, .default = `Trouble Sleeping`),
         Stress_Sleeping = `Stress Keeps Patient from Sleeping`,
         Pain_Sleeping = `Pain Keeps Patient from Sleeping`,
         Bathroom_Sleeping = `Bathroom Needs Keeps Patient from Sleeping`,
         Unknown_Sleeping = `Uknown Keeps Patient from Sleeping`,
         Medication_Sleeping = `Medication Keeps Patient from Sleeping`,
         Prescription_Sleep_Medication = case_when(`Prescription Sleep Medication` == 2 ~1, .default = `Prescription Sleep Medication`),
         Race = case_when(Race != 1 ~ 0, .default = Race),
         .keep = "unused") %>% 
  mutate_all(as.factor) %>%
  select(-Age)

glimpse(npha_data)
rand_forest_model <- randomForest(`Number of Doctors Visited` ~ .,
                                  data = npha_data,
                                  importance = TRUE, nperm = 3,
                                  na.action = na.omit)
plot(rand_forest_model)


