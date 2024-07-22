library(tidyverse)
library(rsample)
library(glmnet)
library(rpart)
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

summary(npha_data)
