library(tidyverse)
library(rsample)
library(glmnet)
set.seed(3)
npha_data <- read_csv("data/NPHA-doctor-visits.csv")

glimpse(npha_data)
npha_data <- npha_data %>% 
  select(-Age)

npha_data <- npha_data %>% 
  filter(`Phyiscal Health` != -1,
         `Mental Health` != -1)