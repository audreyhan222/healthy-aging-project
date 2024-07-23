library(tidyverse)
library(rsample)
library(glmnet)
library(MASS)
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
  dplyr::select(-Age)

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
        mutate_all(as.factor)




data_split <- initial_split(npha_data, prop = 0.3)
#need to drop response variables
train <- training(data_split)
test <- testing(data_split)

test_x <- test %>% 
          dplyr::select(-`Number of Doctors Visited`)

test_y <- test$`Number of Doctors Visited`

mod <- polr(`Number of Doctors Visited` ~ ., data = train, Hess =TRUE, method = "cloglog")
preds <- predict(mod, newdata = test_x)

preds

mean(preds == test_y)


summary(mod)

# 
# library(BSDA)
# 
# ctable <- coef(summary(mod))
# p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) *2
# 
# ctable <- cbind(ctable, "p value" = p)
# 
# ctable