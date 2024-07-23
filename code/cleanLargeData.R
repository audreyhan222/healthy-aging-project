da_data <- da37305.0001

cleaned_da_data <- da_data %>% 
  select(-CASEID:-QF1, -PPSTATEN, -T_Q1:-T_QF1, -PPWORK, -Q45_9:-Q45_11, -Q4_RECODE,
         -PPINCIMP, -PPEDUCAT, -PPEDUC, -PPREG9, -PPETHM)


cleaned_da_data <- cleaned_da_data %>% 
  select(-PPAGE, -PPHHSIZE, -PPT01:-PPT18OV, -INSURANCE_ANY, -MEDICARE_ANY:-PRIVATE_ANY, -Q45RECODE,
         -ANY_COST_BARRIER)



cleaned_da_data <- cleaned_da_data %>% 
  mutate(SLEEP_OTC = case_when(SLEEP_OTC == "(1) Use regularly" ~ as.factor("(1) Use"),
                               SLEEP_OTC == "(2) Use occasionally" ~ as.factor("(1) Use"),
                               .default = SLEEP_OTC),
         SLEEP_MEDS = case_when(SLEEP_MEDS == "(1) Use regularly" ~ as.factor("(1) Use"),
                                SLEEP_MEDS == "(2) Use occasionally" ~ as.factor("(1) Use"),
                               .default = SLEEP_MEDS),
         PPMARIT_EDITTED = case_when(PPMARIT != "(1) Married" ~ as.factor("(0) Not Married"),
                             .default = PPMARIT),
         RACE_ETH_EDITTED = case_when(RACE_ETH != "(1) White, Non-Hispanic" ~ as.factor("(0) Non-White"),
                                      .default = RACE_ETH),
         .keep = "unused")

glimpse(cleaned_da_data)
summary(cleaned_da_data)
