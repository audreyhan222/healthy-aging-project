da_data <- da37305.0001

cleaned_da_data <- da_data %>% 
  select(-1:-5, -7, -8:-10, -32:-53, -55:-98, -115:-122, -Q20:-Q45_REFUSED, 
         -Q47:-Q60C, -Q62:-Q70, -Q72:-QF1, -Q4_OTHER, -Q6, -Q6_OTHER, -Q13A, 
         -Q13B, -Q14_REFUSED, -Q14_OTHER, -Q17, -Q37_OTHER, -Q45_OTHER, -PPSTATEN,
         -PPAGECAT, -PPAGECT4, -PPEDUC, -PPT01:-PPT18OV, -PPHHHEAD, -PPHOUSE,
         -PPRENT:-T_QF1, -Q45RECODE:-Q45_11, -INSURANCE_SENIOR:-INSURANCE_YOUNGER3, 
         -PPREG4, -Q3, -Q16, -XSENIOR, -MEDICARE_ANY:-PRIVATE_ANY, -SLEEP_PROBLEMS_3CAT:-PROB_WAKING,
         -PASSIVE:-PHARM_LESSCOST, -POOR_SLEEP_NORMAL, -Q1:-Q2, -Q4_1:-Q4_REFUSED, -Q7,
         -Q4_RECODE, -PPINCIMP, -PPEDUCAT, -Q8, -Q10, -PPMSACAT, -Q2A, -Q5, -DENTAL_APPROACH:-REC_LOWPRICE,
         -INSURANCE_ANY, -RACE_ETH)


cleaned_da_data <- cleaned_da_data %>% 
  select(-7:-17, -1: -6, -AGE_CAT)

cleaned_da_data <- cleaned_da_data %>% 
  drop_na()

cleaned_da_data <- cleaned_da_data %>% 
  mutate(SLEEP_OTC = case_when(SLEEP_OTC == "(1) Use regularly" ~ as.factor("(1) Use"),
                               SLEEP_OTC == "(2) Use occasionally" ~ as.factor("(1) Use"),
                               .default = SLEEP_OTC),
         SLEEP_MEDS = case_when(SLEEP_MEDS == "(1) Use regularly" ~ as.factor("(1) Use"),
                                SLEEP_MEDS == "(2) Use occasionally" ~ as.factor("(1) Use"),
                               .default = SLEEP_MEDS),
         PPMARIT = case_when(SLEEP_MEDS == "(1) Married" ~ as.factor("(2) Married"),
                             
                             .default = PPMARIT)) %>% 
  filter(PPMARIT != "PPMARIT(-1)")

glimpse(cleaned_da_data)
summary(cleaned_da_data)
