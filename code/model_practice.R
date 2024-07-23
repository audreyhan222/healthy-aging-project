library('tidyverse')
library('rsample')
npha_data <- NPHA_doctor_visits

npha_data <- npha_data %>% 
  mutate(num_doctors = case_when(`Number of Doctors Visited` == 1 ~ 0,
                                            `Number of Doctors Visited` == 2 ~ 1,
                                            `Number of Doctors Visited` == 3 ~ 1))
npha_data <- mutate_all(npha_data, as.factor)
logistic_model <- glm(num_doctors ~., 
                      family = binomial, data = npha_data)
summary(logistic_model)

data_split = initial_split(npha_data, prop = 0.7)
train_data = training(data_split)
test_data = testing(data_split)

pred_prob <- logistic_model %>% 
  predict(test_data, type = "response")

predicted.classes <- ifelse(pred_prob > 0.5, "1", "0")
mean(predicted.classes == test_data$num_doctors)


summary(npha_data)
predicted.classes
