library(rpart)
library(rpart.plot)
library(tidyverse)

white = npha_data %>% 
  filter(Race == 1)
nonwhite = npha_data %>% 
  filter(Race != 1)
tree_model <- rpart(`Number of Doctors Visited` ~ ., data = white, 
                    method = "class",control = rpart.control(minsplit = 80, cp=0.005))

rpart.plot(tree_model, box.palette = "gray", main = "Decision Tree")

glimpse(npha_data)
