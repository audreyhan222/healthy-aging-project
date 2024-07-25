library(rpart)
library(rpart.plot)
library(tidyverse)

white = npha_data %>% 
  filter(Race == 1)
nonwhite = npha_data %>% 
  filter(Race != 1)
tree_model_white <- rpart(`Number of Doctors Visited` ~ ., data = white, 
                          method = "class",control = rpart.control(minsplit = 80, cp=0.005))

rpart.plot(tree_model_white, box.palette = "gray", main = "Decision Tree for Group 1",
           clip.facs = TRUE)

tree_model_nonwhite <- rpart(`Number of Doctors Visited` ~ ., data = nonwhite, 
                             method = "class",control = rpart.control(minsplit = 30, cp=0.001))

rpart.plot(tree_model_nonwhite, box.palette = "gray", main = "Decision Tree for Group 2")