# IST719 M800
# Information Visualization
#Final Project
#Group_1
######################################

#Setting up workspace
dev.off() # Clear the graph window
cat('\014')  # Clear the console
rm(list=ls()) # Clear all user objects from the environment!!!

#Loading required libraries
library(tidyverse)
library(lubridate)

#importing dataset
filepath <- file.choose()

raw_data <- read.csv(filepath, header = T) %>% 
  janitor::clean_names() 

#Data cleaning
data <- raw_data %>%
  mutate(action = fct_recode(action,"Not yet inspected" =""),
         critical_flag = fct_recode(critical_flag, "Not Applicable" = ""),
         violation_code = factor(violation_code),
         inspection_date = mdy(inspection_date),
         grade_date = mdy(grade_date)) %>%
  filter(latitude !=0 & longitude !=0 & grade !="") %>%
  select(-phone, -record_date, -community_board, 
         -census_tract,-council_district,
         -bin, -bbl,-nta) %>%
  mutate(grade = fct_recode(grade, "Z" = "G"),
         boro = factor(boro)) %>%
  na.omit()
  
  
summary(data)

# Distribution of restaurants
ggplot() + 
  geom_text(data=restaurants, 
            aes(x=longitude, y=latitude, 
                col = boro, label = grade),
            show.legend = FALSE)+
  ylim(min(restaurants$latitude),max(restaurants$latitude))+
  xlim(min(restaurants$longitude),max(restaurants$longitude))

#Most recent grading of restaurants
restaurants <- data %>%
  select(camis, latitude, longitude, boro, grade, inspection_date) %>%
  group_by(camis) %>%
  filter(inspection_date == max(inspection_date)) %>%
  unique()

ggplot()+
  geom_bar(data=restaurants, 
           aes(x=grade, fill = boro),
           position = "dodge")+
  labs(title = "Most recent Gradings",
       subtitle = "Mutiple inspections are conducted on restaurants.\nThe plot depicts the grade given to a restaurant on the most recent inspection.",
       x ="Grade",
       y = "Number of Restaurants")
