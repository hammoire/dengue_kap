# R-script to create table 1 ----------------------------------------------

#Load libraries
library(tidyverse)

#Import data ensure data file kap_data.csv is saved to you working directory
kap_data <- read_csv("~/Desktop/kap_data.csv")


# Create table values -----------------------------------------------------
#Age
median(kap_data$age)
quantile(kap_data$age, c(0.25, 0.75))

#Sex
(sex_tab <- table(kap_data$sex)) #freq
sex_tab/sum(sex_tab) * 100 #%

#Education level
(educ_tab <- table(factor(kap_data$education_level, 
             levels = c("primary", "secondary", "higher"),
             ordered = TRUE))) #freq
educ_tab/sum(educ_tab) * 100 #%


#Occupation
(occu_tab <- table(kap_data$occupation)) #freq
occu_tab/sum(occu_tab) * 100 #%

#Annual income
(income_tab <- table(factor(kap_data$income_annual, 
             levels = c("0-499 USD", "500-999 USD", "1000-4999 USD", "5000+"),
             ordered = TRUE))) #freq
income_tab/sum(income_tab) * 100 #%

#People per household
median(kap_data$num_people_house)
quantile(kap_data$num_people_house, c(0.25, 0.75))

#Child less than 5 years present
(child_tab <- table(kap_data$child_present,exclude = "ifany")) #freq
child_tab/sum(child_tab) * 100 #%



