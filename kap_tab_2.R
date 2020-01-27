# R-script for creating table 2  ------------------------------------------

#Load r packages
library(tidyverse)

#Import data ensure data file kap_data.csv is saved to you working directory
kap_data <- read_csv("~/Desktop/kap_data.csv")

#Variabes required for table 2 
tab2_var <- {c("sex", "heard_den", "den_lima", "high_risk", 
  "know_someone_den", "mosquito_transmits", "fever_chills", "head_eye", 
  "malaise_fatigue_apetite", "joint_body", "deng_sympt", "prev_containers", 
  "prev_clean_house", "prev_products", "prev_fumigate", "prev_bednet", 
  "prev_dispose_waste", "mosq_house_last_year", "mosq_season_summer", 
  "mosq_season_winter", "mosq_season_spring", "mosq_season_autumn", 
  "control_any", "control_products", "control_clean_house", "control_fumigate", 
  "control_containers", "products_insec_spray", "products_repel_coil", 
  "products_bleach", "products_vaporize", "seen_mosq", "cont_nuisance", 
  "cont_fear")}

#Create table 2
#nest all data frames for each question and apply chi-squared to erach data frame
tab_2 <- kap_data %>% 
  select(tab2_var) %>% 
  gather(question, present, -sex) %>% 
  nest(-question) %>% 
  mutate(total = map_dbl(data, function(x){sum(x$present, na.rm = T)}),
         prop = round(map_dbl(data, function(x){100*mean(x$present, na.rm = T)}), 1),
         num_f = map_dbl(data, function(x){sum(x$present[x$sex == "F"], na.rm = T)}),
         prop_f = round(map_dbl(data, function(x){100*mean(x$present[x$sex == "F"], na.rm = T)}), 1),
         num_m = map_dbl(data, function(x){sum(x$present[x$sex == "M"], na.rm = T)}),
         prop_m = round(map_dbl(data, function(x){100*mean(x$present[x$sex == "M"], na.rm = T)}), 1),
         chi_sex = map(data, function(x){chisq.test(x$sex, x$present)}),
         chi_sex_p = round(map_dbl(chi_sex, "p.value"), 3),
         chi_sex_sig = case_when(chi_sex_p < 0.01 ~ "**",
                                 chi_sex_p < 0.05 ~ "*",
                                 TRUE ~ ""),
         total = str_c(prop, " (", total, ")"),
         sex_m = str_c(prop_m, " (", num_m, ")"),
         sex_f = str_c(prop_f, " (", num_f, ")"),
         chi_sex_p = str_c(chi_sex_p, chi_sex_sig)) %>% 
  select(question, total, sex_m, sex_f, chi_sex_p)

View(tab_2)  
