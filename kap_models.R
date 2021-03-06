# R script to run logistic regression models 1-6 --------------------------

#Load R packages required
library(tidyverse)
library(ResourceSelection) #Hosmer-Lemeshow test

#Import data ensure data file kap_data.rds is saved to you working directory. This file is available in the main directory
kap_data <- read_rds("~/Desktop/kap_data.rds")

#Convert age and education level in years to binary variables
median_age <- median(kap_data$age)
median_educ_yrs <- median(kap_data$education_level_years)
  

#Create final df for modelling
mod_var <- {c(
  #Independent vars
  "CODIGO", "age", "age_bin", "sex", "education_level_years", "education_level_higher", "education_level_years_bin",  "high_income",  "child_present", 
   #Dependent vars
   #Mod 1
   "deng_sympt", 
   #Mod 2
   "mosquito_transmits",  
   #Mod 3
   "prev_containers", 
   #Mod 4
   "overall_knowledge",
   #Mod 5
   "control_any",
   #Mod 6 
   "top25")} #select vars needed
kap_data_mods <- kap_data %>% 
  mutate(age_bin = case_when(age <= median_age ~ "18-40yrs",
                             TRUE ~ "40yrs+"),
         education_level_years_bin = case_when(education_level_years <= median_educ_yrs ~ "0-10.5yrs",
                                 TRUE ~ "10.5+yrs"),
         education_level_higher = education_level == "higher") %>% 
  select(mod_var) %>% 
  within(sex <- relevel(factor(sex), ref = "M")) #reset reference for sex

# Knowledge model ---------------------------------------------------------
#create model list
model_list <-{ c("deng_sympt ~ age_bin + sex + education_level_higher + child_present + high_income",
                 "mosquito_transmits ~ age_bin + sex + education_level_higher + child_present + high_income",
                 "prev_containers ~  age_bin + sex + education_level_higher + child_present + high_income",
                 "overall_knowledge ~ age_bin + sex + education_level_higher + child_present + high_income",
                 "control_any ~ age_bin + sex + education_level_higher + child_present + high_income + overall_knowledge",
                 "top25 ~ age_bin + sex + education_level_higher + child_present + high_income + overall_knowledge")}

#Function to create model out puts
kap_mod_summarise <- function(formula_string) {
  mod_form <- as.formula(formula_string)
  null_form <- str_c(mod_form[[2]], " ~ 1")
  mod_null <- glm(null_form, data = kap_data_mods, family = binomial)
  mod <- glm(mod_form, data = kap_data_mods, family = binomial)
  mod_summary <- summary(mod)
  wald <- as.data.frame(mod_summary$coefficients)[[4]]
  R2_temp <- 1-logLik(mod)/logLik(mod_null)
  R2_mcf <- round(R2_temp[1], 3) #McFadden's R squared
  HL_temp <- hoslem.test(x = mod$y, y = fitted(mod), g = 10)
  HL_pval <- round(HL_temp$p.value, 2) #Hoslem test
  
 
  vif_max <- round(max(car::vif(mod)),digits = 2) 
 
  
  round(exp(cbind(OR=coef(mod),confint(mod))),2) %>%
    as.data.frame() %>%
    rownames_to_column("independent") %>%
    mutate(model = as.character(mod_form[[2]]),
           R2_mcf = R2_mcf,
           HL_pval = HL_pval,
           CI_95 = str_c(`2.5 %`, `97.5 %`, sep = "-"),
           wald = round(wald, 3),
           wald_star = case_when(wald < 0.01 ~ "**",
                                 wald < 0.05 ~ "* ",
                                 TRUE ~ "  "),
           wald_star_comb = str_c(wald, wald_star),
           OR_star = str_c(OR, wald_star),
           OR_CI = str_c(OR_star, " (", CI_95, ")"),
           independent = case_when(independent == "EDAD" ~ "age (yrs)",
                                   independent == "SEXOF" ~ "sex (f)",
                                   independent == "NIVEDUC2" ~ "education (yrs)",
                                   independent == "MENOR_DE_EDADTRUE" ~ "child (<5yrs)",
                                   independent == "high_incomeTRUE" ~ "high income",
                                   TRUE ~ independent),
           vif = vif_max) %>%
    select(model, independent, OR_CI, wald_star_comb, HL_pval, R2_mcf,vif) %>% 
    filter(independent != "(Intercept)")
}

#iterate over models and create summary df
kap_mod_summary <- map_df(model_list, kap_mod_summarise) 

