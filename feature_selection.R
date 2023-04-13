library(tidyverse)
library(readxl)
library(dplyr)
library(ggplot2)
library(MASS)
library(haven)

# NEED TO INSTALL
library(survey)

# setwd("~/Desktop/DSI Spring/Stats/Final Project")

####################################################
# TODO: Get regression for each variable in a loop #
####################################################

load('cleaned_data.Rdata')

df_covariates <- df %>% dplyr::select(year, wrkstat, marital, age, race, educ, sex, born, income,
                                      region, partyid, relig, zodiac, vstrat, vpsu, wgt_comb)

df_variables <- df %>% dplyr::select(wealth_imp, sex_imp, parents_imp, educ_imp, hardWork_imp, rightPpl_imp,
                                     political_imp, race_imp, religion_imp)

stat_matrix <- matrix(NA, 9, 1)
fwd_mdl_list <- list()
bck_mdl_list <- list()
idx <- 1
# (WIP) Forwards and Backwards Logistic Regression
for (col in df_variables){
  # Create data we need for specific variable
  df_cov_temp <- df_covariates
  df_cov_temp$var <- col
  df_cov_temp <- df_cov_temp %>% drop_na()
  
  # Create Survey design object to control weights
  surv.des <- svydesign(data = df_cov_temp, ids = ~vpsu, weights = ~wgt_comb, strata = ~vstrat, nest=TRUE)
  
  # t-test between year and variable
  ttest.svy <- svyttest(formula = var ~ year, design = surv.des)
  stat_matrix[idx] <- ttest.svy$p.value
  
  # Forward and Backward stepwise regression
  lm_full <- svyglm(formula = var ~ year+wrkstat+marital+age+race+educ+sex+born+income
                    +region+partyid+relig+zodiac, 
                    design = surv.des, family=quasibinomial(link="logit"))
  lm_none <- svyglm(formula = var~1, design = surv.des, family=quasibinomial(link="logit"), data = df_cov_temp)
  
  fwd_step_mdl <- stepAIC(object = lm_none, direction='forward', scope=list(upper=lm_full, lower=lm_none))
  fwd_mdl_list <- append(fwd_mdl_list, list(summary(fwd_step_mdl)))
  
  # Using AIC as metric for feature selection
  bkd_step_mdl <- stepAIC(lm_full, direction='backward')
  bck_mdl_list <- append(bck_mdl_list, list(summary(bkd_step_mdl)))
  
  idx <- idx + 1
}


