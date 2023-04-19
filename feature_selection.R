library(tidyverse)
library(readxl)
library(dplyr)
library(ggplot2)
library(MASS)
library(haven)
library(glmnet)


# NEED TO INSTALL
library(survey)

set.seed(1)


# setwd("~/Desktop/DSI Spring/Stats/Final Project")

####################################################
# TODO: Get regression for each variable in a loop #
####################################################

make.mtrx <- function (df){
  mtrx <- model.matrix(var ~ year+wrkstat+marital+age+race+educ+sex+born+income+region+partyid+relig+zodiac, df)
  return (mtrx)
}

load('cleaned_data.Rdata')

df_covariates <- df %>% dplyr::select(year, wrkstat, marital, age, race, educ, sex, born, income,
                                      region, partyid, relig, zodiac, vstrat, vpsu, wgt_comb)

df_variables <- df %>% dplyr::select(wealth_imp, sex_imp, parents_imp, educ_imp, hardWork_imp, rightPpl_imp,
                                     political_imp, race_imp, religion_imp)

stat_matrix <- matrix(NA, 9, 2)
fwd_mdl_list <- list()
bck_mdl_list <- list()
lasso_beta <- list()
lasso_lambda <- list()
best_lmbda_cv <- list()
idx <- 1

for (cname in colnames(df_variables)){
  stat_matrix[idx,1] <- cname
  idx <- idx + 1
}

idx <- 1
for (col in df_variables){

  # Create data we need for specific variable
  df_cov_temp <- df_covariates
  df_cov_temp$var <- col
  df_cov_temp <- df_cov_temp %>% drop_na()
  
  model.mtrx.all <- make.mtrx(df_cov_temp) 
  
  lm_lasso <- glmnet(model.mtrx.all[,-1],
                     df_cov_temp$var, 
                     weights = df_cov_temp$wgt_comb, 
                     family=quasibinomial(link="logit"), 
                     alpha = 1)
  
  # Training model
  lasso_mod_train <- cv.glmnet(model.mtrx.all[,-1],
                               df_cov_temp$var,  
                               weights = df_cov_temp$wgt_comb,
                               family=quasibinomial(link="logit"),
                               alpha = 1)
  
  # Select lambda that minimizes training MSE (from model using training data only)
  best_lmbda_cv <- append(best_lmbda_cv, lasso_mod_train$lambda.min)
  
  
  lasso_beta <- append(lasso_beta, lm_lasso$beta)
  lasso_lambda <- append(lasso_lambda, list(lm_lasso$lambda))
  
  
  # Create Survey design object to control weights
  surv.des <- svydesign(data = df_cov_temp, ids = ~vpsu, weights = ~wgt_comb, strata = ~vstrat, nest=TRUE)
  
  # t-test between year and variable
  ttest.svy <- svyttest(formula = var ~ year, design = surv.des)
  stat_matrix[idx, 2] <- ttest.svy$p.value
  
  # Forward and Backward stepwise regression
  #lm_full <- svyglm(formula = var ~ year+wrkstat+marital+age+race+educ+sex+born+income+region+partyid+relig+zodiac, 
  #                  design = surv.des, family=quasibinomial(link="logit"))
  #lm_none <- svyglm(formula = var~1, design = surv.des, family=quasibinomial(link="logit"), data = df_cov_temp)
  #
  #fwd_step_mdl <- stepAIC(object = lm_none, direction='forward', scope=list(upper=lm_full, lower=lm_none))
  #fwd_mdl_list <- append(fwd_mdl_list, list(summary(fwd_step_mdl)))
  #
  ## Using AIC as metric for feature selection
  #bkd_step_mdl <- stepAIC(lm_full, direction='backward')
  #bck_mdl_list <- append(bck_mdl_list, list(summary(bkd_step_mdl)))
  
  
  idx <- idx + 1
}

# Testing out some vizs for Lasso Regression
var.id = 6

bestlam <- best_lmbda_cv[[var.id]]

coef_passes <- data.frame(as.matrix(lasso_beta[[var.id]]))
colnames(coef_passes) <-  as.character(lasso_lambda[[var.id]])

coef_passes <- cbind(CoefName = rownames(coef_passes), coef_passes)
rownames(coef_passes) <- 1:nrow(coef_passes)

data_long_e <- coef_passes %>% # Apply pivot_longer function
  pivot_longer(colnames(coef_passes)[-1]) %>% 
  as.data.frame()

data_long_e$name <- as.numeric(data_long_e$name)

data_long_trunc <- data_long_e %>% filter(name >= bestlam) %>% filter(value != 0)

ggplot(data_long_trunc, aes(x=name, y=value, group=CoefName, color=CoefName)) +
  geom_line() + 
  geom_hline(aes(yintercept=0)) +
  geom_vline(aes(xintercept=bestlam)) +
  theme(legend.position='bottom', legend.key.size = unit(.4, 'cm')) + 
  labs(title = "Coefficient Values at Each Lambda", x = "Lambda")

data_long_trunc %>% filter(min(name) == name) %>% filter(value != 0) %>% arrange(desc(abs(value)))
