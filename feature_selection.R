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


df <- df %>% dplyr::select(race_imp, year, wrkstat, marital, age, race, educ, sex, born, income,
                            region, partyid, relig, zodiac, vstrat, vpsu, wgt_comb) %>% drop_na()

df <- df %>% filter(race != '.i:  Inapplicable')

surv.des <- svydesign(data = df, ids = ~vpsu, weights = ~wgt_comb, strata = ~vstrat, nest=TRUE)

# T-Tests between year and every variable

svyttest(formula = wealth_imp ~ year, design = surv.des)
svyttest(formula = sex_imp ~ year, design = surv.des)
svyttest(formula = parents_imp ~ year, design = surv.des)
svyttest(formula = educ_imp ~ year, design = surv.des)
svyttest(formula = hardWork_imp ~ year, design = surv.des)
svyttest(formula = rightPpl_imp ~ year, design = surv.des)
svyttest(formula = political_imp ~ year, design = surv.des)
svyttest(formula = race_imp ~ year, design = surv.des)
svyttest(formula = religion_imp ~ year, design = surv.des)

# (WIP) Forwards and Backwards Logistic Regression

educ_full <- svyglm(formula = educ_imp~year+wrkstat+marital+age+race+educ+sex+born+income
       +region+partyid+relig+zodiac, 
       design = surv.des, family=quasibinomial(link="logit"))

educ_none <- svyglm(formula = educ_imp~1, design = surv.des, family=quasibinomial(link="logit"), data = df)


# Using AIC as metric for feature selection
fwd_step_mdl <- stepAIC(object = educ_none, direction='forward', scope=list(upper=educ_full, lower=educ_none))
summary(fwd_step_mdl)

# Using AIC as metric for feature selection
bkd_step_edu <- stepAIC(educ_full, direction='backward')
summary(bkd_step_edu)


wealth_full <- svyglm(formula = wealth_imp~year+wrkstat+marital+age+race+educ+sex+born+income
                    +region+partyid+relig+zodiac, 
                    design = surv.des, family=quasibinomial(link="logit"))

wealth_none <- svyglm(formula = wealth_imp~1, design = surv.des, family=quasibinomial(link="logit"), data = df)


bkd_step_wlth <- stepAIC(wealth_full, direction='backward')
summary(bkd_step_wlth)

fwd_step_wlth <- stepAIC(object = wealth_none, direction='forward', scope=list(upper=wealth_full, lower=wealth_none))
summary(fwd_step_wlth)


race_full <- svyglm(formula = race_imp~year+wrkstat+marital+age+race+educ+sex+born+income
                      +region+partyid+relig+zodiac, 
                      design = surv.des, family=quasibinomial(link="logit"))

race_none <- svyglm(formula = race_imp~1, design = surv.des, family=quasibinomial(link="logit"), data = df)

bkd_step_race <- stepAIC(race_full, direction='backward')
summary(bkd_step_race)


# Using AIC as metric for feature selection
fwd_step_race <- stepAIC(object = race_none, direction='forward', scope=list(upper=race_full, lower=race_none))
summary(fwd_step_race)
