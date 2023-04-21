library(tidyverse)
library(readxl)
library(dplyr)
library(ggplot2)
library(MASS)
library(haven)
library(glmnet)

library(survey)


load('cleaned_data.Rdata')


## Testing group differences
df_covariates <- df %>% dplyr::select(wrkstat, marital, age, race, educ, sex, born, income,
                                      region, partyid, relig, zodiac)

df_variables <- df %>% dplyr::select(wealth_imp, sex_imp, parents_imp, educ_imp, hardWork_imp, rightPpl_imp,
                                     political_imp, race_imp, religion_imp)

## Loop through questions

for (q in colnames(df_variables)){
  
  
  for (c in colnames(df_covariates)){
    
    df_cov_temp <- df %>% dplyr::select(q, c, year, vpsu,wgt_comb,vstrat)
    df_cov_temp <- df_cov_temp %>%
      filter(df[[c]] != ".i:  Inapplicable") %>% drop_na()
    
    surv.des <- svydesign(data = df_cov_temp, ids = ~vpsu, weights = ~wgt_comb, strata = ~vstrat, nest=TRUE)
    
    res <- svyby(~get(q), ~ get(c) + year, design = surv.des, FUN = svymean)
    
    names(res)[1] <- c
    names(res)[3] <- q
    
    qq <- res[[q]]
    cc <- res[[c]]
    
    g <- ggplot(res, aes(x = cc, y = qq)) +
          geom_bar(stat = "identity", position = position_dodge()) +
          geom_errorbar(aes(ymin = qq - 1.96 * se/sqrt(3), ymax = qq + 1.96 * se/sqrt(3)), position = position_dodge(width = 0.9), width = 0.2) +
          facet_wrap(~ year,) +
          xlab(c) +
          labs(title = paste0(q, ", ", c))
    
    
    
    ggsave(paste0("Group Differences/", q, "/", c, "test.png"))

  }
  
}


