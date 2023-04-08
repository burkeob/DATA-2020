library(tidyverse)
library(readxl)
library(dplyr)
library(ggplot2)
library(haven)

# NEED TO INSTALL
library(survey)

# setwd("~/Desktop/DSI Spring/Stats/Final Project")

load('cleaned_data.Rdata')

colnames(df)[20:28]

surv.des <- svydesign(data = df, ids = ~1, weights = ~wgt_comb)

svyttest(formula = wealth_imp ~ year, design = surv.des)
svyttest(formula = sex_imp ~ year, design = surv.des)
svyttest(formula = parents_imp ~ year, design = surv.des)
svyttest(formula = educ_imp ~ year, design = surv.des)
svyttest(formula = hardWork_imp ~ year, design = surv.des)
svyttest(formula = rightPpl_imp ~ year, design = surv.des)
svyttest(formula = political_imp ~ year, design = surv.des)
svyttest(formula = race_imp ~ year, design = surv.des)
svyttest(formula = religion_imp ~ year, design = surv.des)

