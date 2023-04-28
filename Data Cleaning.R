library(tidyverse)
library(readxl)

# loading in the cleaned data
wd <- "~/Desktop/DSI Spring/Stats/DATA-2020/"

setwd(wd)

df <- read_excel("GSS.xlsx") |>
  filter(year == 1987 | (year == 2021 & ballot == "Ballot c") | (year == 2021 & ballot == "Ballot b")) |> 
  select(-c(ballot, id_))

# make combined weight column
df <- df |>
  mutate(wgt_comb = ifelse(year == 1987, wtssall, wtssnrps))

# replace outcome vars with 1 if agree, 0 if disagree, and NA if missing or unsure
df$wealth_imp = ifelse(df$opwlth %in% c("Very important", "Fairly important", "Essential"), 1, 0)
df$wealth_imp = ifelse((df$opwlth %in% c("Not important at all", "Not very important") | df$wealth_imp == 1), df$wealth_imp, NA)

df$parents_imp = ifelse(df$oppared %in% c("Very important", "Fairly important", "Essential"), 1, 0)
df$parents_imp = ifelse((df$oppared %in% c("Not important at all", "Not very important") | df$parents_imp == 1), df$parents_imp, NA)

df$educ_imp = ifelse(df$opeduc %in% c("Very important", "Fairly important", "Essential"), 1, 0)
df$educ_imp = ifelse((df$opeduc %in% c("Not important at all", "Not very important") | df$educ_imp == 1), df$educ_imp, NA)

df$hardWork_imp = ifelse(df$ophrdwrk %in% c("Very important", "Fairly important", "Essential"), 1, 0)
df$hardWork_imp = ifelse((df$ophrdwrk %in% c("Not important at all", "Not very important") | df$hardWork_imp == 1), df$hardWork_imp, NA)

df$rightPpl_imp = ifelse(df$opknow %in% c("Very important", "Fairly important", "Essential"), 1, 0)
df$rightPpl_imp = ifelse((df$opknow %in% c("Not important at all", "Not very important") | df$rightPpl_imp == 1), df$rightPpl_imp, NA)

df$political_imp = ifelse(df$opclout %in% c("Very important", "Fairly important", "Essential"), 1, 0)
df$political_imp = ifelse((df$opclout %in% c("Not important at all", "Not very important") | df$political_imp == 1), df$political_imp, NA)

df$race_imp = ifelse(df$oprace %in% c("Very important", "Fairly important", "Essential"), 1, 0)
df$race_imp = ifelse((df$oprace %in% c("Not important at all", "Not very important") | df$race_imp == 1), df$race_imp, NA)

df$religion_imp = ifelse(df$oprelig %in% c("Very important", "Fairly important", "Essential"), 1, 0)
df$religion_imp = ifelse((df$oprelig %in% c("Not important at all", "Not very important") | df$religion_imp == 1), df$religion_imp, NA)

df$sex_imp = ifelse(df$opsex %in% c("Very important", "Fairly important", "Essential"), 1, 0)
df$sex_imp = ifelse((df$opsex %in% c("Not important at all", "Not very important") | df$sex_imp == 1), df$sex_imp, NA)

# Drop the orignial outcomes vars - can add them back in if we want more detail
df <- df |>
  select(-c(opsex,oprelig,oprace,opclout,opknow,ophrdwrk,opeduc,oppared,opwlth))


# Clean the feature vars

# Start with categorical - can keep missing values as their own category
df <- df |>
  mutate(year = as.factor(year))

df <- df |>
  mutate(wrkstat = ifelse(wrkstat %in% c(".n:  No answer" , ".s:  Skipped on Web"), "missing", wrkstat)) |>
  mutate(wrkstat = as.factor(wrkstat))

df <- df |>
  mutate(marital = ifelse(marital %in% c(".n:  No answer" ,
                                         ".s:  Skipped on Web", 
                                         ".d:  Do not Know/Cannot Choose"), "missing", marital)) |>
  mutate(marital = as.factor(marital))

df <- df |>
  mutate(sex = ifelse(sex %in% c(".n:  No answer" ,
                                  ".s:  Skipped on Web", 
                                  ".i:  Inapplicable",
                                  ".d:  Do not Know/Cannot Choose"), "missing", sex)) |>
  mutate(sex = as.factor(sex))


# Not really useful - lets just do above and below 25,000

df <- df |>
  mutate(income = ifelse(income %in% c(".n:  No answer" ,
                                 ".s:  Skipped on Web", 
                                 ".i:  Inapplicable",
                                 ".r:  Refused", 
                                 ".d:  Do not Know/Cannot Choose"), "missing", ifelse(
                                   income == "$25,000 or more", "$25,000 or more", "Less than $25,000"
                                 ))) |>
  mutate(income = as.factor(income))


df <- df |>
  mutate(region = as.factor(region))

# We may want to undo this, as a first pass I saw that strong demo was significant
df <- df |>
  mutate(partyid = ifelse(partyid %in% c(".n:  No answer" ,
                                 ".s:  Skipped on Web", 
                                 ".i:  Inapplicable",
                                 ".d:  Do not Know/Cannot Choose"), "missing", ifelse(
                                   str_detect(partyid, "Ind"), "Independent",
                                   ifelse(str_detect(partyid, "demo"), "Democrat", "Republican")
                                 ))) |>
  mutate(partyid = as.factor(partyid))


df <- df |>
  mutate(relig = ifelse(relig %in% c(".n:  No answer" ,
                                         ".s:  Skipped on Web", 
                                         ".d:  Do not Know/Cannot Choose"), "missing", relig)) |>
  mutate(relig = as.factor(relig))

# whether this person was born in this country
df <- df |>
  mutate(born = ifelse(born %in% c("YES", "NO"), born, "missing")) |>
  mutate(born = as.factor(born))

df <- df |>
  mutate(income = as.factor(income))

levels(df$relig)

# Numerical - will have to do some sort of imputation to keep these
df <- df |>
  mutate(age = as.numeric(age)) |>
  mutate(age = ifelse(age < 0 , NA, age))

# educ is years of schooling
df <- df |>
  mutate(educ = as.numeric(educ)) |>
  mutate(educ = ifelse(educ < 0 , NA, educ))

# Target vars
# c(wealth_imp, parents_imp, educ_imp, hardWork_imp, rightPpl_imp, political_imp, race_imp, religion_imp, sex_imp)

# weight vars - don't include in any regressions or whatnot
# also don't include the id_ var
# wtssall, vstrat, vpsu, wgt_comb, oversamp, wtssnrps

save(df, file = "cleaned_data.Rdata")
load(file = "cleaned_data.Rdata")
