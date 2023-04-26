# install.packages('svydiags')
library(tidyverse)
library(tidyr)
library(glmnet)
library(survey)
library(lme4)
library(svydiags)

load(file='cleaned_data.Rdata')
names <- c(2:3, 6:13)
df[,names] <- lapply(df[,names] , factor)
nums <- c(14,16)
df[,nums] <- lapply(df[,nums] , as.numeric)
df = subset(df, select = -c(wtssall,wtssnrps) )
df <- within(df, relig <- relevel(relig, ref = 'missing'))
df <- within(df, marital <- relevel(marital, ref ='missing'))
df <- within(df, partyid <- relevel(partyid, ref ='missing'))
df <- within(df, wrkstat <- relevel(wrkstat, ref ='missing'))
df <- within(df, income <- relevel(income, ref ='missing'))
df <- within(df, sex <- relevel(sex, ref ='missing'))

surv.des <- svydesign(data = df, ids = ~vpsu, weights = ~wgt_comb, strata = ~vstrat, nest=TRUE)


#separate models by year:

#extract significant coef
sig_coefs <- function(model) {
  coefs <- as.data.frame(summary(model)[13][[1]])
  coefs <- coefs %>% subset(coefs[4]<0.05)
  return(coefs)}

#compare coefs
compare_coefs <- function(model1,model2){
  coefs1 <- as.data.frame(summary(model1)[13][[1]])
  coefs2 <- as.data.frame(summary(model2)[13][[1]])
  # coefs1 <- sig_coefs(model1)
  # coefs2 <- sig_coefs(model2)
  rows = intersect(row.names(coefs2),row.names(coefs1))
  coefs1 <- coefs1 %>% filter(row.names(coefs1) %in% rows)
  coefs2 <- coefs2 %>% filter(row.names(coefs2) %in% rows)
  
  beta1 <- coefs1[[1]]
  beta2 <- coefs2[[1]]
  se1 <- coefs1[[4]]^2
  se2 <- coefs2[[4]]^2
  z <- abs(beta1-beta2)/sqrt(se1+se2)
  pvals <- setNames(pnorm(-z),rows)
  pvals <- as.data.frame(pvals)
  pvals['model1_estimate'] <- beta1
  pvals['model2_estimate'] <- beta2

  return(pvals %>% subset(pvals<0.05))
}

#binned residuals
binned.resids <- function (x, y, nclass=sqrt(length(x))){
  breaks.index <- floor(length(x)*(1:(nclass-1))/nclass)
  breaks <- c (-Inf, sort(x)[breaks.index], Inf)
  output <- NULL
  xbreaks <- NULL
  x.binned <- as.numeric (cut (x, breaks))
  for (i in 1:nclass){
    items <- (1:length(x))[x.binned==i]
    x.range <- range(x[items])
    xbar <- mean(x[items])
    ybar <- mean(y[items])
    n <- length(items)
    sdev <- sd(y[items])
    output <- rbind (output, c(xbar, ybar, n, x.range, 2*sdev/sqrt(n)))
  }
  colnames (output) <- c ("xbar", "ybar", "n", "x.lo", "x.hi", "2se")
  return (list (binned=output, xbreaks=xbreaks))
}




#logistic regression - religion ---------------------------------------
model_relig_1987 <- svyglm(religion_imp ~ wrkstat+marital+age+educ+sex+race+born+income+
                       region+partyid+relig+zodiac, design=surv.des, family='quasibinomial', subset=(year == 1987))

model_relig_2021 <- svyglm(religion_imp ~ wrkstat+marital+age+educ+sex+race+born+income+
                       region+partyid+relig+zodiac, design=surv.des, family='quasibinomial', subset=(year == 2021))

compare_coefs(model_relig_1987,model_relig_2021)

#binned residuals 

# pred_1987 <- model_1987 %>% predict(model_1987$data, type = "response")  #this errors for some reason
br <- as.data.frame(binned.resids (model_relig_1987$fitted.values, model_relig_1987$residuals, nclass=40)$binned)
p1 <- ggplot(br, aes(br[,1],br[,2])) + geom_point() + 
  geom_line(aes(br[,1],br[,6]), color='gray') + 
  geom_line(aes(br[,1],-br[,6]), color='gray') + geom_hline(aes(yintercept=0)) + 
  xlab('Estimated  response') + ylab('Average Residual') + 
  ggtitle('Binned Residual Plot- 1987')
p1

br <- as.data.frame(binned.resids (model_relig_2021$fitted.values, model_relig_2021$residuals, nclass=40)$binned)
p2 <- ggplot(br, aes(br[,1],br[,2])) + geom_point() + 
  geom_line(aes(br[,1],br[,6]), color='gray') + 
  geom_line(aes(br[,1],-br[,6]), color='gray') + geom_hline(aes(yintercept=0)) + 
  xlab('Estimated  response') + ylab('Average Residual') + 
  ggtitle('Binned Residual Plot - 2021')
p2


#logistic regression - wealth ---------------------------------------
model_wealth_1987 <- svyglm(wealth_imp ~ wrkstat+marital+age+educ+sex+race+born+income+
                             region+partyid+relig+zodiac, design=surv.des, family='quasibinomial', subset=(year == 1987))

model_wealth_2021 <- svyglm(wealth_imp ~ wrkstat+marital+age+educ+sex+race+born+income+
                             region+partyid+relig+zodiac, design=surv.des, family='quasibinomial', subset=(year == 2021))

compare_coefs(model_wealth_1987,model_wealth_2021)

#binned residuals 

br <- as.data.frame(binned.resids (model_wealth_1987$fitted.values, model_wealth_1987$residuals, nclass=40)$binned)
p1 <- ggplot(br, aes(br[,1],br[,2])) + geom_point() + 
  geom_line(aes(br[,1],br[,6]), color='gray') + 
  geom_line(aes(br[,1],-br[,6]), color='gray') + geom_hline(aes(yintercept=0)) + 
  xlab('Estimated  response') + ylab('Average Residual') + 
  ggtitle('Binned Residual Plot- 1987')
p1

br <- as.data.frame(binned.resids (model_wealth_2021$fitted.values, model_wealth_2021$residuals, nclass=40)$binned)
p2 <- ggplot(br, aes(br[,1],br[,2])) + geom_point() + 
  geom_line(aes(br[,1],br[,6]), color='gray') + 
  geom_line(aes(br[,1],-br[,6]), color='gray') + geom_hline(aes(yintercept=0)) + 
  xlab('Estimated  response') + ylab('Average Residual') + 
  ggtitle('Binned Residual Plot - 2021')
p2


#logistic regression - sex ---------------------------------------
model_sex_1987 <- svyglm(sex_imp ~ wrkstat+marital+age+educ+sex+race+born+income+
                              region+partyid+relig+zodiac, design=surv.des, family='quasibinomial', subset=(year == 1987))

model_sex_2021 <- svyglm(sex_imp ~ wrkstat+marital+age+educ+sex+race+born+income+
                              region+partyid+relig+zodiac, design=surv.des, family='quasibinomial', subset=(year == 2021))

compare_coefs(model_sex_1987,model_sex_2021)

#binned residuals 

br <- as.data.frame(binned.resids (model_sex_1987$fitted.values, model_sex_1987$residuals, nclass=40)$binned)
p1 <- ggplot(br, aes(br[,1],br[,2])) + geom_point() + 
  geom_line(aes(br[,1],br[,6]), color='gray') + 
  geom_line(aes(br[,1],-br[,6]), color='gray') + geom_hline(aes(yintercept=0)) + 
  xlab('Estimated  response') + ylab('Average Residual') + 
  ggtitle('Binned Residual Plot- 1987')
p1

br <- as.data.frame(binned.resids (model_sex_2021$fitted.values, model_sex_2021$residuals, nclass=40)$binned)
p2 <- ggplot(br, aes(br[,1],br[,2])) + geom_point() + 
  geom_line(aes(br[,1],br[,6]), color='gray') + 
  geom_line(aes(br[,1],-br[,6]), color='gray') + geom_hline(aes(yintercept=0)) + 
  xlab('Estimated  response') + ylab('Average Residual') + 
  ggtitle('Binned Residual Plot - 2021')
p2




#logistic regression - combined years ---------------------------------------

model_wealth <- svyglm(wealth_imp ~ wrkstat+marital+age+educ+sex+race+born+income+
                          region+partyid+relig+zodiac, design=surv.des, family='quasibinomial')

model_relig <- svyglm(religion_imp ~ wrkstat+marital+age+educ+sex+race+born+income+
                          region+partyid+relig+zodiac, design=surv.des, family='quasibinomial')

model_race <- svyglm(race_imp ~ wrkstat+marital+age+educ+sex+race+born+income+
                                        region+partyid+relig+zodiac, design=surv.des, family='quasibinomial')

model_polit <- svyglm(political_imp ~ wrkstat+marital+age+educ+sex+race+born+income+
                          region+partyid+relig+zodiac, design=surv.des, family='quasibinomial')

model_sex <- svyglm(sex_imp ~ wrkstat+marital+age+educ+sex+race+born+income+
                        region+partyid+relig+zodiac, design=surv.des, family='quasibinomial')


#df for accuracy
df_relig <- df %>% drop_na(c(religion_imp,wrkstat,marital,age,educ,sex,race,born,income,region,partyid,relig,zodiac))
df_wealth <- df %>% drop_na(c(wealth_imp,wrkstat,marital,age,educ,sex,race,born,income,region,partyid,relig,zodiac))
df_parents <- df %>% drop_na(c(parents_imp,wrkstat,marital,age,educ,sex,race,born,income,region,partyid,relig,zodiac))
df_educ <- df %>% drop_na(c(educ_imp, wrkstat,marital,age,educ,sex,race,born,income,region,partyid,relig,zodiac))
df_work <- df %>% drop_na(c(hardWork_imp, wrkstat,marital,age,educ,sex,race,born,income,region,partyid,relig,zodiac))
df_ppl <- df %>% drop_na(c(rightPpl_imp, wrkstat,marital,age,educ,sex,race,born,income,region,partyid,relig,zodiac))
df_politics <- df %>% drop_na(c(political_imp,wrkstat,marital,age,educ,sex,race,born,income,region,partyid,relig,zodiac))
df_race <- df %>% drop_na(c(race_imp,wrkstat,marital,age,educ,sex,race,born,income,region,partyid,relig,zodiac))
df_sex <- df %>% drop_na(c(sex_imp,wrkstat,marital,age,educ,sex,race,born,income,region,partyid,relig,zodiac))

#accuracy calculations and visualizations
acc <- function(model,x,y){
  pred <- predict(model, x, type='response')
  pred[pred>0.5] <- 1
  pred[pred<1] <- 0
  accuracy = mean(abs(pred - y))
  return(accuracy)
}

accuracy_relig <- acc(model_relig,df_relig, df_relig$religion_imp)
accuracy_wealth <- acc(model_wealth,df_wealth, df_wealth$wealth_imp)
accuracy_race <- acc(model_race,df_race, df_race$race_imp)
accuracy_sex <- acc(model_sex,df_sex, df_sex$sex_imp)
accuracy_polit <- acc(model_polit,df_politics, df_politics$political_imp)

accuracy_relig 
accuracy_wealth
accuracy_race
accuracy_sex
accuracy_polit



# #mixed effects model - religion-------------------------------------------------------

# #no strat variable
mixed_relig <- glmer(formula = religion_imp ~ wrkstat+marital+age+educ+sex+race+born+income+
                     region+partyid+relig+zodiac + (1+wrkstat+marital+age+educ+sex+race+born+income+
                     region+partyid+relig+zodiac|year), data = df_relig, weights = wgt_comb, family = 'binomial')


#VIF --------------------------------------------------------------

#this is bad - don't use
#can't handle missing values and data must align between model and X
temp_data <-df[,c(1:13,15,17,25)]
temp_data <- temp_data %>% drop_na()
survcomp.des <- svydesign(data = df, ids = ~vpsu, weights = ~wgt_comb, strata = ~vstrat, nest=TRUE)

model_1987_comp <- svyglm(religion_imp ~ wrkstat+marital+age+educ+sex+race+born+income+
                            region+partyid+relig+zodiac, design=survcomp.des, family='quasibinomial', subset=(year == 1987))

model_2021_comp <- svyglm(religion_imp ~ wrkstat+marital+age+educ+sex+race+born+income+
                            region+partyid+relig+zodiac, design=survcomp.des, family='quasibinomial', subset=(year == 2021))

model_ary <- model.matrix(model_1987_comp$formula, data = model_1987_comp$data)
X <- model_ary[, -2]
w <- model_1987_comp$weights
vif_1987 <- svyvif(model_1987_comp, X, w)

model_ary <- model.matrix(model_2021_comp$formula, data = model_2021_comp$data)
X <- model_ary[, -2]
w <- model_2021_comp$weights
vif_2021 <- svyvif(model_2021_comp, X, w)




