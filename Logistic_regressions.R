# install.packages('ggthemes')
library(tidyverse)
library(tidyr)
library(glmnet)
library(survey)
library(lme4)
library(ggthemes)
library(caret)
options(scipen=1)

load(file='cleaned_data.Rdata')
names <- c(2:3, 6:13)
df[,names] <- lapply(df[,names] , factor)
nums <- c(14,16)
df[,nums] <- lapply(df[,nums] , as.numeric)
df = subset(df, select = -c(wtssall,wtssnrps) )
df <- within(df, relig <- relevel(relig, ref = 'None'))
df <- within(df, marital <- relevel(marital, ref ='Never married'))
df <- within(df, partyid <- relevel(partyid, ref ='Independent'))
df <- within(df, wrkstat <- relevel(wrkstat, ref ='Working full time'))
df <- within(df, income <- relevel(income, ref ='$25,000 or more'))
df <- within(df, sex <- relevel(sex, ref ='MALE'))
df <- within(df, born <- relevel(born, ref ='YES'))
df <- within(df, race <- relevel(race, ref ='White'))
levels(df$wrkstat)[levels(df$wrkstat)=='With a job, but not at work because of temporary illness, vacation, strike'] <- 'FMLA,vacation,strike'

surv.des <- svydesign(data = df, 
                          ids = ~vpsu, 
                          weights = ~wgt_comb, 
                          strata = ~vstrat, 
                          nest=TRUE)


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
  se1 <- coefs1[[2]]^2
  se2 <- coefs2[[2]]^2
  z <- abs(beta1-beta2)/sqrt(se1+se2)
  pvals <- setNames(pnorm(-z),rows)
  
  # pvals <- as.data.frame(pvals)
  # pvals['Estimate 1987'] <- beta1
  # pvals['p-value 1987'] <- coefs1[[4]]
  # pvals['SE 1987'] <- coefs1[[2]]
  # pvals['Estimate 2021'] <- beta2
  # pvals['p-value 2021'] <- coefs2[[4]]
  # pvals['SE 2021'] <- coefs2[[2]]
  # 
  # return(pvals %>% subset(pvals<0.05))
  
  #for plotting
  pvals <- signif(pvals,2)       
  pvals <- as.data.frame(pvals)
  diff_beta <- beta1-beta2
  pvals['Variable'] <- row.names(pvals)
  pvals["difference"] <- diff_beta
  pvals['direction'] <-"Increase from 1987"
  pvals[pvals$difference<0,"direction"] <- "Decrease from 1987"
  # pvals["difference"] <- abs(diff_beta)
  pvals["magnitude"] <- abs(diff_beta)
  pvals <- arrange(pvals,desc(magnitude))

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
                       region+partyid+relig, design=surv.des, family='quasibinomial', subset=(year == 1987))

model_relig_2021 <- svyglm(religion_imp ~ wrkstat+marital+age+educ+sex+race+born+income+
                       region+partyid+relig, design=surv.des, family='quasibinomial', subset=(year == 2021))

base_relig_1987 <- svyglm(religion_imp ~ 1, design=surv.des, family='quasibinomial', subset=(year == 1987))

base_relig_2021 <- svyglm(religion_imp ~ 1, design=surv.des, family='quasibinomial', subset=(year == 2021))


summary(model_relig_1987)$deviance 
summary(base_relig_1987)$deviance
summary(model_relig_2021)$deviance 
summary(base_relig_2021)$deviance

acc(model_relig_1987,subset(df_relig,year==1987),subset(df_relig,year==1987)$religion_imp)
acc(model_relig_2021,subset(df_relig,year==2021),subset(df_relig,year==2021)$religion_imp)


temp <- as.data.frame(coefs_relig <- compare_coefs(model_relig_1987,model_relig_2021))

temp <- temp %>% 
  mutate(direction = factor(direction))

p <-  ggplot(filter(temp,Variable!='(Intercept)'), aes(x=difference, y=fct_reorder(Variable,magnitude), fill=direction)) + geom_col() +
  labs(x="Estimate", y="Covariate", fill="Direction of Change") + scale_fill_manual(values=c('#84a98c','#9f86c0')) +
  ggtitle('Importance of Religion: Coefficient Change')  + 
  geom_text(aes(label=pvals),size=3.5, nudge_x=3.1) + theme_few() + 
  theme(axis.text=element_text(size=12),axis.title=element_text(size=14)) + 
  xlim(min(temp$difference),max(temp$difference)+5)

p

ggsave("religion_coefs.png")

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
                             region+partyid+relig, design=surv.des, family='quasibinomial', subset=(year == 1987))

model_wealth_2021 <- svyglm(wealth_imp ~ wrkstat+marital+age+educ+sex+race+born+income+
                             region+partyid+relig, design=surv.des, family='quasibinomial', subset=(year == 2021))

base_wealth_1987 <- svyglm(wealth_imp ~ 1, design=surv.des, family='quasibinomial', subset=(year == 1987))

base_wealth_2021 <- svyglm(wealth_imp ~ 1, design=surv.des, family='quasibinomial', subset=(year == 2021))


summary(model_wealth_1987)$deviance 
summary(base_wealth_1987)$deviance
summary(model_wealth_2021)$deviance 
summary(base_wealth_2021)$deviance

acc(model_wealth_1987,subset(df_wealth,year==1987),subset(df_wealth,year==1987)$wealth_imp)
acc(model_wealth_2021,subset(df_wealth,year==2021),subset(df_wealth,year==2021)$wealth_imp)

temp <- as.data.frame(coefs_wealth <- compare_coefs(model_wealth_1987,model_wealth_2021))

temp <- temp %>% 
  mutate(direction = factor(direction))

p <-  ggplot(filter(temp,Variable!='(Intercept)'), aes(x=difference, y=fct_reorder(Variable,magnitude), fill=direction)) + geom_col() +
  labs(x="Estimate", y="Covariate", fill="Direction of Change") + scale_fill_manual(values=c('#84a98c','#9f86c0')) +
  ggtitle('Importance of Wealth: Coefficient Change')  + geom_text(aes(label=pvals),size=3.5, nudge_x=1) + 
  theme_few() + xlim(min(temp$difference),max(temp$difference)+1) +
  theme(axis.text=element_text(size=12),axis.title=element_text(size=14)) 

p

ggsave("wealth_coefs.png")

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



#logistic regression - race---------------------------------------
model_race_1987 <- svyglm(race_imp ~ wrkstat+marital+age+educ+sex+race+born+income+
                           region+partyid+relig, design=surv.des, family='quasibinomial', subset=(year == 1987))

model_race_2021 <- svyglm(race_imp ~ wrkstat+marital+age+educ+sex+race+born+income+
                           region+partyid+relig, design=surv.des, family='quasibinomial', subset=(year == 2021))

base_race_1987 <- svyglm(race_imp ~ 1, design=surv.des, family='quasibinomial', subset=(year == 1987))

base_race_2021 <- svyglm(race_imp ~ 1, design=surv.des, family='quasibinomial', subset=(year == 2021))

summary(model_race_1987)$deviance
summary(base_race_1987)$deviance
summary(model_race_2021)$deviance
summary(base_race_2021)$deviance

acc(model_race_1987,subset(df_race,year==1987),subset(df_race,year==1987)$race_imp)
acc(model_race_2021,subset(df_race,year==2021),subset(df_race,year==2021)$race_imp)


temp <- as.data.frame(coefs_race <- compare_coefs(model_race_1987,model_race_2021))

temp <- temp %>% 
  mutate(direction = factor(direction))

p <-  ggplot(filter(temp,Variable!='(Intercept)'), aes(x=difference, y=fct_reorder(Variable,magnitude), fill=direction)) + geom_col() +
  labs(x="Estimate", y="Covariate", fill="Direction of Change") + scale_fill_manual(values=c('#84a98c','#9f86c0')) +
  ggtitle('Importance of Race: Coefficient Change') + geom_text(aes(label=pvals),size=3.5, nudge_x=2) + theme_few() + 
  theme(axis.text=element_text(size=12),axis.title=element_text(size=14)) + 
  xlim(min(temp$difference),max(temp$difference)+5)

p

ggsave("race_coefs.png")

#binned residuals 

br <- as.data.frame(binned.resids (model_race_1987$fitted.values, model_race_1987$residuals, nclass=40)$binned)
p1 <- ggplot(br, aes(br[,1],br[,2])) + geom_point() + 
  geom_line(aes(br[,1],br[,6]), color='gray') + 
  geom_line(aes(br[,1],-br[,6]), color='gray') + geom_hline(aes(yintercept=0)) + 
  xlab('Estimated  response') + ylab('Average Residual') + 
  ggtitle('Binned Residual Plot- 1987')
p1

br <- as.data.frame(binned.resids (model_race_2021$fitted.values, model_race_2021$residuals, nclass=40)$binned)
p2 <- ggplot(br, aes(br[,1],br[,2])) + geom_point() + 
  geom_line(aes(br[,1],br[,6]), color='gray') + 
  geom_line(aes(br[,1],-br[,6]), color='gray') + geom_hline(aes(yintercept=0)) + 
  xlab('Estimated  response') + ylab('Average Residual') + 
  ggtitle('Binned Residual Plot - 2021')
p2


#logistic regression - sex ---------------------------------------
model_sex_1987 <- svyglm(sex_imp ~ wrkstat+marital+age+educ+sex+race+born+income+
                           region+partyid+relig, design=surv.des, family='quasibinomial', subset=(year == 1987))

model_sex_2021 <- svyglm(sex_imp ~ wrkstat+marital+age+educ+sex+race+born+income+
                           region+partyid+relig, design=surv.des, family='quasibinomial', subset=(year == 2021))



#logistic regression - politics ---------------------------------------
model_polit_1987 <- svyglm(political_imp ~ wrkstat+marital+age+educ+sex+race+born+income+
                           region+partyid+relig, design=surv.des, family='quasibinomial', subset=(year == 1987))

model_polit_2021 <- svyglm(political_imp ~ wrkstat+marital+age+educ+sex+race+born+income+
                           region+partyid+relig, design=surv.des, family='quasibinomial', subset=(year == 2021))




# error rate --------------------------------------------------------------

#df for accuracy
df_relig <- df %>% subset(select=c(religion_imp,wrkstat,marital,age,educ,sex,race,born,income,region,partyid,relig,year)) %>% drop_na()
df_wealth <- df %>% subset(select=c(wealth_imp,wrkstat,marital,age,educ,sex,race,born,income,region,partyid,relig,year))  %>% drop_na()
df_politics <- df %>% subset(select=c(political_imp,wrkstat,marital,age,educ,sex,race,born,income,region,partyid,relig,year))  %>% drop_na()
df_race <- df %>% subset(select=c(race_imp,wrkstat,marital,age,educ,sex,race,born,income,region,partyid,relig,year))  %>% drop_na()
df_sex <- df %>% subset(select=c(sex_imp,wrkstat,marital,age,educ,sex,race,born,income,region,partyid,relig,year)) %>% drop_na()

#error rate calculations
acc <- function(model,x,y){
  pred <- predict(model, x, type='response')
  pred[pred<=0.5] <- 0
  pred[pred>0.5] <- 1
  err = mean(abs(pred - y))
  return(err)
}


#plots
temp <- as.data.frame(compare_coefs(model_race_1987,model_race_2021))

temp <- temp %>% 
  mutate(Year = factor(Year))

p <-  ggplot(temp, aes(x=Estimate, y=Variable, color=Year)) + 
  geom_point(position = position_dodge(width=.5), size=5) + 
  geom_errorbarh(aes(xmin=(Estimate+SE), xmax=(Estimate-SE)), position=position_dodge(width=.5), height=0,  size=2) + 
  labs(x="Estimate", y="Variable", color="Year") + scale_color_manual(values=c('#84a98c','#9f86c0')) +
  ggtitle('Race Model')

p

ggsave("race_coefs.png")

