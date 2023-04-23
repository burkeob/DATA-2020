install.packages('svydiags')
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
df_relig <- df %>% drop_na(religion_imp)

#logistic regression - religion ---------------------------------------

model_1987 <- svyglm(religion_imp ~ wrkstat+marital+age+educ+sex+race+born+income+
                    region+partyid+relig+zodiac, design=surv.des, family='quasibinomial', subset=(year == 1987))
summary(model_1987)


model_2021 <- svyglm(religion_imp ~ wrkstat+marital+age+educ+sex+race+born+income+
                    region+partyid+relig+zodiac, design=surv.des, family='quasibinomial', subset=(year == 2021))
summary(model_2021)


#binned residuals ------------------------------------------------------

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

# res_1987 <- svystdres(model_1987)

# pred_1987 <- model_1987 %>% predict(model_1987$data, type = "response")  #this errors for some reason
br <- as.data.frame(binned.resids (model_1987$fitted.values, model_1987$residuals, nclass=40)$binned)
p1 <- ggplot(br, aes(br[,1],br[,2])) + geom_point() + 
  geom_line(aes(br[,1],br[,6]), color='gray') + 
  geom_line(aes(br[,1],-br[,6]), color='gray') + geom_hline(aes(yintercept=0)) + 
  xlab('Estimated  response') + ylab('Average Residual') + 
  ggtitle('Binned Residual Plot- 1987')
p1

br <- as.data.frame(binned.resids (model_2021$fitted.values, model_2021$residuals, nclass=40)$binned)
p2 <- ggplot(br, aes(br[,1],br[,2])) + geom_point() + 
  geom_line(aes(br[,1],br[,6]), color='gray') + 
  geom_line(aes(br[,1],-br[,6]), color='gray') + geom_hline(aes(yintercept=0)) + 
  xlab('Estimated  response') + ylab('Average Residual') + 
  ggtitle('Binned Residual Plot - 2021')
p2


#VIF --------------------------------------------------------------

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




# #mixed effects model - religion
# #no strat variable
# mixed_relig <- glmer(formula = religion_imp ~ wrkstat+marital+age+educ+sex+race+born+income+
#                      region+partyid+relig+zodiac + (1|year), data = df_relig, weights = wgt_comb, family = 'binomial')



