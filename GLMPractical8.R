

require(readstata13)
require(GGally)
require(tidyverse)

#### Data load ####
setwd("~/Dropbox/LSHTM/MedicalStatistics/2.Term2/GLM/Session8")
d <- read.dta13("oesophageal_data-1.dta", nonint.factors = TRUE)

#### Exercise 1 ####

# No missing data
table(complete.cases(d))

ggpairs(d, columns = 1:4) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

# Load function
source("http://pcwww.liv.ac.uk/~william/R/crosstab.r")

with(d,table(age_group,case))
crosstab(d, row.vars = "age_group", col.vars = "case", type = "r")

with(d,table(case,tobacco_group))
crosstab(d, row.vars = "tobacco_group", col.vars = "case", type = "r")

with(d,table(case,alcohol_grp))
crosstab(d, row.vars = "alcohol_grp", col.vars = "case", type = "r")


#### Exercise2 ####

d %>%
  mutate(tobacco2 = ifelse(tobacco_group != "None",1,0)) %>%
  group_by(tobacco2) %>%
  with(table(case,tobacco2))

# Odds ratio is 10.38708
255*191/(9*521)

# 95% CI is (5.23537,20.60816)
255*191/(9*521)*exp(1.96*sqrt(1/255 + 1/521 + 1/9 + 1/191)) # upper
255*191/(9*521)/exp(1.96*sqrt(1/255 + 1/521 + 1/9 + 1/191)) # lower


#### Exercise3 ####
fit1 <- d %>%
  mutate(tobacco2 = ifelse(tobacco_group != "None",1,0)) %>%
  glm(., formula = tobacco2~case, family = binomial(link="logit"))
summary(fit1)

#exponentiated results
cbind(exp(summary(fit1)$coefficients)[,1],
summary(fit1)$coefficients[,-1])


fit2 <- d %>%
  mutate(tobacco2 = ifelse(tobacco_group != "None",1,0)) %>%
  glm(., formula = case~tobacco2, family = binomial(link="logit"))
summary(fit2)

cbind(exp(summary(fit2)$coefficients)[,1],
      summary(fit2)$coefficients[,-1])

# The coefficients, standard errorsm z-values are identical.


#### Exercise4 ####

# table produces array
d2 <- d %>%
  mutate(tobacco2 = ifelse(tobacco_group != "None",1,0)) 
table(exposure=d2$tobacco2,outcome=d2$case, d2$alcohol_grp) 
# table with piping does not work for 3-dimension tables

# Or ftable() produces all in one
ftable <- ftable(data.frame(alcohol=d2$alcohol_grp,
                            tobacco=d2$tobacco2,
                            case=d2$case))

# If persist in piping
d %>%
  mutate(tobacco2 = ifelse(tobacco_group != "None",1,0)) %>%
  group_by(alcohol_grp, tobacco2) %>%
  summarize(total = n(), case = sum(case), noncase= total-case)

# Alternatively
d %>%
  mutate(tobacco2 = ifelse(tobacco_group != "None",1,0)) %>%
  crosstab(row.vars = c("alcohol_grp", "tobacco2"), col.vars = "case", 
           type = "f", addmargins = FALSE) 


##  require(epiDisplay) ##
# https://rdrr.io/cran/epiDisplay/man/mhor.html
# This one is my preference.

dt <- table(exposure=d2$tobacco2,outcome=d2$case, d2$alcohol_grp)

# This gives you stratum-specific ORs and adjusted estimates
#including their 95% CIs, p-values
mhor(mhtable = dt,
     design = "case-control")

#Stratified analysis by  Var3 
#OR lower lim. upper lim.  P value
#Var3 0-39 grams per day      Inf       4.50        Inf 2.36e-06
#Var3 40-79 grams per day    9.52       2.99       48.6 1.01e-06
#Var3 80-119 grams per day   3.04       1.02       11.1 4.38e-02
#Var3 120+ grams per day    12.41       1.26      623.2 1.26e-02
#M-H combined                8.59       4.18       17.6 9.30e-12

#M-H Chi2(1) = 46.47 , P value = 0 


## {mantelhaen.test} ## 
# In order to use MH test using {mantelhaen.test}
# you need to prepare 3-dimensional dataset in array 
# Three steps exist

#1. 3-dimension cross-table; the first argument is a stratify variable

ftable <- ftable(data.frame(alcohol=d2$alcohol_grp,
                               tobacco=d2$tobacco2,
                               case=d2$case))

#2. Trasform the ftable into a 2 x 2 x 3 array
dt <- table(exposure=d2$tobacco2,outcome=d2$case, d2$alcohol_grp) 

# 3. MH test
mantelhaen.test(dt, exact=FALSE)
#Mantel-Haenszel X-squared = 45.063, df = 1, p-value = 1.908e-11
#alternative hypothesis: true common odds ratio is not equal to 1
#95 percent confidence interval:
#  4.18195 17.63310
#sample estimates:
#  common odds ratio 
#8.587244 

# There might be some confouncding by alcohol consumption.


## require(EpiStats) ##
# https://cran.r-project.org/web/packages/EpiStats/vignettes/EpiStats.pdf
# Easy to use but the layout of outputs is a bit lame
EpiStats::cc(d2,case, tobacco2)

EpiStats::CCInter(d2,cases="case",exposure="tobacco2", by="alcohol_grp")


## require(epiR) ##
# https://cran.r-project.org/web/packages/epiR/epiR.pdf
# it is annoying that I need to permute matrix... less recommended in my opinions.
ftab2 <- as.matrix(ftable(data.frame(tobacco=d2$tobacco2,case=d2$case)))

# permute matrix as required for epi.2by2
ftab2[,c(1,2)] <- ftab2[,c(2,1)]
ftab2[c(1,2),] <- ftab2[c(2,1),]
colnames(ftab2) <- c("case", "noncase")
rownames(ftab2) <- c("smoker", "nonsmoker")

# 2 by 2 table and various outputs
epi.2by2(ftab2, method = "case.control" , outcome = "as.columns")


#### Exercise5 ####
fit3 <- glm(d2, formula = case~tobacco2+alcohol_grp, family = binomial(link = "logit"))
summary(fit3)

cbind(exp(summary(fit3)$coefficients)[,1],
      summary(fit3)$coefficients[,-1])

fit4 <- glm(d2, formula = tobacco2~case+alcohol_grp, family = binomial(link = "logit"))
summary(fit4)

cbind(exp(summary(fit4)$coefficients)[,1],
      summary(fit4)$coefficients[,-1])

# Again, the coefficients and other outputs for case and tobacco2 are identical.


#### Exercise6 ####
fit5 <- glm(d2, formula = case~tobacco2+alcohol_grp + as.factor(age_group), family = binomial(link = "logit"))
summary(fit5)

cbind(exp(summary(fit5)$coefficients)[,1],
      summary(fit5)$coefficients[,-1])

fit6 <- glm(d2, formula = tobacco2~case+alcohol_grp + as.factor(age_group), family = binomial(link = "logit"))
summary(fit6)

cbind(exp(summary(fit6)$coefficients)[,1],
      summary(fit6)$coefficients[,-1])

# The coefficients for case and tobacco2 did change.
# In actuality, do regress case on exposure.

### Exercise 7 ###
# similar exercise so omitted

# when categorical variables are used, confouding is not fully adjusted
# advantage of categorizing is more intepretable
# In the model using a categorical alcohol variable,
# there is a non-linear relationship observed, which can be improved in a model with
# a continuous alcohol variable using a quadratic term or something.