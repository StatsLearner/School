require(ggplot2)
require(tidyverse)
require(multcomp)

### Download data
d <- read.csv("RUBBER.RAW", sep ="",  header = FALSE)
colnames(d) <- c("age","factory","death","manyears")
d$age <- as.factor(d$age)
d$factory <- as.factor(d$factory)


#### Exercise 1 ####
apply(d[,3:4],2,sum)

#### Exercise 2 ####
d %>%
  mutate(lograte=log(death/manyears)) %>%
  ggplot(aes(x=age, y=lograte, colour=factory)) + geom_point()

#### Exercise 3 ####
### (a) ###

fit <- glm(data = d, formula = death~1 + offset(log(manyears)), family = poisson(link="log"))
summary(fit)

fit1 <- glm(data = d, formula = death~age + offset(log(manyears)), family = poisson(link="log"))
summary(fit1)

# LR test
anova(fit, fit1, test = "Chisq")

# Another way
fit$deviance - fit1$deviance # 102.1718
pchisq(fit$deviance - fit1$deviance,fit$df.residual -fit1$df.residual, lower.tail = FALSE)
## the fit model is called null model, so we can take the difference between the null deviance
## and the fit1 deviance.

### (b) ###
# The rate ratio comparing age 60-69(category2) with 50-59(category1)
cbind(Estimate = exp(coef(fit1)[2]),
      LL = exp(coef(fit1)[2] - qnorm(0.975)*summary(fit1)$coefficients[2,2]),
      UU = exp(coef(fit1)[2] + qnorm(0.975)*summary(fit1)$coefficients[2,2]))

# The rate ratio comparing age 80-89 with 70-79
exp(coef(fit1)[4]-coef(fit1)[3])

K <- matrix(c(0,0,-1,1),1)
summary(glht(fit1, linfct = K))
exp(confint(glht(fit1, linfct = K))$confint[1,])


#### Exercise 4 ####
fit2 <- glm(data = d, formula = death~age + factory + offset(log(manyears)), family = poisson(link="log"))
summary(fit2)

cbind(Estimate = exp(coef(fit2)[5]),
      LL = exp(coef(fit2)[5] - qnorm(0.975)*summary(fit2)$coefficients[5,2]),
      UU = exp(coef(fit2)[5] + qnorm(0.975)*summary(fit2)$coefficients[5,2]))

anova(fit1, fit2, test = "Chisq")

  
#### Exercise 5 ####
### Saturated model
fit3 <- glm(data = d, formula = death~age*factory + offset(log(manyears)), family = poisson(link="log"))
summary(fit3)

# For a comparison with the output in stata
(cbind(Estimate=exp(coef(fit3)), exp(confint(fit3))))

# LR test for the interaction between age and factory
anova(fit2, fit3, test = "Chisq")


### 1) factotry1, age70-79(age3)
exp(coef(fit3)[1] + coef(fit3)[3])

### 2) factory2, age50-59(age1) 
exp(coef(fit3)[1] + coef(fit3)[5])

### 3) factory2, age60-69(age2) 
exp(coef(fit3)[1] + coef(fit3)[2] + coef(fit3)[5] + coef(fit3)[6])

### check
d %>%
  mutate(rate=death/manyears)


#### Exercise 6 ####
d <- read.csv("RUBBER.RAW", sep ="",  header = FALSE)
colnames(d) <- c("age","factory","death","manyears")
d$factory <- as.factor(d$factory)

###(a)
fit4 <- glm(data = d, 
    formula = death~age + offset(log(manyears)), family = poisson(link="log"))
summary(fit4)

fit4$null.deviance - fit4$deviance #89.44253
pchisq(fit4$null.deviance - fit4$deviance, df = 1, lower.tail = F)

# Exponentiate
(cbind(Estimate=exp(coef(fit4)), exp(confint(fit4))))


###(b)
fit5 <- glm(data = d, 
            formula = death~age + factory + offset(log(manyears)), family = poisson(link="log"))
summary(fit5)

# Exponentiate
(cbind(Estimate=exp(coef(fit5)), exp(confint(fit5))))

# LR test
anova(fit4,fit5, test = "Chisq")

###(c)
fit6 <- glm(data = d, 
            formula = death~age*factory + offset(log(manyears)), family = poisson(link="log"))
summary(fit6)

(cbind(Estimate=exp(coef(fit6)), exp(confint(fit6))))

#LR test
anova(fit5, fit6, test = "Chisq")

#### Exercise 7 ####
summary(fit5)

# In R, Residual Deviance = 2(LL(Saturated Model) - LL(Proposed Model)) 
pchisq(fit5$deviance, df = fit5$df.residual, lower.tail = F)