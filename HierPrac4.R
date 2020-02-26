
#### Data load
d <- read.csv("gcse_selected.csv", sep = ",", header = T)

#### Packages
require(tidyverse)
require(lme4)
require(lmerTest)
require(stargazer)


#### Data prep ####
d$schgend <- factor(d$schgend,
                    levels = c(1,2,3),
                    labels = c("mixed gender", "boys only", "girls only"))

d$school <- factor(d$school)


#### Exercise 1 ####

d %>%
  group_by(school) %>%
  with(table(schgend)/nrow(d)*100)


#### Exercise 2 ####
d_byschool <- d %>%
  group_by(school) %>%
  summarise(n=n(), gcse_mean = mean(gcse), lrt_mean = mean(lrt),
            gcse_sd = sd(gcse), lrt_sd = sd(lrt))

summary(d_byschool)


#### Exercise 3 ####
fit1 <- lm(gcse~lrt+school - 1, data = d[d$school!=48,]) # -1 drops the intercept
summary(fit1)

#plot of this ancova model - useless
ggplot(d[d$school!=48,],aes(x = lrt, y=gcse, colour = school)) +
  geom_point(alpha = 0.5) + geom_line(data = cbind(d[d$school!=48,], pred = predict(fit1)), aes(y = pred)) + 
  theme_classic()


#### Exercise 4 ####
fit2 <- lm(gcse~lrt+schgend, data = d[d$school!=48,])
summary(fit2)

# residual standard error
sigma(fit1) 
sigma(fit2) # The second model has a larger residual se

# coefficients for lrt are similar
# that in the model2 is slightly larger
list(model1 = coef(fit1)[1], model2 = coef(fit2)[2])


#### Exercise 5 ####
# mixed effects model
fit3 <- lmer(gcse ~ lrt + (1|school), data = d[d$school!=48,])
summary(fit3)

# Extract the variance and standard deviation only
# sigma_u is 3.07, sigma_2 is 7.524
# suggesting that student variability is larger than school variability
vc <- VarCorr(fit3)
print(vc,comp=c("Variance", "Std.Dev"), digits=3)

# Fitting is quite good
## residuals vs fitted
plot(fit3)

## qqplot
qqnorm(resid(fit3))
qqline(resid(fit3))

# LRT to test the random effect, showinc AIC as well
#{lmerTest} package
ranova(fit3)

#### Exercise 6 ####
fit4 <- lmer(gcse ~ lrt + schgend + (1|school), data = d[d$school!=48,])

# sigma_u 2.915 and sigma_e 7.524
# the sigma_u became smaller but the sigma_e was identical
# the sigma_e was identical because the fixed intercept did not change
# the within variability.
summary(fit4)

# the output is slightly different from the testparm i.schgend
# in stata, but what it does is essentially the same.
# it seems that the loglikelihoods are different from the outputs
# in stata.
anova(fit3, fit4)


#### Exercise 7 ####
fit5 <- lmer(gcse ~ lrt + schgend + (1+ lrt |school), data = d[d$school!=48,])

# the sigma_e became a bit smaller, 7.4427
# the correlation between the random intercept and slope was 0.58
summary(fit5)


#### Exercise 8 ####
anova(fit4, fit5)


#### Exercise 9 ####

## DONT KNOW HOW TO DO THIS IN R

#### Exercise 10 ####

## Omitted ##


#### Exercise 11 ####
fit6 <- lmer(gcse ~ lrt + schgend + (1+ lrt |school), data = d)
summary(fit6)


# Convert the output of the lmerTest (which is in class merModLmerTest) 
# to the lmerMod class. 
# This will be compatible with stargazer.
class(fit5) <- "lmerMod"
class(fit6) <- "lmerMod"

# no change with/without school 48
stargazer(fit5, fit6,
          type = "text",
          digits = 3,
          no.space=TRUE)


#### Exercise 12 ####

d_sample <- d %>%
  group_by(school) %>%
  sample_n(size = 1) 

# they matched with EB predicted random intercepts and coefficients
# but ranef seems to produce conditional modes??
summary(ranef(fit6)$school)

# predicted random intercepts and coefficients
ranef(fit6)$school[48,]

# school level intercept residuals
r_int<- ranef(fit6)$school$`(Intercept)`
qqnorm(r_int)
qqline(r_int)

# school level slope residuals
r_slope<- ranef(fit6)$school$lrt
qqnorm(r_slope)
qqline(r_slope)

# summary of level 1 resisuals
summary(summary(fit6)$residuals)

# the level 1 residuals for school 48
summary(fit6)$residuals[d$school==48]


# just for fun
## the variability in the mixed gender group
## around the lrt = 0 was not modelled well?
ggplot(data = d_sample, aes(x=lrt, y=gcse, color = schgend)) +
  geom_point(alpha=0.5) +
  geom_point(data = cbind(d_sample, pred = predict(fit6, d_sample)),
                         aes(y = pred), size = 3, shape = 4)

## girls only also may have a variablity more than it modelled?
ggplot(data = d_sample, aes(x=lrt, y=gcse)) +
  facet_wrap(~schgend, nrow=2) +
  geom_point(alpha=0.3) +
  geom_point(data = cbind(d_sample, pred = predict(fit6, d_sample)),
             aes(y = pred), size = 2, shape = 4)

## All in one in balck and white to see an overall trend
ggplot(data = d_sample, aes(x=lrt, y=gcse)) +
  geom_point(alpha=0.3) +
  geom_point(data = cbind(d_sample, pred = predict(fit6, d_sample)),
             aes(y = pred), size = 2, shape = 4)


