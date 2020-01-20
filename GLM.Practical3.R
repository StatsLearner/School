
require(ggplot2)
require(tidyverse)
require(stargazer)

### Download data

d <- read.csv("INSECT.RAW", sep ="",  header = FALSE)
colnames(d) <- c("CS2", "Killed", "Total")

### Exercise (1a)
d %>%
  mutate(Risk = Killed / Total) %>%
  ggplot(., aes(x=CS2, y=Risk)) + geom_point() +
  ggtitle("Proportion Killed") + ylab("Proportion Killed") +
  xlab("CS2 Dose (mg/l)")
  
### Exercise (1b)
d %>%
  mutate(Risk = Killed / Total) %>%
  mutate(LogOdds = log(Risk/(1-Risk))) %>%
  ggplot(., aes(x=CS2, y=LogOdds)) + geom_point() +
  ggtitle("Log Odds of Proportion Killed") + ylab("Proportion Killed") +
  xlab("CS2 Dose (mg/l)")


### Exercise (2)
d$Y <- cbind(d$Killed, d$Total - d$Killed) # Gives the outcome as a matrix of success/failure
fit <- glm(data = d, Y ~ CS2, family = binomial(link = "logit"))
summary(fit)

#(2a)#
#what is the estimated probability of death at
# a dose of 55 mg/l CS2?

# Since the link function is logit, pi = exp(Xb) / (1 + exp(Xb))
(exp(coef(fit)[1] + coef(fit)[2] * 55)) / (1 + exp(coef(fit)[1] + coef(fit)[2] * 55) )


#(2b)#
#Calculate the dose that would lead to 
# a 50% death rate (sometimes termed the LD50).

-coef(fit)[1]/coef(fit)[2]

#(2d)#
# Interpret the dose parameter in terms of an odds-ratio
# and calculate its 95% CI.


L <- exp(coef(fit)[2] - qnorm(0.975) * summary(fit)$coefficients[2,2])
U <- exp(coef(fit)[2] + qnorm(0.975) * summary(fit)$coefficients[2,2])
names(L) <- "Lower Limit"
names(U) <- "Upper Limit"
print(c(L,U))

exp(confint(fit)) # profile likelihood CI

#(2e)#
# Look at the fitted values from the model
# obtained using predict.
fit$fitted.values

# Just for fun
d2 <- data.frame(CS2=seq(0,100,1))
d2 <- data.frame(d2,predict = predict(fit, d2))
d2 <- data.frame(d2, pred_back = exp(d2$predict)/(1+exp(d2$predict)))

ggplot(d2, aes(x=CS2, y=pred_back)) + geom_point(alpha=0.1) + geom_line() +
  ggtitle("Predicted Proportion Killed") + ylab("Proportion Killed") +
  xlab("CS2 Dose (mg/l)")


#(2f)#
# Compare the fitted and original raw proportions, 
# by plotting them against dose. What do you conclude?

d <- data.frame(d, pred_logodds = predict(fit, d))
d <- data.frame(d, pred_raw = exp(d$pred_logodds) / (1 + exp(d$pred_logodds)))

d %>%
  mutate(Risk = Killed / Total) %>%
  ggplot(., aes(x=CS2, y=Risk, color="Measured")) + geom_point() +
  geom_point(aes(x=CS2, y=pred_raw, color="Fitted")) +
  ggtitle("Fitted and Observed") + ylab("Proportion Killed") +
  xlab("CS2 Dose (mg/l)")

### Exercise (3)
#(3a)#
fit2 <- glm(data = d, formula = Y~CS2 + I(CS2^2), family = binomial(link="logit"))
summary(fit2)

#(3b)#
d <- data.frame(d, pred2_logodds = predict(fit2, d))
d <- data.frame(d, pred2_raw = exp(d$pred2_logodds) / (1 + exp(d$pred2_logodds)))

d %>%
  mutate(Risk = Killed / Total) %>%
  ggplot(., aes(x=CS2, y=Risk, color="Measured")) + geom_point() +
  geom_point(aes(x=CS2, y=pred_raw, color="Fitted")) +
  geom_point(aes(X=CS2, y=pred2_raw, color="Quad Fitted")) +
  ggtitle("Fitted and Observed") + ylab("Proportion Killed") +
  xlab("CS2 Dose (mg/l)")

# Usse smoother for fun
d %>%
  mutate(Risk = Killed / Total) %>%
  ggplot(., aes(x=CS2, y=Risk, color="Measured")) + geom_point() + geom_smooth(se=FALSE) +
  geom_point(aes(x=CS2, y=pred_raw, color="Fitted")) + geom_smooth(aes(x=CS2, y=pred_raw, color="Fitted"), se=FALSE) +
  geom_point(aes(X=CS2, y=pred2_raw, color="Quad Fitted")) + geom_smooth(aes(x=CS2, y=pred2_raw, color="Quad Fitted"), se=FALSE) +
  ggtitle("Fitted and Observed") + ylab("Proportion Killed") +
  xlab("CS2 Dose (mg/l)")

#(4a, b)#
fit3 <- d %>%
  mutate(CS2_cat = as.factor(CS2)) %>%
  glm(formula = Y~CS2_cat , family = binomial(link="logit")) #Log Oddsratio

fit4 <- d %>%
  mutate(CS2_cat = as.factor(CS2)) %>%
  glm(formula = Y~CS2_cat , family = binomial(link="log")) #Log risk ratio

fit5 <- d %>%
  mutate(CS2_cat = as.factor(CS2)) %>%
  glm(formula = Y~CS2_cat , family = binomial(link="identity")) #Risk differece

fit6 <- d %>%
  mutate(CS2_cat = as.factor(CS2)) %>%
  glm(formula = Y~ CS2 + CS2_cat , family = binomial(link="logit"))


stargazer(fit3,fit4,fit5,fit6,
          type = "text",
          no.space=TRUE,
          omit.stat=c("f"))


# The deviance is -2*llr which is approximately chi-square distributed 
# In R, Residual Deviance = 2(LL(Saturated Model) - LL(Proposed Model)) 
# That's why just 2 * residual deviance is asymptotically chi-square distributed
pchisq((fit$deviance), df=nrow(d)-2, lower.tail = FALSE) 

# LR test of difference in deviance obtained the same result as above.
pchisq(fit$deviance - fit6$deviance, fit$df.residual-fit6$df.residual,lower.tail = FALSE)

# This is the quickest code to produce the same result
anova(fit, fit6, test = "Chisq")

# All of these results looks slightly different from what the solution says...