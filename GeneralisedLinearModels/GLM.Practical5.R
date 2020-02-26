require(ggplot2)
require(sandwich)
require(lmtest) # for coeftest
require(MASS) #f or negative binomial
require(stargazer)

### Download data
d <- read.csv("arisona.csv", sep =",",  header = TRUE)

### Exercise 1 ####

ggplot(data = d, aes(x=los)) + geom_histogram() +
  ggtitle("Length of stay") + xlab("Length of stay")


# 95% wald CI - lower limit
mean(d$los) - qnorm(0.975)*sqrt(var(d$los)/nrow(d))
# 95% wald CI - upper limit
mean(d$los) + qnorm(0.975)*sqrt(var(d$los)/nrow(d))

## This wald CI is valid on the assumption of normality, 
## which is not likely to be hold on this variable.
## But, the sample size is quite large, so due to the CLT,
## the skewness might not be an issue.


#### Exercise 2 ####

fit1 <- glm(formula = los~1 , data = d, family = poisson(link = "log"))
summary(fit1)

# 95% Wald upper CI
exp(fit1$coefficients - qnorm(0.975)*summary(fit1)$coefficients[1,2])
# 95% Wald lower CI
exp(fit1$coefficients + qnorm(0.975)*summary(fit1)$coefficients[1,2])
# 95% profile likelihood confidence interval
exp(confint(fit1)) 


# Copmare the two CIs
list("normal-based" = c(mean(d$los) - qnorm(0.975)*sqrt(var(d$los)/nrow(d)),
  mean(d$los) + qnorm(0.975)*sqrt(var(d$los)/nrow(d))))
list("poisson-based" = c(exp(fit1$coefficients - qnorm(0.975)*summary(fit1)$coefficients[1,2]),
exp(fit1$coefficients + qnorm(0.975)*summary(fit1)$coefficients[1,2])))

## The poisson-based CI is narrower than that based on the normality assumption.
## However, the narrow poisson-based CI is likely to be attributable to the fact that
## it does not incorporate the overdispersion of data.


### Exercise 3 ####

fit2 <- glm(formula = los~type2 + type3, data = d, 
            family = poisson(link = "log"))
summary(fit2)
(cbind(Estimate=exp(coef(fit2)), exp(confint(fit2))))

### Exercise 4 ####
fit3 <- glm(formula = los~ as.factor(age) + type2 + type3, data = d, 
            family = poisson(link = "log"))
summary(fit3) 

# Exponentiated results
cbind(Estimate=exp(coef(fit3)),
      "p-value"=summary(fit3)$coefficients[,4],
       exp(confint(fit3)))


# LR test
-2*(logLik(fit2) - logLik(fit3)) # 18.4422
fit2$deviance - fit3$deviance # 18.4422
pchisq(18.4422, df=fit2$df.residual - fit3$df.residual, 
       lower.tail = FALSE)

anova(fit2, fit3, test = "Chisq")

## The LR test yielded a significant result, therefore,
## one or some of the interaction terms between age category and type of
## admission is/are statistically significant, meaning age is dependent of
## type of admission.


### Exercise 5 ####

cov <- vcovHC(fit3, type = "HC0") #In RSM the HC1 was chosen as type
se <- sqrt(diag(cov))
cbind("estimate"=exp(coef(fit3)),
      "robustSE"=se, #SE values are so different from those in the solution.
      "z-statistic"=coef(fit3)/se,
      "p-value"= 2*pnorm(abs(coef(fit3)/se), lower.tail = FALSE),
      "LL" = exp(coef(fit3) - qnorm(0.975)*se),
      "UL" = exp(coef(fit3) + qnorm(0.975)*se))

## Another approach (from {lmetst} package) in a log form
coeftest(fit3, vcov = vcovHC(fit3, type = "HC0"))

## The interaction terms are all statistically insignificant.
## Therefore, there is no evidenace against that 
## the age is not associated with the outcome and is independent of type of admission.



### Exercise 6 ####

# I do not know how to do it!


### Exercise 7 ####

fit4 <- glm.nb(data = d, los~1) # {MASS} package
summary(fit4)
# The R parameter (theta) is equal to the inverse of the dispersion parameter (alpha) 
# estimated in Stata

# Just for fun - compare the three models
stargazer(fit1, fit2, fit4,
          type = "text",
          no.space=TRUE,
          omit.stat=c("f"))



### Exercise 8 ####

fit5 <- glm.nb(data = d, los~ type2 + type3) # {MASS} package
summary(fit5)


# negative binomial mean
# type1 admission
(mu1 <- exp(coef(fit5)[1]))
names(mu1) <- c("")

# type2 admission
(mu2 <- exp(coef(fit5)[1]+(coef(fit5)[2])))
names(mu2) <- c("")

# type3 admission
(mu3 <- exp(coef(fit5)[1]+(coef(fit5)[3])))
names(mu3) <- c("")


# negative binoial variance
data.frame(type=c("Elective", "Urgent", "Emergency"),
      mean=c(mu1,mu2,mu3),
      variance=c(mu1*(1+(1/fit5$theta)*mu1),
                 mu2*(1+(1/fit5$theta)*mu2),
                 mu3*(1+(1/fit5$theta)*mu3)))

# observed variance
c(var(d$los[d$type1==1]),
var(d$los[d$type2==1]),
var(d$los[d$type3==1]))

data.frame(type=c("Elective", "Urgent", "Emergency"),
      NBVariance=c(mu1*(1+(1/fit5$theta)*mu1),
        mu2*(1+(1/fit5$theta)*mu2),
        mu3*(1+(1/fit5$theta)*mu3)),
      ObservedVariance=c(var(d$los[d$type1==1]),
        var(d$los[d$type2==1]),
        var(d$los[d$type3==1])))


