
require(ggplot2)
require(lme4)
require(lmerTest)
require(tidyverse)
require(stargazer)

#### Data Production ####

set.seed(1234)

# School random intercept
school <- (rnorm(10,0,15))

sd <- 5
N <- 100

group <- rep(1,N)
prescore <- rnorm(N, mean = 50, sd = sd)
prescore <- ifelse(prescore < 0, 0, 
                   ifelse(prescore > 100, 100, prescore))
postscore <- rnorm(N, 10 + school[1] + 1*prescore, sd)
postscore <- ifelse(postscore < 0, 0, 
                    ifelse(postscore > 100, 100, postscore))
d <- data.frame(group, prescore, postscore)


for (i in 2:length(school)) {
  y <- rnorm(N, 10 + school[i], sd) # fixed intercept5 + random intercept
  group <- rep(i, N)
  prescore <- rnorm(N, mean = 50, sd = sd)
  prescore <- ifelse(prescore < 0, 0, 
              ifelse(prescore > 100, 100, prescore))
  postscore <- rnorm(N, 10 + school[i] + 1*prescore, sd)
  postscore <- ifelse(postscore < 0, 0, 
                     ifelse(postscore > 100, 100, postscore))
  temp <- data.frame(group, prescore, postscore)
  d <- rbind(d,temp)
}

summary(d$prescore)
summary(d$postscore)
hist(d$prescore)
hist(d$postscore)

d <- data.frame(id = rep(1:(N*length(school)),1), d)

#### Explanatory Analysis ####

ggplot(data=d, aes(x=prescore, y=postscore, color=as.factor(group))) + 
  geom_point(alpha=0.5) + labs(color = "School")

ggplot(data=d, aes(x=prescore, y=postscore, color=as.factor(group))) + 
  facet_wrap(~as.factor(group), nrow=2) +
  geom_point(alpha=0.5) + labs(color = "School")

fit <- lm(data = d, postscore~prescore)
summary(fit)

plot(fit, which = 1)
plot(fit, which = 2)

ggplot(data=d, aes(x=prescore)) + geom_point(aes(y=postscore, color = as.factor(group)), alpha=0.5) +
  geom_smooth(aes(y=postscore),method = "lm") + labs(color = "School")

ggplot(data=d, aes(x=prescore)) + geom_point(aes(y=postscore, color = as.factor(group)), alpha=0.5) +
  facet_wrap(~as.factor(group), nrow=2) +
  geom_smooth(aes(y=postscore),method = "lm") + labs(color = "School")


#### Multilevel Modelling ####
fit2 <- lmer(postscore ~ prescore + (1|group), data = d)
summary(fit2) 

plot(fit2)
qqnorm(resid(fit2))
qqline(resid(fit2))

# Convert the output of the lmerTest (which is in class merModLmerTest) 
# to the lmerMod class. 
# This will be compatible with stargazer.
class(fit2) <- "lmerMod"

stargazer(fit, fit2,
          type = "text",
          digits = 3,
          no.space=TRUE)