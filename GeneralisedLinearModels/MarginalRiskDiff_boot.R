# Obtain a margnalised risk difference estimate and a non-parametric percentile based confidence interval

d <- read.csv("kidney_example.csv", sep = ",", header = T)

fit1 <- glm(data = d, y~z+x+z*x, family = "binomial")
res <- exp(coef(fit1))

# Pr(Y=1|X=1,Z=0) = 0.1333333 
res[1]*res[3] / (1+res[1]*res[3])

# Pr(Y=1|X=1,Z=1) = 0.3125
res[1]*res[2]*res[3]*res[4] / (1+res[1]*res[2]*res[3]*res[4])

# Pr(Y=1|X=0,Z=0) =  0.06896552 
res[1] / (1+res[1])

# Pr(Y=1|X=0,Z=1) = 0.269962 
res[1]*res[2] / (1+res[1]*res[2])

# P(Z) = 0.49
summary(d$z)

# marginalising P(Y|X=1) = 0.221125 
Pr.x1 <- (res[1]*res[3] / (1+res[1]*res[3]))*(1-0.49)+ (res[1]*res[2]*res[3]*res[4] / (1+res[1]*res[2]*res[3]*res[4]))*0.49

# marginalising P(Y|X=0) = 0.1674538 
Pr.x0 <- (res[1] / (1+res[1]))*(1-0.49)+ (res[1]*res[2] / (1+res[1]*res[2]))*0.49

# risk difference = 0.05367122 
Pr.x1 - Pr.x0


# Bootstrap
marg.rd <- NULL
k <- 5000
for (i in 1:k) {
  set.seed(i)
  indice <- sample(1:nrow(d), nrow(d), replace=T)
  d.boot <- d[indice,]
  fit.boot <- glm(data = d.boot, y~z+x+z*x, family = "binomial")
  res.boot <- exp(coef(fit.boot))
  # Pr(Y=1|X=1,Z=0) 
  res.boot[1]*res.boot[3] / (1+res.boot[1]*res.boot[3])
  # Pr(Y=1|X=1,Z=1)
  res.boot[1]*res.boot[2]*res.boot[3]*res.boot[4] / (1+res.boot[1]*res.boot[2]*res.boot[3]*res.boot[4])
  # Pr(Y=1|X=0,Z=0)
  res.boot[1] / (1+res.boot[1])
  # Pr(Y=1|X=0,Z=1)  
  res.boot[1]*res.boot[2] / (1+res.boot[1]*res.boot[2])
  # P(Z) 
  mean(d.boot$z)
  # marginalising P(Y|X=1)  
  Pr.x1.boot <- (res.boot[1]*res.boot[3] / (1+res.boot[1]*res.boot[3]))*(1- mean(d.boot$z))+ (res.boot[1]*res.boot[2]*res.boot[3]*res.boot[4] / (1+res.boot[1]*res.boot[2]*res.boot[3]*res.boot[4]))* mean(d.boot$z)
  # marginalising P(Y|X=0)
  Pr.x0.boot <- (res.boot[1] / (1+res.boot[1]))*(1- mean(d.boot$z))+ (res.boot[1]*res.boot[2] / (1+res.boot[1]*res.boot[2]))* mean(d.boot$z)
  # risk difference 
  marg.rd[i] <-Pr.x1.boot - Pr.x0.boot
}
hist(marg.rd)
mean(marg.rd)
sqrt(var(marg.rd))
quantile(marg.rd, probs = c(0.025, 0.975)) # Percentile-based CI

