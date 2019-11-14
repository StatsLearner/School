
##########################################
##########################################
### Are the distributions of ratio skewed?
##########################################
##########################################

################
# Ratio of # of events
################

### 1 ###
#Sampling from the Uniform
s <- seq(from = 0.01, to = 1, by = 0.01)
x <- sample(s, replace = TRUE)
y <- sample(s, replace = TRUE)
r <- x/y

# Plot density curves
plot(density(x))
plot(density(y))
plot(density(r)) ## The plot of ratio is skewed

### 2 ###
# Sampling from the Normal
z1.100 <- rnorm(100, mean = 0, sd = 1)
z2.100 <- rnorm(100, mean = 0, sd = 1)
r2.100 <- z1.100/z2.100

# Plot density curves
plot(density(z1.100))
plot(density(z2.100))
plot(density(r2.100)) ## The plot of ratio is the Cauchy distribution

### The Cauchy distribution is an example of a ratio distribution. ###
### The random variable associated with this distribution comes about ###
### as the ratio of two normally distributed variables with zero mean.###

### 3 ###
# Sampling from the Binomial
b1.100 <- rbinom(n=100, size = 100, prob = 0.1)
b2.100 <- rbinom(n=100, size = 100, prob = 0.1)
r.b.100 <- b1.100/b2.100

# Plot density curves
plot(density(b1.100))
plot(density(b2.100))
plot(density(r.b.100)) ## The plot of ratio is skewed


# Sampling from the Binomial with a large sample size
b1.1000 <- rbinom(n=100, size = 1000, prob = 0.1)
b2.1000 <- rbinom(n=100, size = 1000, prob = 0.1)
r.b.1000 <- b1.1000/b2.1000

# Plot density curves
plot(density(b1.1000))
plot(density(b2.1000))
plot(density(r.b.1000)) ## The plot of ratio looks symmetric

# Calculate skewness 100 times with n=1000 binomial
d1 <- sapply(rep(100,100), rbinom, 1000, 0.1)
d2 <- sapply(rep(100,100), rbinom, 1000, 0.1)
skew.mat <- matrix(NA, nrow = 100,ncol = 1)

for (i in 1:100){
  
  r <- d1[,i]/d2[,i]
  res <- (sum((r - mean(r))^3)/1000)/(sum((r - mean(r))^2)/999)^(3/2)
  skew.mat[i,1] <- res 
  
}

# histogram of skewness
hist(skew.mat) # In many cases the skewness values are not equal to 0

### 4 ###
# Conclusion
# Even with a larger sample size, the ratio of the number of events from 
# the binomial distribution has a skewed probability mass function.


###############
# Risk ratio
###############

### 1 ###
# Sampling from the Binomial distribution and calculated proportions
prp1.100 <- rbinom(n=100, size = 100, prob = 0.1)/100
prp2.100 <- rbinom(n=100, size = 100, prob = 0.1)/100
rr.100 <- prp1.100/prp2.100

# Plot density curves
plot(density(prp1.100))
plot(density(prp2.100))
plot(density(rr.100)) ## The plot of ratio is skewed


### 2 ###
# Sampling from the Binomial distribution  with a larger sample size
# and calculated proportions

prp1.1000 <- rbinom(n=100, size = 1000, prob = 0.1)/1000
prp2.1000 <- rbinom(n=100, size = 1000, prob = 0.1)/1000
rr.1000 <- prp1.1000/prp2.1000

plot(density(prp1.1000))
plot(density(prp2.1000))
plot(density(rr.1000)) ## The plot of ratio looks symmetric

# Calculate skewness 100 times with n=1000 binomial
d1 <- sapply(rep(100,100), rbinom, 1000, 0.1)/1000
d2 <- sapply(rep(100,100), rbinom, 1000, 0.1)/1000
skew.mat <- matrix(NA, nrow = 100,ncol = 1)

for (i in 1:100){
  
  r <- d1[,i]/d2[,i]
  res <- (sum((r - mean(r))^3)/1000)/(sum((r - mean(r))^2)/999)^(3/2)
  skew.mat[i,1] <- res 
  
}

# histogram of skewness
hist(skew.mat) # In many cases the skewness values are not equal to 0

### 3 ###
# Conclusion
# Even with a larger sample size, the risk ratio from 
# the binomial distribution has a skewed probability mass function.


###############
# Odds ratio
###############

### 1 ###
# Sampling from the Binomial distribution and calculated proportions
prp1.100 <- rbinom(n=100, size = 100, prob = 0.1)/100
prp2.100 <- rbinom(n=100, size = 100, prob = 0.1)/100
or.100 <- (prp1.100/(1-prp1.100)) / (prp2.100/(1-prp2.100))

# Plot density curves
plot(density(prp1.100))
plot(density(prp2.100))
plot(density(or.100)) ## The plot of ratio is skewed

### 2 ###
# Sampling from the Binomial distribution with a larger sample size
# and calculated proportions
prp1.1000 <- rbinom(n=100, size = 1000, prob = 0.1)/1000
prp2.1000 <- rbinom(n=100, size = 1000, prob = 0.1)/1000
or.1000 <- (prp1.1000/(1-prp1.1000)) / (prp2.1000/(1-prp2.1000))

# Plot density curves
plot(density(prp1.1000))
plot(density(prp2.1000))
plot(density(or.1000)) ## The plot of ratio looks symmetric

# Calculate skewness 100 times with n=1000 binomial
d1 <- sapply(rep(100,100), rbinom, 1000, 0.1)/1000
d2 <- sapply(rep(100,100), rbinom, 1000, 0.1)/1000
skew.mat <- matrix(NA, nrow = 100,ncol = 1)

for (i in 1:100){
  
  r <- (d1[,i]/(1-d1[,i])) / (d2[,i]/(1-d2[,i]))
  res <- (sum((r - mean(r))^3)/1000)/(sum((r - mean(r))^2)/999)^(3/2)
  skew.mat[i,1] <- res 
  
}

# histogram of skewness
hist(skew.mat) # In many cases the skewness values are not equal to 0

### 3 ###
# Conclusion
# Even with a larger sample size, the odds ratio from 
# the binomial distribution has a skewed probability mass function.



###############
# Appendix
###############

# Check the distribution of skewness of the normal distribution
d <- sapply(rep(100,100), rnorm, 0, 1)
for (i in 1:100){
  
  r <- d[,i]
  res <- (sum((r - mean(r))^3)/1000)/(sum((r - mean(r))^2)/999)^(3/2)
  skew.mat[i,1] <- res 
  
}

hist(skew.mat) # Centered around 0.