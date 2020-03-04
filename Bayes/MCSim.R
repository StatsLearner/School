
#### MC integration ####
# No of iterations
iter <- 10^5

# Iterations
sims <- rnorm(iter, mean=1, sd=10)
hist(sims)

# MC Integration to find Pr(3=<X<=6)
(mc.int <- sum(sims >=3 & sims <=6)/iter)


#### Approximating the Binomial Dist
# We flip a coin 10 times and we want to know the probability of getting more than 3 heads. 
# Now this is a trivial problem for the Binomial distribution, 
# but suppose we have forgotten about this or never learned it in the first place. 
# We can easily solve this problem with a Monte Carlo Simulation.
iter <- 10^5

binom <- function() {
  sum(sample(c(0,1), 10, replace = T)) > 3
}

(mc.binom <- sum(replicate(iter,binom()))/iter)
pbinom(3,10,0.5, lower.tail = F)


#### Approximating Pi####

iter <- 10^5
x <- runif(iter, min=-0.5, max=0.5)
y <- runif(iter, min=-0.5, max=0.5)
in.circle <- x^2 + y^2 <=0.5^2
mc.pi <- (sum(in.circle)/iter)*4

plot(x,y, pch=".", col=ifelse(in.circle,"blue", "grey"),
     xlab ="", ylab="", asp= 1,
     main=paste("MC approximation of Pi =", mc.pi))

