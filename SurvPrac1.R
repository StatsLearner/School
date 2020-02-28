
#########Fitting exponential null model################
d <- read.csv("surv_data_practical1.csv",sep=",",header=T)

require(survival)

exp.model <- survreg(Surv(survtimes)~1,dist="exponential",data=d)
summary(exp.model)$table

# get the lambda with the other result
cbind(lambda=exp(coef(exp.model)[1]),
      summary(exp.model)$table)



#################Investigating distributions###########

#exponential - survivor function#

#weibull - hazard#

wei.haz<-function(x,lambda,kappa){lambda*kappa*x^(kappa-1)}


curve(wei.haz(x,0.2,2),xlab="Time",ylab="Hazard function",col="red", ylim=c(0,1))
curve(wei.haz(x,0.4,2),add=T,col="blue")
curve(wei.haz(x,0.4,3),add=T,col="green")
curve(wei.haz(x,0.4,0.8),add=T,col="pink")
legend(0,1.05,
       c(expression(paste(kappa,"=2, ",lambda,"=0.2")),expression(paste(kappa,"=2, ",lambda,"=0.4")),
                 expression(paste(kappa,"=4, ",lambda,"=0.4")),expression(paste(kappa,"=0.8, ",lambda,"=0.4"))),
       col=c("red","blue","green","pink"),lty=c(1,1),bty="n",
       cex = 0.8)
title(main = "Hazard of Weibull Dist", font.main = 1)

#log-logistic - hazard#

loglog.haz<-function(x,theta,kappa){(exp(theta)*kappa*x^(kappa-1))/(1+exp(theta)*x^(kappa))}

curve(loglog.haz(x,1,0.2),xlab="Time",ylab="Hazard function",col="red")
curve(loglog.haz(x,1,2),add=T,col="black")
curve(loglog.haz(x,3,2),add=T,col="blue")
legend(0,11,
       c(expression(paste(theta,"=1, ", kappa,"=0.2")),
         expression(paste(theta,"=1, ", kappa,"=2")),
         expression(paste(theta,"=3, ", kappa,"=2"))),
       col=c("red","black","blue"),lty=c(1,1),bty="n",
       cex = 0.8)
title(main = "Hazard of Log-Logistic Dist", font.main = 1)