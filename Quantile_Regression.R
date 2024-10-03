#install.packages("quantreg")
library(quantreg)

# Draw Engel Curves
data(engel)
attach(engel)
plot(income,foodexp,cex=.25,type="n",xlab="Household Income", ylab="Food Expenditure")
points(income,foodexp,cex=.5,col="blue")
abline(rq(foodexp~income,tau=.5),col="blue")
abline(lm(foodexp~income),lty=2,col="red") #the dreaded ols line
taus = c(.05,.1,.25,.75,.90,.95)
for( i in 1:length(taus)){
  abline(rq(foodexp~income,tau=taus[i]),col="gray")
}

#Draw Engel Coefficient Plots
income_c <- income - mean(income)
fit1 <- summary(rq(foodexp~income_c,tau=2:98/100))
fit2 <- summary(rq(foodexp~income_c,tau=c(.05, .25, .5, .75, .95)))
plot(fit1,mfrow = c(1,2))
plot(fit2,mfrow = c(1,2))

