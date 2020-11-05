library(smcure)
data(e1684)

ind=which(is.na(e1684))
e1684=e1684[-37,]

e.surv=Surv(time=e1684$FAILTIME,event=e1684$FAILCENS)
test=phmc_mpl(e.surv~e1684$TRT+e1684$AGE+e1684$SEX,pi.formula = ~e1684$TRT+e1684$AGE+e1684$SEX,data=e1684,phmc_mpl.control(n.knots=c(8),smooth=NULL))
summary(test)

plot(test,which=1:3)

TRT0=predict(model1,cov=c(0,0,0.5791),type="survival")
TRT1=predict(model1,cov=c(1,0,0.5791),type="survival")
plot(TRT0$time,TRT0[,2],type="l",col="red",xlab="Time",ylab="Fitted Survival Probability",main="Comparative Fitted Survival Probabilities by Treatment")
lines(TRT0$time,TRT1[,2],type="l",col="blue")
legend("topright",legend=c("TRT=0","TRT=1"),col=c("red","blue"),lty=1)

TRT0h=predict(model1,cov=c(0,0,0.5791),type="hazard")
TRT1h=predict(model1,cov=c(1,0,0.5791),type="hazard")
plot(TRT0h$time,TRT0h[,2],type="l",col="red",xlab="Time",ylab="Estimated Hazard Function",main="Comparative Hazard Function Estimates by Treatment")
lines(TRT0h$time,TRT1h[,2],type="l",col="blue")
legend("topright",legend=c("TRT=0","TRT=1"),col=c("red","blue"),lty=1)
