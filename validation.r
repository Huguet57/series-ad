#################Validation#################################
validation = function(model, dades){
  s=frequency(get(model$series))
  resid=model$residuals
  par(mfrow=c(2,2),mar=c(3,3,3,3))
  #Residuals plot
  plot(resid,main="Residuals")
  abline(h=0)
  abline(h=c(-3*sd(resid),3*sd(resid)),lty=3,col=4)
  #Square Root of absolute values of residuals (Homocedasticity)
  scatter.smooth(sqrt(abs(resid)),main="Square Root of Absolute residuals",
                 lpars=list(col=2))
  
  #Normal plot of residuals
  qqnorm(resid)
  qqline(resid,col=2,lwd=2)
  
  ##Histogram of residuals with normal curve
  hist(resid,breaks=20,freq=FALSE)
  curve(dnorm(x,mean=mean(resid),sd=sd(resid)),col=2,add=T)
  
  #ACF & PACF of residuals
  par(mfrow=c(2,2))
  acf(resid,ylim=c(-1,1),lag.max=60,col=c(2,rep(1,s-1)),lwd=1)
  pacf(resid,ylim=c(-1,1),lag.max=60,col=c(rep(1,s-1),2),lwd=1)

  #ACF & PACF of square residuals 
  acf(resid^2,ylim=c(-1,1),lag.max=60,col=c(2,rep(1,s-1)),lwd=1)
  pacf(resid^2,ylim=c(-1,1),lag.max=60,col=c(rep(1,s-1),2),lwd=1)
  par(mfrow=c(1,1))
  
  cat("\n--------------------------------------------------------------------\n\n")
  
  #Ljung-Box p-values
  par(mar=c(2,2,1,1))
  tsdiag(model,gof.lag=7*s)
  cat("\n--------------------------------------------------------------------\n")
  print(model)
  
  #Stationary and Invertible
  cat("\nModul of AR Characteristic polynomial Roots:\n")
  Mod(polyroot(c(1,-model$model$phi)))
  
  cat("\nModul of MA Characteristic polynomial Roots:\n")
  Mod(polyroot(c(1,model$model$theta)))
  
  #Model expressed as an MA infinity (psi-weights)
  psis=ARMAtoMA(ar=model$model$phi,ma=model$model$theta,lag.max=36)
  names(psis)=paste("psi",1:36)
  cat("\nPsi-weights (MA(inf))\n")
  cat("\n--------------------\n")
  print(psis[1:20])
  
  #Model expressed as an AR infinity (pi-weights)
  pis=-ARMAtoMA(ar=-model$model$theta,ma=-model$model$phi,lag.max=36)
  names(pis)=paste("pi",1:36)
  cat("\nPi-weights (AR(inf))\n")
  cat("\n--------------------\n")
  print(pis[1:20])
  
  ##Shapiro-Wilks Normality test
  print(shapiro.test(resid(model)))
  
  #Sample ACF vs. Teoric ACF
  par(mfrow=c(2,2),mar=c(3,3,3,3))
  acf(dades, ylim=c(-1,1) ,lag.max=36,main="Sample ACF")
  
  plot(ARMAacf(model$model$phi,model$model$theta,lag.max=36),ylim=c(-1,1), 
       type="h",xlab="Lag",  ylab="", main="ACF Teoric")
  abline(h=0)
  
  #Sample PACF vs. Teoric PACF
  pacf(dades, ylim=c(-1,1) ,lag.max=36,main="Sample PACF")
  
  plot(ARMAacf(model$model$phi,model$model$theta,lag.max=36, pacf=T),ylim=c(-1,1),
       type="h", xlab="Lag", ylab="", main="PACF Teoric")
  abline(h=0)
  par(mfrow=c(1,1))
}