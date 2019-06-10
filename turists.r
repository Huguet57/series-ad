# 1. Carreguem les dades
DIR <- "~/series-ad/"
setwd(DIR)
filename <- "./data/metro.dat"

# 2. Creem la sèrie
serie <- ts(read.table(filename),
            start=1996,
            freq=12)

# 3. Fem el plot de la sèrie sense modificar
plot(serie,
     main="Viajeros del metro de Barcelona",
     ylab="miles de personas")

abline(v=1996:2019,
       lty=3,
       col=4)

# 4. Primer estudi
# 4.1. Plot de les mitjanes - variàncies
TS <- matrix(serie, nr=12)
mitj <- apply(TS, 2, mean)
vars <- apply(TS, 2, var)

plot(mitj, vars,
     xlab="Medias anuales",
     ylab="Varianzas anuales",
     main="serie")

abline(lm(vars~mitj),
       col=2, lty=3, lwd=2)

# 4.2. Boxplot per anys
boxplot(serie ~ floor(time(serie)))

# 5. Transformació logarítmica
lnserie <- log(serie)
plot(lnserie)
boxplot(lnserie~floor(time(lnserie)))

# 5.1. Descomposici?n en componentes b?sicas (tendencia+estacionalidad+proceso estacionario)
plot(decompose(lnserie))

# 5.2. An?lisis de los ?ndices estacionales (plot mensual)
monthplot(lnserie)

# 5.3. Diferenciaci?n estacional (eliminaci?n de la componente estacional)
d12lnserie <- diff(lnserie, 12)
plot(d12lnserie)
abline(h=0)

# 5.4. Diferenciaci?n regular (media no constante)
d1d12lnserie <- diff(d12lnserie, 1)
plot(d1d12lnserie)
abline(h=0)

# 5. Nueva diferenciaci?n regular (posible no estacionariedad)
d1d1d12lnserie <- diff(d1d12lnserie, 1)
plot(d1d1d12lnserie)
abline(h=0)

# 6. Comparació de les variàncies
var(lnserie)
var(d12lnserie)
var(d1d12lnserie)
var(d1d1d12lnserie)

# 7. ACF i PACF
par(mfrow = c(1,2))

acf(d1d12lnserie,
    ylim = c(-1,1),
    col = c(2,rep(1,11)),
    lwd = 2,
    lag.max = 72)

pacf(d1d12lnserie,
     ylim = c(-1,1),
     col = c(rep(1,11),2),
     lwd = 2,
     lag.max = 72)

par(mfrow = c(1,1))

# 8. ARIMA model
(mod <- arima(d1d12lnserie,
              order = c(0,0,1),
              seasonal = list(order = c(2,0,0),
                              period = 12)))

(mod <- arima(lnserie,
              order = c(0,1,1),
              seasonal = list(order = c(2,1,0),
                              period=12)))

# 9. Validació
source("./validation.r")
validation(model = mod,
           dades = d1d12lnserie)

# 10. Estabilitat Model
ultim <- c(2017,12)
serie1 <- window(serie,
                 end = ultim + c(1,0))
lnserie1 <- log(serie1)
serie2 <- window(serie,
                 end=ultim)
lnserie2 <- log(serie2)

# 10.1 Models ARIMA 
(mod <- arima(lnserie1,
              order = c(0,1,1),
              seasonal = list(order=c(2,1,0),
                              period=12)))
(mod2 <- arima(lnserie2,
               order = c(0,1,1),
               seasonal = list(order=c(2,1,0),
                               period=12)))

# 11. Model prediction
pred <- predict(mod2,n.ahead=12)
pr <- ts(c(tail(lnserie2,1),pred$pred),start=ultim,freq=12)

se<-ts(c(0,pred$se),start=ultim,freq=12)

# 11.1. Intervals
tl<-ts(exp(pr-1.96*se),start=ultim,freq=12)
tu<-ts(exp(pr+1.96*se),start=ultim,freq=12)
pr<-ts(exp(pr),start=ultim,freq=12)

ts.plot(serie,tl,tu,pr,
        lty=c(1,2,2,1),col=c(1,4,4,2),xlim=ultim[1]+c(-3,+2),
        type="o",main="Model ARIMA(0,1,1)(2,1,0)12")
abline(v=(ultim[1]-3):(ultim[1]+2),
       lty=3,col=4)

# 11.2. Error de la predicció
previs <- window(cbind(tl,pr,tu,serie,error=round(serie-pr,3)), start=ultim)
obs <- window(serie, start=ultim)
(mod.EQM1 <- sqrt(sum(((obs-pr)/obs)^2)/12))
(mod.EAM1 <- sum(abs(obs-pr)/obs)/12)

# 12. Previsions a llarg termini amb el model complet
pred=predict(mod,n.ahead=12)
pr<-ts(c(tail(lnserie,1),pred$pred),start=ultim+c(1,0),freq=12)
se<-ts(c(0,pred$se),start=ultim+c(1,0),freq=12)

# 12.1. Intervals
tl1<-ts(exp(pr-1.96*se),start=ultim+c(1,0),freq=12)
tu1<-ts(exp(pr+1.96*se),start=ultim+c(1,0),freq=12)
pr1<-ts(exp(pr),start=ultim+c(1,0),freq=12)

ts.plot(serie,tl1,tu1,pr1,
        lty=c(1,2,2,1),col=c(1,4,4,2),xlim=c(ultim[1]-2,ultim[1]+3),
        type="o",main="Model ARIMA(0,1,1)(2,1,0)12")
abline(v=(ultim[1]-2):(ultim[1]+3),lty=3,col=4)

(previs1=window(cbind(tl1,pr1,tu1),start=ultim+c(1,0)))

# 13. ARIMA model with outlier treatment
# 13.1. Atòpics
source("./airbcn/atipics2.R")

# 13.1.1. Detecci?n de at?picos: hay una diferenciaci?n regular y una de orden 12
# fijamos el criterio a 2.8 y buscamos tambi?n LS
mod.atip=outdetec(mod,dif=c(1,12),crit=2.8,LS=T)

# 13.1.2. Tabla de at?picos encontrados con su fecha
atipics=mod.atip$atip[order(mod.atip$atip[,1]),]
meses=c("Ene","Feb","Mar","Abr","May","Jun","Jul","Ago","Sep","Oct","Nov","Dic")
data.frame(atipics,
           Fecha=paste(meses[(atipics[,1]-1)%%12+1],
                       start(lnserie)[1]+((atipics[,1]-1)%/%12)))
mod.atip$sigma2

data.frame(atipics,
           Fecha=paste(meses[(atipics[,1]-1)%%12+1],
                       start(lnserie)[1]+((atipics[,1]-1)%/%12)),
           perc.Obs=exp(atipics[,3])*100)

# 13.2. Comparaci?n serie observada con la serie linealizada (sin at?picos)
lnserie.lin=lineal(lnserie,mod.atip$atip)
serie.lin=exp(lnserie.lin)

plot(serie.lin,col=2)
lines(serie)

# 13.3. Efecto de los at?picos en la serie de logaritmos
plot(lnserie-lnserie.lin)

# 14. Identificaci?n del modelo para la serie linealizada
d1d12lnserie.lin=diff(diff(lnserie.lin,12))
par(mfrow=c(1,2))
acf(d1d12lnserie.lin,ylim=c(-1,1),lag.max=72,col=c(2,rep(1,11)),lwd=2)
pacf(d1d12lnserie.lin,ylim=c(-1,1),lag.max=72,col=c(rep(1,11),2),lwd=2)
par(mfrow=c(1,1))

# 14.1. Estimaci?n del modelo para la serie linealizada
(mod.lin=arima(lnserie.lin,order=c(0,1,1),seasonal=list(order=c(2,1,0),period=12)))

dades=d1d12lnserie.lin
model=mod.lin
validation(model,dades)

# 15. Estabilitat Modelpara la serie linealizada (SENSE CONSTANT!!!!)
ultim=c(2017,12)
serie1.lin=window(serie.lin,end=ultim+c(1,0))
lnserie1.lin=log(serie1.lin)
serie2.lin=window(serie.lin,end=ultim)
lnserie2.lin=log(serie2.lin)

(mod.lin=arima(lnserie1.lin,order=c(0,1,1),seasonal=list(order=c(2,1,0),period=12)))
(mod2.lin=arima(lnserie2.lin,order=c(0,1,1),seasonal=list(order=c(2,1,0),period=12)))

# 16 Model prediction
pred=predict(mod2.lin,n.ahead=12)
wLS=sum(mod.atip$atip[mod.atip$atip$type_detected=="LS"
                      & mod.atip$atip$Obs<=length(serie)-12,3])
predic=pred$pr+wLS
pr<-ts(c(tail(lnserie2,1),predic),start=ultim,freq=12)
se<-ts(c(0,pred$se),start=ultim,freq=12)

# 16.1. Intervals
tl<-ts(exp(pr-1.96*se),start=ultim,freq=12)
tu<-ts(exp(pr+1.96*se),start=ultim,freq=12)
pr<-ts(exp(pr),start=ultim,freq=12)

ts.plot(serie,tl,tu,pr,
        lty=c(1,2,2,1),col=c(1,4,4,2),xlim=ultim[1]+c(-3,+2),
        type="o",main="Model ARIMA(0,1,1)(2,1,0)12")
abline(v=(ultim[1]-3):(ultim[1]+2),lty=3,col=4)

# 17. Error en la predicció
(previs.lin=window(cbind(tl,pr,tu,serie,error=round(serie-pr,3)),start=ultim))
obs=window(serie,start=ultim)
(mod.EQM2=sqrt(sum(((obs-pr)/obs)^2)/12))
(mod.EAM2=sum(abs(obs-pr)/obs)/12)

# 18. Previsions a llarg termini amb el model complet
pred=predict(mod.lin,n.ahead=12)
wLS=sum(mod.atip$atip[mod.atip$atip$type_detected=="LS",3])
predic=pred$pr+wLS
pr<-ts(c(lnserie[length(lnserie)],predic),start=ultim+c(1,0),freq=12)
se<-ts(c(0,pred$se),start=ultim+c(1,0),freq=12)

# 18.1. Intervals
tl2<-ts(exp(pr-1.96*se),start=ultim+c(1,0),freq=12)
tu2<-ts(exp(pr+1.96*se),start=ultim+c(1,0),freq=12)
pr2<-ts(exp(pr),start=ultim+c(1,0),freq=12)

# 18.2. Plot
ts.plot(serie,tl2,tu2,pr2,lty=c(1,2,2,1),col=c(1,4,4,2),xlim=ultim[1]+c(-1,+3),type="o",main="Model ARIMA(0,1,1)(2,1,0)12")
abline(v=(ultim[1]-2):(ultim[1]+3),lty=3,col=4)
(previs2=window(cbind(tl2,pr2,tu2),start=ultim+c(1,0)))

# 19. Plot comparació del model amb outliers tracts i sense
cbind(previs1,previs2)
ts.plot(serie, tl1,tu1,pr1, tl2,tu2,pr2,
        lty=c(1,2,2,1,2,2,1),col=c(1,4,4,2,3,3,6),xlim=ultim[1]+c(1,3),
        type="o",main="AIRBCN")
legend("bottomleft",c("ARIMA(0,1,1)(2,1,0)12",
                   "ARIMA(0,1,1)(2,1,0)12 with outlier treatment"),
       col=c(4,3),lty=1,lwd=2)
abline(v=ultim[1]+1:3,lty=3,col=4)

# 20. Resultats
resul=data.frame(
  par=c(length(coef(mod)),length(coef(mod.lin))+nrow(mod.atip$atip)),
  Sigma2Z=c(mod$sigma2,mod.lin$sigma2),
  AIC=c(AIC(mod),AIC(mod.lin)+2*nrow(mod.atip$atip)),
  BIC=c(BIC(mod),BIC(mod.lin)+log(length(serie)-13)*nrow(mod.atip$atip)),
  RMSPE=c(mod.EQM1,mod.EQM2),
  MAPE=c(mod.EAM1,mod.EAM2),
  meanLength=c(sum(previs1[,3]-previs1[,1]),sum(previs2[,3]-previs2[,1]))/12)
row.names(resul)=c("ARIMA(0,1,1)(2,1,0)12","ARIMA(0,1,1)(2,1,0)12+Atip")

resul
