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

#Intervals
tl<-ts(exp(pr-1.96*se),start=ultim,freq=12)
tu<-ts(exp(pr+1.96*se),start=ultim,freq=12)
pr<-ts(exp(pr),start=ultim,freq=12)

ts.plot(serie,tl,tu,pr,lty=c(1,2,2,1),col=c(1,4,4,2),xlim=ultim[1]+c(-3,+2),type="o",main="Model ARIMA(0,1,1)(2,1,0)12")
abline(v=(ultim[1]-3):(ultim[1]+2),lty=3,col=4)

