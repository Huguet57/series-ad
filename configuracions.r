# 1. Carreguem les dades
DIR <- "~/series-ad/"
setwd(DIR)
filename <- "./data/EntradTur.dat"

# 2. Creem la sèrie
serie <- ts(read.table(filename),
            start=2000,
            freq=12)

# 3. Fem el plot de la sèrie sense modificar
plot(serie/1e6,
     main="Entrada de turistas en España",
     ylab="millones de personas")

abline(v=2000:2019,
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
par(mfrow = c(1,1))
barplot(c(var(lnserie),
        var(d12lnserie),
        var(d1d12lnserie),
        var(d1d1d12lnserie)),
        ylab = "Variation of the transformed model",
        names.arg = c("ln", "d12ln", "d1d12ln", "d1d1d12ln"))

# 7. ACF i PACF
par(mfrow = c(2,2))

acf(d1d12lnserie,
    main = "d1d12ln ACF",
    ylim = c(-1,1),
    col = c(2,rep(1,11)),
    lwd = 2,
    lag.max = 72)

pacf(d1d12lnserie,
     main = "d1d12ln PACF",
     ylim = c(-1,1),
     col = c(rep(1,11),2),
     lwd = 2,
     lag.max = 72)

par(mfrow = c(2,2))

barplot(ARMAacf(ar = numeric(),
                ma = c(-0.6),
                lag.max = 72),
        main = "ARMA ACF with p = 0, q = 1",
        ylim = c(-1,1))

barplot(ARMAacf(ar = numeric(),
                ma = c(-0.6),
                lag.max = 72,
                pacf = TRUE),
        main = "ARMA PACF with p = 0, q = 1",
        ylim = c(-1,1))

barplot(ARMAacf(ar = numeric(),
                ma = c(-0.6, 0.3),
                lag.max = 72),
        main = "ARMA ACF with p = 0, q = 2",
        ylim = c(-1,1))

barplot(ARMAacf(ar = numeric(),
                ma = c(-0.6, 0.3),
                lag.max = 72,
                pacf = TRUE),
        main = "ARMA PACF with p = 0, q = 2",
        ylim = c(-1,1))

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