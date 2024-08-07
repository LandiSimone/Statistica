data = read.csv("tabella.csv", header = TRUE, sep = ",", stringsAsFactors = F)
colnames(data) <- c("Data", "Miglia")
head(data)
summary(data)

# Costruzione Time Series
data_ts <- ts(data$Miglia, frequency = 12, start = 2000, end = c(2021, 12))
head(data_ts)
ts.plot(data_ts, xlab="Anni", ylab="Miglia (in milioni di unità)")

# Parametri Time Series
start(data_ts)
end(data_ts)
frequency(data_ts)


### ANALISI PRELIMINARE
# ACF
acf(data_ts)
acf(data_ts, 25) ### Chiari picchi dopo un periodo e due periodi
acf(diff(data_ts))
acf(diff(data_ts), 25)


# Confronto Stagionalità
m_data = matrix(data_ts, 12, 22)

ts.plot(m_data, col = heat.colors(22), main = "Confronto Stagionalità", ylab = "Miglia (in milioni di unità)", lwd=2)
lines(rowMeans(m_data),lwd=3,col="black")

# Rimuoviamo il trend normalizzando
par(bg = "grey")
ts.plot(scale(m_data, scale=F), col = heat.colors(22), main = "Confronto Stagionalità", ylab = "Miglia Normalizzate", xlab="Mese", lwd=1.5)
par(bg = "white")

# Bande di confidenza Stagionalità
data_ts.sd=vector("numeric",12)
for(i in 1:12){
  data_ts.sd[i]=sd(m_data[i,])
}

data_ts.m=rowMeans(m_data)
plot(data_ts.m,pch=20,type="b", ylim=range(c(data_ts.m-3*data_ts.sd, data_ts.m+3*data_ts.sd)), 
     main = "Bande di Confidenza della Stagionalità", ylab="Miglia (in milioni di unità)", xlab="Mese")
arrows(1:12,data_ts.m-data_ts.sd,1:12,data_ts.m+data_ts.sd,length=0.02,angle=90,
       code=3,col="green3")
points(data_ts.m+data_ts.sd,type="b",pch=20,col="gray")
points(data_ts.m-data_ts.sd,type="b",pch=20,col="gray")

# Boxplot Stagionalità
boxplot(t(m_data), pch=19)
boxplot(t(scale(m_data, scale=F)), pch=19)


### DECOMPOSIZIONE

# Decomposizione Additiva
data_ts.da=decompose(data_ts)
plot(data_ts.da)

# Decomposizione Moltiplicativa
data_ts.dm=decompose(data_ts, type="multiplicative")
plot(data_ts.dm)

# Stl Additivo
#data_ts.stla = stl(data_ts,s.window=11)
#plot(data_ts.stla, main="Decomposizione STL Additiva")

# Stl Moltiplicativo
#data_ts.stlm = stl(log(data_ts),s.window=7)
#plot(data_ts.stlm, main="Decomposizione STL Moltiplicativa")

# Confronto Stagionalità
plot(data_ts.da$seasonal,main = "Confronto Stagionalità", ylab = "Stagionalità")
lines(mean(data_ts.dm$trend, na.rm=T)*(data_ts.dm$seasonal-1),col="red")
#lines(data_ts.stla$time.series[,1],col="green")
#lines(data_ts.stlm$time.series[,1]*mean(data_ts.stla$time.series[,2],na.rm=T),col="blue")


# Confronto rumore
plot(data_ts.da$random, main = "Confronto Rumore", ylab="Rumore")
lines(mean(data_ts.dm$trend, na.rm=TRUE)*(data_ts.dm$random-1), col="red")
#lines(data_ts.stla$time.series[,3],col="green")
#lines(data_ts.stlm$time.series[,3]*mean(data_ts.stla$time.series[,2],na.rm=T),col="blue")

### VALORI PARAGONABILI CATTURANO COSE SIMILI, dobbiamo decidere quale decomposizione funzioni
# meglio.

### ANALISI DEI RESIDUI

# DECOMPOSIZIONE ADDITIVA
data_ts.dar <- na.omit(data_ts.da$random)

# Istogramma
hist(data_ts.dar, 35, freq=F, main="Istogramma Residui Additivi", xlab="Residui")
lines(density(data_ts.dar), col="blue")
lines(sort(data_ts.dar), dnorm(sort(data_ts.dar), mean(data_ts.dar), sd(data_ts.dar)), col="red")

# Q-Q Plot
qqnorm(data_ts.dar, main="Q-Q Plot Residui Additivi")
qqline(data_ts.dar, col="red")

# Shapito Wilk
shapiro.test(data_ts.dar)

# ACF Residui
acf(data_ts.dar, 50, main = "ACF Residui Additivi")

# DECOMPOSIZIONE MOLTIPLICATIVA
data_ts.dmr <- na.omit(log(data_ts.dm$random))

# Istogramma
hist(data_ts.dmr, 35, freq=F, main="Istogramma Residui Moltiplicativi", xlab="Residui")
lines(density(data_ts.dmr), col="blue")
lines(sort(data_ts.dmr), dnorm(sort(data_ts.dmr), mean(data_ts.dmr), sd(data_ts.dmr)), col="red")

# Q-Q Plot
qqnorm(data_ts.dmr, main="Q-Q Plot Residui Moltiplicativi")
qqline(data_ts.dmr, col="red")

# Shapiro Wilk
shapiro.test(data_ts.dmr)

# ACF Residui
acf(data_ts.dmr, 50, main = "ACF Residui Moltiplicativi")

# DECOMPOSIZIONE STL ADDITIVA
#data_ts.stlar <- data_ts.stla$time.series[,3]

# Istogramma
#hist(data_ts.stlar, 30, freq=F, main="Istogramma Residui STL Additivi", xlab="Residui")
#lines(density(data_ts.stlar), col="blue")
#lines(sort(data_ts.stlar), dnorm(sort(data_ts.stlar), mean(data_ts.stlar), sd(data_ts.stlar)), col="red")

# Q-Q Plot
#qqnorm(data_ts.stlar, main="Q-Q Plot Residui STL Additivi")
#qqline(data_ts.stlar, col="red")

# Shapiro Wilk
#shapiro.test(data_ts.stlar)

# ACF Residui
#acf(data_ts.stlar, 50, main = "ACF Residui STL Additivi")


# DECOMPOSIZIONE STL MOLTIPLICATIVA
#data_ts.stlmr <- data_ts.stlm$time.series[,3]

# Istogramma
#hist(data_ts.stlmr, 40, freq=F, main="Istogramma Residui STL Moltiplicativi", xlab="Residui")
#lines(density(data_ts.stlmr), col="blue")
#lines(sort(data_ts.stlmr), dnorm(sort(data_ts.stlmr), mean(data_ts.stlmr), sd(data_ts.stlmr)), col="red")

# Q-Q Plot
#qqnorm(data_ts.stlmr, main="Q-Q Plot Residui STL Moltiplicativi")
#qqline(data_ts.stlmr, col="red")

# Shapiro Wilk
#shapiro.test(data_ts.stlmr)

# ACF Residui
#acf(data_ts.stlmr, 50, main = "ACF Residui STL Moltiplicativi")

# Confronto Deviazione Standard modelli di Decomposizione
sd(acf(data_ts.dar, plot=F)$acf)
sd(acf(data_ts.dmr, plot=F)$acf)
#sd(acf(data_ts.stlar, plot = F)$acf)
#sd(acf(data_ts.stlmr, plot = F)$acf)

### PREVISIONE

# Holt Winters

data_ts.hw.1 = HoltWinters(data_ts)
plot(data_ts.hw.1)

data_ts.hw.1$alpha
data_ts.hw.1$beta
data_ts.hw.1$gamma

# Confronto con moltiplicativo
# Trend
ts.plot(data_ts.dm$trend,data_ts.hw.1$fitted[,2],col=c("black","red"), main = "Confronto HW Auto con Trend  Moltiplicativo")
legend(
  "bottomleft",
  legend = c( "Moltiplicativo", "Holt-Winters Auto"),
  col = c("black", "red"),
  lwd = 2,
  cex = 1.0
)

# Stagionalità
ts.plot(mean(data_ts.dm$trend, na.rm=T)*(data_ts.dm$seasonal-1),data_ts.hw.1$fitted[,4],col=c("black","red"), main = "Confronto HW Auto con Stagionalità MOltiplicativo")
legend(
  "bottomleft",
  legend = c( "Moltiplicativo", "Holt-Winters Auto"),
  col = c("black", "red"),
  lwd = 2,
  cex = 1.0
)

# Holt Winters Manuale
x=1:23

coefficients(lm(data_ts[1:23]~x))

data_ts.hw.2 = HoltWinters(data_ts, alpha=0.35, beta=0.1, gamma=0.45, l.start=221601, b.start=783 )
plot(data_ts.hw.2)
plot(data_ts.hw.2$fitted)

plot(HoltWinters(data_ts,l.start=48,b.start=-0.21)$fitted)

# Confronto con moltiplicativo
# Trend
ts.plot(data_ts.dm$trend,data_ts.hw.2$fitted[,2],col=c("black","red"), main = "Confronto HW Man con Trend Moltiplicativo")
legend(
  "bottomleft",
  legend = c( "Moltiplicativo", "Holt-Winters Man"),
  col = c("black", "red"),
  lwd = 2,
  cex = 1.0
)

# Stagionalità
ts.plot(mean(data_ts.dm$trend, na.rm=T)*(data_ts.dm$seasonal-1),data_ts.hw.2$fitted[,4],col=c("black","red"), main = "Confronto HW Man con Stagionalità Moltiplicativo")
legend(
  "bottomleft",
  legend = c( "Moltiplicativo", "Holt-Winters Man"),
  col = c("black", "red"),
  lwd = 2,
  cex = 1.0
)

# Autoregressione con Yule-Walker
data_ts.ar = ar(data_ts)
data_ts.ar
data_ts.ar$var/var(data_ts[15:length(data_ts)])

data_ts.ar.an = data_ts - data_ts.ar$resid


# Autoregressione con Minimi Quadrati
data_ts.ls = ar(data_ts, method="ols")
data_ts.ls
data_ts.ar$var/var(data_ts[23:length(data_ts)])

data_ts.ls.an = data_ts - data_ts.ls$resid



### ANALISI DEI RESIDUI

# Holt Winters con parametri automatici
data_ts.hw.1.r = residuals(data_ts.hw.1)

plot(data_ts.hw.1.r, type="p", pch=20)
plot(data_ts.hw.1$fitted[,1], data_ts.hw.1.r, pch=20)

# Varianza Spiegata
var(data_ts.hw.1.r)/var(window(data_ts, c(2001, 1)))

# Autocorrelazione
acf(data_ts.hw.1.r, main="ACF Residui HW Auto")

# Istogramma
hist(data_ts.hw.1.r, 30, freq=F, main="Istogramma Residui HW Auto")
lines(density(data_ts.hw.1.r),col="blue")
lines(sort(na.omit(data_ts.hw.1.r)),dnorm(sort(na.omit(data_ts.hw.1.r)),mean(na.omit(data_ts.hw.1.r)),sd(na.omit(data_ts.hw.1.r))),col="red")

# Q-Q Plot
qqnorm(data_ts.hw.1.r,main = "Q-Q Plot HW Auto")
qqline(data_ts.hw.1.r, col="red")

# Shapiro Test
shapiro.test(data_ts.hw.1.r)

# Incertezza
plot(data_ts.hw.1, predict(data_ts.hw.1, 12), main="Holt-Winters Auto", ylim=c(300,1800))
#lines(predict(data_ts.hw.1, 12) + qnorm(0.05, mean(data_ts.hw.1.r)), col="blue")
#lines(predict(data_ts.hw.1, 12) + qnorm(0.95, mean(data_ts.hw.1.r)), col="blue")
lines(predict(data_ts.hw.1, 12) + quantile(data_ts.hw.1.r, 0.05), col="green3")
lines(predict(data_ts.hw.1, 12) + quantile(data_ts.hw.1.r, 0.95), col="green3")
legend(
  "bottomleft",
  legend = c( "Osservazioni", "Holt-Winters Auto"),
  col = c("black", "red"),
  lwd = 2,
  cex = 0.6
)


# Holt Winters con parametri manuali
data_ts.hw.2.r = residuals(data_ts.hw.2)

plot(data_ts.hw.2.r, type="p", pch=20)
plot(data_ts.hw.2$fitted[,1], data_ts.hw.2.r, pch=20)

# Varianza Spiegata
var(data_ts.hw.2.r)/var(window(data_ts, c(2001, 1)))

# Autocorrelazione
acf(data_ts.hw.2.r, main="ACF Residui HW Man")

# Istogramma
hist(data_ts.hw.2.r, 30, freq=F, main="Istogramma Residui HW Man")
lines(density(data_ts.hw.2.r),col="blue")
lines(sort(na.omit(data_ts.hw.2.r)),dnorm(sort(na.omit(data_ts.hw.2.r)),mean(na.omit(data_ts.hw.2.r)),sd(na.omit(data_ts.hw.2.r))),col="red")

# Q-Q Plot
qqnorm(data_ts.hw.2.r,main = "Q-Q Plot HW Man")
qqline(data_ts.hw.2.r, col="red")

# Shapiro Test
shapiro.test(data_ts.hw.2.r)

# Incertezza
plot(data_ts)
plot(data_ts.hw.2, predict(data_ts.hw.2, 12), main="Holt-Winters Manuale", ylim=c(300,1800))
#lines(predict(data_ts.hw.2, 12) + qnorm(0.05, mean(data_ts.hw.2.r)), col="blue")
#lines(predict(data_ts.hw.2, 12) + qnorm(0.95, mean(data_ts.hw.2.r)), col="blue")
lines(predict(data_ts.hw.2, 12) + quantile(data_ts.hw.2.r, 0.05), col="green3")
lines(predict(data_ts.hw.2, 12) + quantile(data_ts.hw.2.r, 0.95), col="green3")
legend(
  "bottomleft",
  legend = c( "Osservazioni", "Holt-Winters Manuale"),
  col = c("black", "red"),
  lwd = 2,
  cex = 0.6
)

# Yule Walker
data_ts.ar.r = data_ts.ar$resid[16:252]

plot(data_ts.ar.r, type="p", pch=20)

# Autocorrelazione
acf(data_ts.ar.r, main="ACF Residui YW")

# Istogramma
hist(data_ts.ar.r, 30, freq=F, main="Istogramma Residui YW")
lines(density(data_ts.ar.r),col="blue")
lines(sort(na.omit(data_ts.ar.r)),dnorm(sort(na.omit(data_ts.ar.r)),mean(na.omit(data_ts.ar.r)),sd(na.omit(data_ts.ar.r))),col="red")

# Q-Q Plot
qqnorm(data_ts.ar.r,main = "Q-Q Plot YW")
qqline(data_ts.ar.r, col="red")

# Shapiro Test
shapiro.test(data_ts.ar.r)

# Incertezza
data_ts.ar.pt = predict(data_ts.ar, n.ahead = 12, se.fit = FALSE)

plot(data_ts, main = "Yule-Walker", col="black")
lines(data_ts.ar.pt, col = "red")
lines(data_ts -data_ts.ar$resid, col = "red")
lines(data_ts.ar.pt + quantile(data_ts.ar.r, 0.05), col="green3")
lines(data_ts.ar.pt + quantile(data_ts.ar.r, 0.95), col="green3")

legend(
  "bottomleft",
  legend = c( "Osservazioni", "Yule-Walker"),
  col = c("black", "red"),
  lwd = 2,
  cex = 0.6
)

# Minimi Quadrati
data_ts.ls.r = data_ts.ls$resid[23:252]

plot(data_ts.ls.r, type="p", pch=20)

# Autocorrelazione
acf(data_ts.ls.r, main="ACF Residui MQ")

# Istogramma
hist(data_ts.ls.r, 20, freq=F, main="Istogramma Residui MQ")
lines(density(data_ts.ls.r),col="blue")
lines(sort(na.omit(data_ts.ls.r)),dnorm(sort(na.omit(data_ts.ls.r)),mean(na.omit(data_ts.ls.r)),sd(na.omit(data_ts.ls.r))),col="red")

# Q-Q Plot
qqnorm(data_ts.ls.r,main = "Q-Q Plot MQ")
qqline(data_ts.ls.r, col="red")

# Shapiro Test
shapiro.test(data_ts.ls.r)

# Incertezza
data_ts.ls.pt = predict(data_ts.ls, n.ahead = 12, se.fit = FALSE)

plot(data_ts, main = "Minimi Quadrati", col="black")
lines(data_ts.ls.pt, col = "red")
lines(data_ts -data_ts.ls$resid, col = "red")
lines(data_ts.ls.pt + quantile(data_ts.ls.r, 0.05), col="green3")
lines(data_ts.ls.pt + quantile(data_ts.ls.r, 0.95), col="green3")

segments(2019,0,2019, col="black")

legend(
  "bottomleft",
  legend = c( "Osservazioni", "Minimi Quadrati"),
  col = c("black", "red"),
  lwd = 2,
  cex = 0.6
)


### AUTOVALIDAZIONE

# Previsione 2020
res.hw.1 = rep(0,11)
res.hw.2 = rep(0,11)
res.ar = rep(0,11)
res.ls = rep(0,11)

for (i in 1:11) {
  train = window(data_ts, end = c(2020, i))
  test = window(data_ts, start = c(2020, i+1))
  res.hw.1[i] = predict(HoltWinters(train, alpha = 0.3639011 , beta = 0, gamma = 0.4074314), 1)
  res.hw.2[i] = predict(HoltWinters(train, alpha=0.35, beta=0.1, gamma=0.45, l.start=1451, b.start=-0.37 ), 1)
  res.ar[i] = predict(ar(train), n.ahead = 1, se.fit = F)
  res.ls[i] = predict(ar(train, method="ols"), n.ahead = 1, se.fit = F)
}

test = window(data_ts, start=c(2020, 2))
sqrt(mean((test-res.hw.1)^2))
sqrt(mean((test-res.hw.2)^2))
sqrt(mean((test-res.ar)^2))
sqrt(mean((test-res.ls)^2))

ts.plot(test, res.hw.1, res.hw.2, res.ar, res.ls, main="Confronto 2020", col=c("black", "blue", "red", "green3", "brown"), ylim=c(400, 1800))
legend(
  "bottomright",
  legend = c( "Test", "HW Auto", "HW Man", "YW", "MQ"),
  col = c("black", "blue", "red", "green3", "brown"),
  lwd = 2,
  cex = 0.6
)

### Previsione 2019

res.hw.1 = rep(0,11)
res.hw.2 = rep(0,11)
res.ar = rep(0,11)
res.ls = rep(0,11)

for (i in 1:11) {
  train = window(data_ts, end = c(2019, i))
  test = window(data_ts, start = c(2019, i+1), end=c(2019,12))
  res.hw.1[i] = predict(HoltWinters(train, alpha = 0.3639011 , beta = 0, gamma = 0.4074314), 1)
  res.hw.2[i] = predict(HoltWinters(train, alpha=0.35, beta=0.1, gamma=0.45, l.start=1451, b.start=-0.37 ), 1)
  res.ar[i] = predict(ar(train), n.ahead = 1, se.fit = F)
  res.ls[i] = predict(ar(train, method="ols"), n.ahead = 1, se.fit = F)
}

test = window(data_ts, start=c(2019, 2), end=c(2019,12))
sqrt(mean((test-res.hw.1)^2))
sqrt(mean((test-res.hw.2)^2))
sqrt(mean((test-res.ar)^2))
sqrt(mean((test-res.ls)^2))

ts.plot(test, res.hw.1, res.hw.2, res.ar, res.ls, main="Confronto 2019", col=c("black", "blue", "red", "green3", "brown"), ylim=c(900, 1800))
legend(
  "bottom",
  legend = c( "Test", "HW Auto", "HW Man", "YW", "MQ"),
  col = c("black", "blue", "red", "green3", "brown"),
  lwd = 2,
  cex = 0.6
)

### Previsione FINALE
# Previsione Yule Walker
data_ts.ar.pt = predict(data_ts.ar, n.ahead=12, se.fit=FALSE)
data_ts.ar.r = data_ts.ar$resid[16:252]

y.max = max(data_ts.ar.pt + quantile(data_ts.ar.r, 0.95))
y.min = min(data_ts.ar.pt + quantile(data_ts.ar.r, 0.05))
ts.plot(data_ts.ar.pt, ylim=c(y.min, y.max), col="red", main="Previsione Finale")
lines(data_ts.ar.pt + quantile(data_ts.ar.r, 0.95), col = "green3")
lines(data_ts.ar.pt + quantile(data_ts.ar.r, 0.05), col = "green3")


