#LIBRERIE
library(corrplot)
library(readr)

#MANIPOLAZIONE TABELLE
#caricamento dei dati dai file csv
data1 = read.csv("tabella.csv", header = TRUE, sep = ";")

#rimuovo il nome del giocatore dalla tabella
data = data1[c("RANK", "GP", "MIN", "PTS", "FGA_PG", "FG3A", "FTA", "REB", "AST", "TOV", "STL", "BLK", "OFF_RATING", "DEF_RATING")]

# dati utilizzati per l'analisi
summary(data)
str(data)
plot(data)

# standardizzazione tabella
st_data = data.frame(scale(data))

# la standardizzazione non varia il grafico di dispersione
# e non è stata quindi considerata nel seguito dell'analisi
plot(st_data)

# grafico correlazioni
corrplot.mixed(cor(data), lower = "number", upper = "ellipse", number.cex=0.70, tl.pos = "lt", tl.cex=0.70)

# MODELLO LINEARE

#modello lineare
data.lm = lm(RANK~., data=data)
summary(data.lm)

# riduzione del modello
r=matrix(ncol = 2, nrow = 7)
r[1,] = c(summary(data.lm)$r.squared, summary(data.lm)$adj.r.squared)

data.lm2 = lm(RANK~.-OFF_RATING, data=data)
r[2,] = c(summary(data.lm2)$r.squared, summary(data.lm2)$adj.r.squared)
summary(data.lm2)

data.lm3 = lm(RANK~.-OFF_RATING-FG3A, data=data)
r[3,] = c(summary(data.lm3)$r.squared, summary(data.lm3)$adj.r.squared)
summary(data.lm3)

data.lm4 = lm(RANK~.-OFF_RATING-FG3A-FTA, data=data)
r[4,] = c(summary(data.lm4)$r.squared, summary(data.lm4)$adj.r.squared)
summary(data.lm4)

data.lm5 = lm(RANK~.-OFF_RATING-FG3A-FTA-DEF_RATING, data=data)
r[5,] = c(summary(data.lm5)$r.squared, summary(data.lm5)$adj.r.squared)
summary(data.lm5)

data.lm6 = lm(RANK~.-OFF_RATING-FG3A-FTA-DEF_RATING-TOV, data=data)
r[6,] = c(summary(data.lm6)$r.squared, summary(data.lm6)$adj.r.squared)
summary(data.lm6)

data.lm7 = lm(RANK~.-OFF_RATING-FG3A-FTA-DEF_RATING-TOV-BLK, data=data)
r[7,] = c(summary(data.lm7)$r.squared, summary(data.lm7)$adj.r.squared)
summary(data.lm7)

ymin=min(r)
ymax=max(r)
plot(r[,1],pch=19,type="b",col="red",ylim=c(ymin,ymax), ylab="Varianza spiegata", xlab = "Indice")
lines(r[,2],pch=19,type="b",col="blue")
legend("bottomleft",inset=0.02,c("varianza spiegata","varianza spiegata aggiustata"),col=c("red","blue"),pch=c(19,19),bg="gray",cex=.8)

#versione ridotta (migliore versione 4)
data = data[c("RANK", "GP", "MIN", "PTS", "FGA_PG", "REB", "AST", "TOV", "STL", "BLK", "DEF_RATING")]

#modello lineare
data.lm = lm(RANK~., data=data)
summary(data.lm)

#analisi dei residui
data.lm.r = residuals(data.lm)
plot(fitted(data.lm), data.lm.r)

#densità empirica
hist(data.lm.r, 20, freq=F)
lines(density(data.lm.r), col="red")
m = mean(data.lm.r)
s = sd(data.lm.r)
lines(sort(data.lm.r), dnorm(sort(data.lm.r),m,s))

#funzione cumulativa di probabilità
plot(ecdf(data.lm.r), pch=".")
y=seq(m-3*s,m+3*s,6*s/100)
lines(y,pnorm(y,m,s),col="red")

#distribuzione dei quantili
qqnorm(data.lm.r)
qqline(data.lm.r)

#Shapiro-Wilk
shapiro.test(data.lm.r)

#Skewness
mean(((data.lm.r-mean(data.lm.r))/sd(data.lm.r))^3)

#kurtosi
mean(((data.lm.r-mean(data.lm.r))/sd(data.lm.r))^4) - 3

# salvo il modello lineare per la predizione
linear_model = data

#rimuovo outlier
boxplot(data.lm.r, main="Boxplot residui iniziali", col=(c("gold","darkgreen")), outcol="red")
Residuals_outliers <- boxplot(data.lm.r, plot=FALSE)$out
Residuals_outliers <- rev(sort(Residuals_outliers))
data = data[c(-which(data.lm.r %in% Residuals_outliers[1:length(Residuals_outliers)])),]

data.lm = lm(RANK~., data=data)
summary(data.lm)

#residui
data.lm.r = residuals(data.lm)

#Shapiro-Wilk senza outlier
shapiro.test(data.lm.r)

#MODELLO LOGARITMICO

ldata = data1

#rimuovo il nome del giocatore dalla tabella
ldata = ldata[c("RANK", "GP", "MIN", "PTS", "FGA_PG", "FG3A", "FTA", "REB", "AST", "TOV", "STL", "BLK", "OFF_RATING", "DEF_RATING")]

#metto 0.001 al posto di 0 per non avere infinito
ldata = replace(ldata, ldata == 0, 0.001)
ldata=log(ldata)
ldata.lm = lm(RANK~., data=ldata)
summary(ldata.lm)

#riduzione del modello
lr=matrix(ncol = 2, nrow = 10)
lr[1,] = c(summary(ldata.lm)$r.squared, summary(ldata.lm)$adj.r.squared)

ldata.lm2 = lm(RANK~.-REB, data=ldata)
lr[2,] = c(summary(ldata.lm2)$r.squared, summary(ldata.lm2)$adj.r.squared)
summary(ldata.lm2)

ldata.lm3 = lm(RANK~.-REB-GP, data=ldata)
lr[3,] = c(summary(ldata.lm3)$r.squared, summary(ldata.lm3)$adj.r.squared)
summary(ldata.lm3)

ldata.lm4 = lm(RANK~.-REB-GP-PTS, data=ldata)
lr[4,] = c(summary(ldata.lm4)$r.squared, summary(ldata.lm4)$adj.r.squared)
summary(ldata.lm4)

ldata.lm5 = lm(RANK~.-REB-GP-PTS-BLK, data=ldata)
lr[5,] = c(summary(ldata.lm5)$r.squared, summary(ldata.lm5)$adj.r.squared)
summary(ldata.lm5)

ldata.lm6 = lm(RANK~.-REB-GP-PTS-BLK-AST, data=ldata)
lr[6,] = c(summary(ldata.lm6)$r.squared, summary(ldata.lm6)$adj.r.squared)
summary(ldata.lm6)

ldata.lm7 = lm(RANK~.-REB-GP-PTS-BLK-AST-OFF_RATING, data=ldata)
lr[7,] = c(summary(ldata.lm7)$r.squared, summary(ldata.lm7)$adj.r.squared)
summary(ldata.lm7)

ldata.lm8 = lm(RANK~.-REB-GP-PTS-BLK-AST-OFF_RATING-FGA_PG, data=ldata)
lr[8,] = c(summary(ldata.lm8)$r.squared, summary(ldata.lm8)$adj.r.squared)
summary(ldata.lm8)

ldata.lm9 = lm(RANK~.-REB-GP-PTS-BLK-AST-OFF_RATING-FGA_PG-FG3A, data=ldata)
lr[9,] = c(summary(ldata.lm9)$r.squared, summary(ldata.lm9)$adj.r.squared)
summary(ldata.lm9)

ldata.lm10 = lm(RANK~.-REB-GP-PTS-BLK-AST-OFF_RATING-FGA_PG-FG3A-DEF_RATING, data=ldata)
lr[10,] = c(summary(ldata.lm10)$r.squared, summary(ldata.lm10)$adj.r.squared)
summary(ldata.lm10)

ymin=min(lr)
ymax=max(lr)
plot(lr[,1],pch=19,type="b",col="red",ylim=c(ymin,ymax), ylab="Varianza spiegata", xlab = "Indice")
lines(lr[,2],pch=19,type="b",col="blue")
legend("bottomleft",inset=0.02,c("varianza spiegata","varianza spiegata aggiustata"),col=c("red","blue"),pch=c(19,19),bg="gray",cex=.8)

#modello ridotto (6)
ldata = ldata[c("RANK", "MIN", "FGA_PG", "FG3A", "FTA", "TOV", "STL", "OFF_RATING", "DEF_RATING")]

#modello lineare
ldata.lm = lm(RANK~., data=ldata)
summary(ldata.lm)

#residui
ldata.lm.r = residuals(ldata.lm)
plot(fitted(ldata.lm), ldata.lm.r)

#densità empirica
hist(ldata.lm.r, 20, freq=F)
lines(density(ldata.lm.r), col="red")
m = mean(ldata.lm.r)
s = sd(ldata.lm.r)
lines(sort(ldata.lm.r), dnorm(sort(ldata.lm.r),m,s))

#funzione cumulativa di probabilità
plot(ecdf(ldata.lm.r), pch=".")
y=seq(m-3*s,m+3*s,6*s/100)
lines(y,pnorm(y,m,s),col="red")

#distribuzione dei quantili
qqnorm(ldata.lm.r)
qqline(ldata.lm.r)

#Shapiro-Wilk
shapiro.test(ldata.lm.r)

# salvo modello logaritmico per predizione
log_model = ldata

# elimino outlier
boxplot(ldata.lm.r, main="Boxplot residui iniziali", col=(c("gold","darkgreen")), outcol="red")
Residuals_outliers <- boxplot(ldata.lm.r, plot=FALSE)$out
Residuals_outliers <- rev(sort(Residuals_outliers))
ldata = ldata[c(-which(ldata.lm.r %in% Residuals_outliers[1:length(Residuals_outliers)])),]

ldata.lm = lm(RANK~., data=ldata)
summary(ldata.lm)
ldata.lm.r = residuals(ldata.lm)

#Shapiro-Wilk senza outlier
shapiro.test(ldata.lm.r)

#VALIDAZIONE

data=linear_model
ldata=log_model
l=nrow(data)

m=100 # numero iterazioni
s_err=rep(0,m)
l_err=rep(0,m)

for(j in 1:m){
  # selezione del test set (uguale nelle due stime per aiutare il confronto)
  idx = sample(l,40)
  # modello lineare
  strain = data[-idx,]
  stest = data[idx,]
  st.lm=lm(RANK~.,data=strain)
  st.p=predict(st.lm,stest)
  # modello nonlineare
  ltrain = ldata[-idx,]
  ltest = ldata[idx,]
  lt.lm=lm(RANK~.,data=ltrain)
  lt.p=predict(lt.lm,ltest)
  # errore medio
  s_err[j] = sqrt(mean((st.p - stest$RANK)^2))
  l_err[j] = sqrt(mean((exp(lt.p) - exp(ltest$RANK))^2))
}

#media errori
mean(s_err)
mean(l_err)

#deviazione standard errori
sd(s_err)
sd(l_err)

# rappresentazione grafica errori dei due modelli
par(mfrow=c(1,1))
gmin=min(s_err,l_err)
gmax=max(s_err,l_err)
plot(s_err,type="b", pch=20, col="blue", ylim=c(gmin, gmax+1),
     ylab="Errore", xlab="Iterazione")
points(l_err,type="b", pch=20, col="red")
legend("topright", inset = c(0, 0), c("Lineare", "Logaritmico"),
       col = c("blue", "red"), pch = c(19,19), cex=0.7, bty="n")

#STIMA DELLE INCERTEZZE
# previsione modello lineare
data<-data[order(data$RANK),]
data.lm=lm(RANK~.,data=data)
alpha=0.95

#utilizzo ogni dato come nuovo dato per la stima degli intervalli
n=nrow(data)
data.cil=matrix(rep(0,3*n),n,3) # per gli intervalli di confidenza parametrici
data.pil=matrix(rep(0,3*n),n,3) # per gli intervalli di previsione parametrici
data.nil=matrix(rep(0,3*n),n,3) # per gli intervalli non parametrici
for(i in 1:n){
  tr=data[-i,]
  nd=data[i,]
  alm=lm(RANK~.,data=tr)
  data.cil[i,]=predict(alm,nd,interval="confidence",level=alpha)
  data.pil[i,]=predict(alm,nd,interval="prediction",level=alpha)
  data.nil[i,]=predict(alm,nd)+c(0,quantile(resid(alm),(1-alpha)/2),quantile(resid(alm),(1+alpha)/2))
}

ymin=min(data.pil)
ymax=max(data$RANK)
plot(data$RANK,pch=20,ylim=c(ymin,ymax),ylab = "Ranting",xlab = "Indice") # valori veri
# intervalli di confidenza parametrici
lines(data.cil[,2],col="red",lwd=2) # stima inferiore
lines(data.cil[,3],col="red",lwd=2) # stima superiore
# intervalli di predizione parametrici
lines(data.pil[,2],col="blue",lwd=2) # stima inferiore
lines(data.pil[,3],col="blue",lwd=2) # stima superiore
# intervalli non parametrici
lines(data.nil[,2],col="green4",lwd=2)
lines(data.nil[,3],col="green4",lwd=2)
points(data.nil[,1],col="cyan", pch=20)
legend("topleft", inset = c(0, 0), c("confidenza parametrici", "predizione parametrici", "non parametrici"),
       col = c("red", "blue", "green4"), pch = c(19,19), cex=0.7, bty="n")

#stima delle incertezze nella previsione
alpha=0.95
u=sample(416,40)
dataa=data[-u,]
datab=data[u,]
dataa.lm=lm(RANK~.,data=dataa)

#ricavo intervalli
# intervalli di confidenza
datab.ci=predict(dataa.lm,datab,interval="confidence",level=alpha)
# intervalli di predizione
datab.pi=predict(dataa.lm,datab,interval="prediction",level=alpha)
# intervalli di confidenza empirici
dataa.r=resid(dataa.lm)
qi=quantile(dataa.r,(1-alpha)/2)
qs=quantile(dataa.r,(1+alpha)/2)

#rappresento graficamente il risultato
##
ymin=min(c(datab.pi[,2],datab.ci[,2]))
ymax=max(c(datab.pi[,3],datab.ci[,3]))
plot(datab$RANK,ylim=c(ymin,ymax),ylab = "Ranting",xlab = "Indice")
x=1:40
###
# intervalli di predizione
segments(x,datab.pi[,2],x,datab.pi[,3],col="blue",lwd=18)
# intervalli di confidenza empirici
segments(x,datab.pi[,1]+qi,x,datab.pi[,1]+qs,col="green4",lwd=12)
# intervalli di confidenza parametrici
segments(x,datab.ci[,2],x,datab.ci[,3],col="red",lwd=6)
# valori stimati
points(x,datab.pi[,1],pch=19,col="white",cex=1.5)
points(datab$RANK,pch=19,col="cyan",cex=1.5)
legend("topright", inset = c(0, 0), c("predizione", "confidenza empirici", "confidenza parametrici"),
       col = c("blue", "green4", "red"), pch = c(19,19), cex=0.7, bty="n")
