library(corrplot)
library(caret)
library(MASS)
require(multiROC)
source("s2_helper.r")
library(readr)

# caricamento dei dati dal file csv
data1 = read.csv('tabella.csv', header = TRUE, sep = ";")

#tabella finale
data = data1[c("GP", "MIN", "PTS", "FGA_PG", "FG3A", "FTA", "REB", "AST", "TOV", "STL", "BLK", "OFF_RATING", "DEF_RATING", "CONFIRMED")]

#salvo la tabella
final = data

#distribuzione classe
with(data, table(CONFIRMED))

#diagramma di dispersione
plot(data,pch=20,col=1+data$CONFIRMED)

################ Regressione logisitca ################

data = final

data.glm=glm(CONFIRMED~.,data=data,family=binomial)

data.glm.p=predict(data.glm,type="response")
#accuratezza
sum((data.glm.p>0.5)==(data$CONFIRMED>0.5))/length(data$CONFIRMED)
s2_confusion(data$CONFIRMED,data.glm.p)
data.glm.roc = s2_roc(data$CONFIRMED,data.glm.p)


################ Analisi Discriminante Lineare ################

data.lda = lda(CONFIRMED~., data=data,CV=F)

data.lda.p=predict(data.lda)
data.lda.post=data.lda.p$posterior[,2]
sum((data.lda.post>0.5)==(data$CONFIRMED>0.5))/length(data$CONFIRMED)

s2_confusion(data$CONFIRMED,data.lda.post)

data.lda.roc=s2_roc(data$CONFIRMED,data.lda.post)

################ Analisi Discriminante Quadratica ################

data.qda = qda(CONFIRMED~., data=data,CV=F)

data.qda.p=predict(data.qda)
data.qda.post=data.qda.p$posterior[,2]
sum((data.qda.post>0.5)==(data$CONFIRMED>0.5))/length(data$CONFIRMED)

s2_confusion(data$CONFIRMED,data.qda.post)

data.qda.roc=s2_roc(data$CONFIRMED,data.qda.post)

################ Confronto ROC e AUC ################

s2_roc.plot(data.qda.roc,col="blue")
s2_roc.lines(data.lda.roc,col="green3")
s2_roc.lines(data.glm.roc,col="red")

legend("bottomright", 
        inset = 0.02, 
        c("QDA", "LDA", "Regressione Logistica"), 
        col = c("blue", "green3", "red"), 
        pch = 19,
        bg = "gray",
        cex=0.8)

#qda
s2_auc(data.qda.roc)
#lda
s2_auc(data.lda.roc)
#regressione logistica
s2_auc(data.glm.roc)

################ Robustezza ###############

idx=sample(605,605)
acc=matrix(0,605, 3)
for(i in 1:605){
  dataf=data
  dataf$CONFIRMED[idx[1:i]]=as.numeric(!dataf$CONFIRMED[idx[1:i]])

  data.glm=glm(CONFIRMED~.,data=dataf,family=binomial)
  data.glm.p=predict(data.glm,type="response")
  acc[i,1]=sum((data.glm.p>0.5)==(data$CONFIRMED>0.5))/length(data$CONFIRMED)

  data.lda = lda(CONFIRMED~., data=dataf,CV=F)
  data.lda.p=predict(data.lda)
  data.lda.post=data.lda.p$posterior[,2]
  acc[i,2]=sum((data.lda.post>0.5)==(data$CONFIRMED>0.5))/length(data$CONFIRMED)

  data.qda = qda(CONFIRMED~., data=dataf,CV=F)
  data.qda.p=predict(data.qda)
  data.qda.post=data.qda.p$posterior[,2]
  acc[i,3]=sum((data.qda.post>0.5)==(data$CONFIRMED>0.5))/length(data$CONFIRMED)
}

#regressione logistica
plot(acc[,1], type='l', col="red", ylim=c(0,1), xlab="Indici scambiati", ylab="Accuratezza", main="Robustezza")
#lda
lines(acc[,2], type='l', col="green3")
#qda
lines(acc[,3], type='l', col="blue")

legend("bottomleft", 
        inset = 0.02, 
        c("QDA", "LDA", "Regressione Logistica"), 
        col = c("blue", "green3", "red"), 
        pch = 19,
        bg = "gray",
        cex=0.8)

################ Autovalutazione ################
library(pROC)
l=length(data$CONFIRMED)
acc=matrix(0,100,3)
auc=matrix(0,100,3)
for(i in 1:100){

  idx=sample(l,100)
  datatrain=data[-idx,]

  data.glm=glm(CONFIRMED~.,family=binomial,data=datatrain)
  data.glm.p=predict(data.glm,data[idx,],type="response")
  acc[i,1]=sum((data.glm.p>0.5)==(data$CONFIRMED[idx]>0.5))/100
  auc[i,1]=roc(data$CONFIRMED[idx], data.glm.p)$auc

  data.lda=lda(CONFIRMED~.,data=datatrain)
  data.lda.p=predict(data.lda,data[idx,])$posterior[,2]
  acc[i,2]=sum((data.lda.p>0.5)==(data$CONFIRMED[idx]>0.5))/100
  auc[i,2]=roc(data$CONFIRMED[idx], data.lda.p)$auc

  data.qda=qda(CONFIRMED~.,data=datatrain)
  data.qda.p=predict(data.qda,data[idx,])$posterior[,2]
  acc[i,3]=sum((data.qda.p>0.5)==(data$CONFIRMED[idx]>0.5))/100
  auc[i,3]=roc(data$CONFIRMED[idx], data.qda.p)$auc
  
}

# regressione logistica
mean(acc[,1])
sd(acc[,1])

mean(auc[,1])
sd(auc[,1])

# lda
mean(acc[,2])
sd(acc[,2])

mean(auc[,2])
sd(auc[,2])

# qda
mean(acc[,3])
sd(acc[,3])

mean(auc[,3])
sd(auc[,3])

# risultati
boxplot(acc, main="Boxplot Accuracy", xlab="Classification Methods", ylab="Accuracy", xaxt="n")
axis(1,at=1:3,labels=c("glm", "lda", "qda"))

boxplot(auc, main="Boxplot AUC", xlab="Classification Methods", ylab="AUC", xaxt="n")
axis(1,at=1:3,labels=c("glm", "lda", "qda"))

wilcox.test(acc[,1], acc[,2])

#regressione logistica
plot(acc[,1], type='l', col="red", ylim=c(0.6,1), xlab="Esecuzione", ylab="Accuratezza", main="Cross-validation")
#lda
lines(acc[,2], type='l', col="green3")
#qda
lines(acc[,3], type='l', col="blue")

legend("bottomleft", 
        inset = 0.02, 
        c("QDA", "LDA", "Regressione Logistica"), 
        col = c("blue", "green3", "red"), 
        pch = 19,
        bg = "gray",
        cex=0.8)

################ Sensibilità ################

l=length(data$CONFIRMED)

meanAcc=matrix(0,20,3)
meanSens=matrix(0,20,3)
meanSpec=matrix(0,20,3)
prob=rep(0,20)

for(j in 1:20){

  acc=matrix(0,70,3)
  sens=matrix(0,70,3)
  spec=matrix(0,70,3)

  p = 0 + ((j-1)*0.05)
  prob[j]=p

  for(i in 1:70){

    idx=sample(l,70)
    datatrain=data[-idx,]

    data.glm=glm(CONFIRMED~.,family=binomial,data=datatrain)
    data.glm.p=predict(data.glm,data[idx,],type="response")

    acc[i,1]=sum((data.glm.p>p)==(data$CONFIRMED[idx]>p))/70
    tp=sum((data.glm.p>p)&(data$CONFIRMED[idx]==1))
    fn=sum((data.glm.p<p)&(data$CONFIRMED[idx]==1))
    sens[i,1]=tp/(tp+fn)

    tn=sum((data.glm.p<p)&(data$CONFIRMED[idx]==0))
    fp=sum((data.glm.p>p)&(data$CONFIRMED[idx]==0))
    spec[i,1]=tn/(tn+fp)

    data.lda=lda(CONFIRMED~.,data=datatrain)
    data.lda.p=predict(data.lda,data[idx,])$posterior[,2]
    acc[i,2]=sum((data.lda.p>p)==(data$CONFIRMED[idx]>p))/70

    tp=sum((data.lda.p>p)&(data$CONFIRMED[idx]==1))
    fn=sum((data.lda.p<p)&(data$CONFIRMED[idx]==1))
    sens[i,2]=tp/(tp+fn)

    tn=sum((data.lda.p<p)&(data$CONFIRMED[idx]==0))
    fp=sum((data.lda.p>p)&(data$CONFIRMED[idx]==0))
    spec[i,2]=tn/(tn+fp)

    data.qda=qda(CONFIRMED~.,data=datatrain)
    data.qda.p=predict(data.qda,data[idx,])$posterior[,2]
    acc[i,3]=sum((data.qda.p>p)==(data$CONFIRMED[idx]>p))/70

    tp=sum((data.qda.p>p)&(data$CONFIRMED[idx]==1))
    fn=sum((data.qda.p<p)&(data$CONFIRMED[idx]==1))
    sens[i,3]=tp/(tp+fn)

    tn=sum((data.qda.p<p)&(data$CONFIRMED[idx]==0))
    fp=sum((data.qda.p>p)&(data$CONFIRMED[idx]==0))
    spec[i,3]=tn/(tn+fp)
  }

  #logistica
  meanAcc[j,1]=mean(acc[i,1])
  meanAcc[j,2]=mean(acc[,2])
  meanAcc[j,3]=mean(acc[,3])

  #lda
  meanSens[j,1]=mean(sens[,1])
  meanSens[j,2]=mean(sens[,2])
  meanSens[j,3]=mean(sens[,3])

  #qda
  meanSpec[j,1]=mean(spec[,1])
  meanSpec[j,2]=mean(spec[,2])
  meanSpec[j,3]=mean(spec[,3])

}


#regressione logistica
plot(prob, meanAcc[,1], type='l', col="red", ylim=c(0.2,1), xlab="Soglia", ylab="Accuratezza", main="Accuratezza")
#lda
lines(prob, meanAcc[,2], type='l', col="green3")
#qda
lines(prob, meanAcc[,3], type='l', col="blue")

legend("bottomleft", 
        inset = 0.02, 
        c("QDA", "LDA", "Regressione Logistica"), 
        col = c("blue", "green3", "red"), 
        pch = 19,
        bg = "gray",
        cex=0.8)

#regressione logistica
plot(prob, meanSens[,1],  type='l', col="coral", ylim=c(0.4,1), xlab="Soglia", ylab="Sensibilità-Specificità", main="Sensibilità-Specificità")
#lda
lines(prob, meanSens[,2], type='l', col="chartreuse")
#qda
lines(prob, meanSens[,3], type='l', col="cyan")
#regressione logistica
lines(prob, meanSpec[,1], type='l', col="red")
#lda
lines(prob, meanSpec[,2], type='l', col="green3")
#qda
lines(prob, meanSpec[,3], type='l', col="blue")

legend("bottomright", 
        inset = 0.02, 
        c("Sensibilità QDA", "Sensibilità LDA", "Sensibilità Reg. Log.", "Specificità QDA", "Specificità LDA", "Specificità Reg. Log."), 
        col = c("cyan", "chartreuse", "coral","blue", "green3", "red"), 
        pch = 19,
        bg = "gray",
        cex=0.8)