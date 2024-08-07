# ROC curve
s2_roc <- function(response,predictor){
  num=1000
  rpredictor<-round(predictor,log10(num))
  roc=matrix(rep(3*(num+1),0),num+1,3)
  for(i in 0:num){
    p=i/num
    TrueNegative=sum((rpredictor<p)&(response==0))
    TruePositive=sum((rpredictor>p)&(response==1))
    FalsePositive=sum((rpredictor>p)&(response==0))
    FalseNegative=sum((rpredictor<p)&(response==1))
    sens=if(TruePositive==0)0 else TruePositive/(TruePositive+FalseNegative)
    spec=if(TrueNegative==0)0 else TrueNegative/(TrueNegative+FalsePositive)
    roc[i+1,]=c(spec,sens,p)
  }
  roc<-data.frame(roc)
  colnames(roc)<-c("specificitÃ ","sensibilitÃ ","soglia")
  roc
}

s2_roc.plot<-function(roc,...){
  plot(roc[,1:2], type="l",main="curva ROC",
       ylab="sensibilitÃ  (tasso positivi veri)",xlab="specificitÃ  (tasso negativi veri)",
       xlim=c(1,0),ylim=c(0,1),...)
  segments(0,1,1,0,col="red",lwd=2,lty="dashed")
}

s2_roc.lines<-function(roc,...){
  lines(roc[,1:2], type="l",main="curva ROC",
        ylab="sensibilitÃ  (tasso positivi veri)",xlab="specificitÃ  (tasso negativi veri)",
        xlim=c(1,0),ylim=c(0,1),...)
}

# confusion matrix su classificazione perentoria
s2_pconfusion <- function(response,predictor){
  cm=matrix(nrow=2,ncol=2)
  
  # binaria
  cm[1,1]=sum((response==predictor)&(response==1))
  cm[1,2]=sum((response!=predictor)&(response==0))
  cm[2,1]=sum((response!=predictor)&(response==1))
  cm[2,2]=sum((response==predictor)&(response==0))
  
  cm<-data.frame(cm)
  rownames(cm)<-c("predicted 1","predicted 0")
  colnames(cm)<-c("actual 1","actual 0")
  cm
}

s2_auc <- function(roc){
  l=nrow(roc)
  ls=sum(roc[1:l-1,1]*(roc[1:(l-1),2]-roc[2:l,2]))
  us=sum(roc[2:l,1]*(roc[1:(l-1),2]-roc[2:l,2]))
  (ls+us)/2
}

# confusion matrix su posterior probabilities
s2_confusion <- function(response,predictor,p=0.5){
  cm=matrix(nrow=2,ncol=2)
  
  # binaria
  cm[1,1]=sum((predictor>=p)&(response==1)) # true positive
  cm[1,2]=sum((predictor>=p)&(response==0)) # false positive
  cm[2,1]=sum((predictor<=p)&(response==1)) # false negative
  cm[2,2]=sum((predictor<=p)&(response==0)) # true negative
  
  cm<-data.frame(cm)
  rownames(cm)<-c("predicted 1","predicted 0")
  colnames(cm)<-c("actual 1","actual 0")
  cm
}


# multiclasse
#for(i in 0:1){
#  for(j in 0:1){
#    cm[i+1,j+1]=sum((bn$class==i)&(bn.lda.p$class==j))
#  }
#}

# table che massimizza i valori diagonali
s2_table <- function(ca,cb){
  fca<-as.factor(ca)
  fcb<-as.factor(cb)
  t<-table(fca,fcb)
  s<-max.col(t,"first")
  fcc<-factor(fcb,levels=levels(fcb)[s])
  table(fca,fcc)
}
