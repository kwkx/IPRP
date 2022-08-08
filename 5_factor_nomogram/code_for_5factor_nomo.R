setwd("C:\\Users\\15103\\Downloads\\IPRP-main\\5_factor_nomogram")###change the path to "5_factor_nomogram" file.
load("5_factor_nomogram.RData")
library(survival)
library(survminer)
library(timeROC)
library(rms)
library(regplot)
library(survivalROC)



# risk=read.table(riskFile, header=T, sep="\t", check.names=F, row.names=1)
# risk=risk[,c("futime", "fustat", "risk")]
# 
# 
# cli=read.table(cliFile, header=T, sep="\t", check.names=F, row.names=1)
# cli=cli[apply(cli,1,function(x)any(is.na(match('unknow',x)))),,drop=F]
# #cli$age=as.numeric(cli$age)
# 
# #combine data
# samSample=intersect(row.names(risk), row.names(cli))
# risk1=risk[samSample,,drop=F]

#############
riskFile="testRisk.txt"     #
cliFile="clinical.txt"   
#
risk=read.table(riskFile, header=T, sep="\t", check.names=F, row.names=1)
risk=risk[,c("futime", "fustat", "risk")]


cli=read.table(cliFile, header=T, sep="\t", check.names=F, row.names=1)
cli=cli[apply(cli,1,function(x)any(is.na(match('unknow',x)))),,drop=F]
#cli$age=as.numeric(cli$age)

#combine data
samSample=intersect(row.names(risk), row.names(cli))
risk1=risk[samSample,,drop=F]
cli=cli[samSample,,drop=F]
rt=cbind(risk1, cli)


nomoRisk=predict(res.cox,newdata = rt, type="risk")
rt$nomoRisk=nomoRisk
ROC_rt=timeROC(T=rt$futime, delta=rt$fustat,
               marker=rt$nomoRisk, cause=1,
               weighting='aalen',
               times=c(1,2,3), ROC=TRUE)
pdf(file="exampleROC.pdf", width=5, height=5)
plot(ROC_rt,time=1,col='green',title=FALSE,lwd=2)
plot(ROC_rt,time=2,col='blue',add=TRUE,title=FALSE,lwd=2)
plot(ROC_rt,time=3,col='red',add=TRUE,title=FALSE,lwd=2)
legend('bottomright',
       c(paste0('AUC at 1 years: ',sprintf("%.03f",ROC_rt$AUC[1])),
         paste0('AUC at 2 years: ',sprintf("%.03f",ROC_rt$AUC[2])),
         paste0('AUC at 3 years: ',sprintf("%.03f",ROC_rt$AUC[3]))
       ),
       col=c("green","blue",'red'),lwd=2,bty = 'n')
dev.off()

fix(rt)

##below is the permutation test process of ROC curve
permutation_test=function(survivetime,survivestate,markervalue)
{
  roc1=survivalROC(Stime=survivetime, survivestate, marker = markervalue, predict.time =1, method="KM")
  
  if(roc1$AUC>=0.5)
  {
    #roc1=survivalROC(Stime=survivetime, status=survivestate, marker = markervalue, predict.time =1, method="KM")
    result=c()
    for(i in 1:300)
    {
      roc=survivalROC(Stime=survivetime, status=survivestate,marker = sample(markervalue,length(markervalue)), predict.time =1, method="KM")
      result=c(result,roc$AUC)
      print(paste0("first_year",i,"turn"))
    }
    
    p1=length(which(result[]>roc1$AUC))/300
  }
  if(roc1$AUC<0.5)
  {
    roc1=survivalROC(Stime=survivetime, status=survivestate, marker = -markervalue, predict.time =1, method="KM")
    result=c()
    for(i in 1:300)
    {
      roc=survivalROC(Stime=survivetime, status=survivestate,marker = sample(-markervalue,length(-markervalue)), predict.time =1, method="KM")
      result=c(result,roc$AUC)
      print(paste0("first_year",i,"turn"))
    }
    
    p1=length(which(result[]>roc1$AUC))/300
  }
  
  
  
  
  
  roc3=survivalROC(Stime=survivetime, survivestate, marker = markervalue, predict.time =2, method="KM")
  if(roc3$AUC>=0.5)
  {
    #roc3=survivalROC(Stime=survivetime, status=survivestate, marker = markervalue, predict.time =3, method="KM")
    result=c()
    for(i in 1:300)
    {
      roc=survivalROC(Stime=survivetime, status=survivestate,marker = sample(markervalue,length(markervalue)), predict.time =2, method="KM")
      result=c(result,roc$AUC)
      print(paste0("secound_year",i,"turn"))
    }
    
    p3=length(which(result[]>roc3$AUC))/300
  }
  if(roc3$AUC<0.5)
  {
    roc3=survivalROC(Stime=survivetime, status=survivestate, marker = -markervalue, predict.time =2, method="KM")
    result=c()
    for(i in 1:300)
    {
      roc=survivalROC(Stime=survivetime, status=survivestate,marker = sample(-markervalue,length(-markervalue)), predict.time =2, method="KM")
      result=c(result,roc$AUC)
      print(paste0("secound_year",i,"turn"))
    }
    
    p3=length(which(result[]>roc3$AUC))/300
  }
  
  
  
  
  
  
  roc5=survivalROC(Stime=survivetime, survivestate, marker = markervalue, predict.time =3, method="KM")
  
  if(roc5$AUC>=0.5)
  {
    #roc3=survivalROC(Stime=survivetime, status=survivestate, marker = markervalue, predict.time =3, method="KM")
    result=c()
    for(i in 1:300)
    {
      roc=survivalROC(Stime=survivetime, status=survivestate,marker = sample(markervalue,length(markervalue)), predict.time =3, method="KM")
      result=c(result,roc$AUC)
      print(paste0("third_year",i,"turn"))
    }
    
    p5=length(which(result[]>roc5$AUC))/300
  }
  if(roc5$AUC<0.5)
  {
    roc5=survivalROC(Stime=survivetime, status=survivestate, marker = -markervalue, predict.time =3, method="KM")
    result=c()
    for(i in 1:300)
    {
      roc=survivalROC(Stime=survivetime, status=survivestate,marker = sample(-markervalue,length(-markervalue)), predict.time =3, method="KM")
      result=c(result,roc$AUC)
      print(paste0("third_year",i,"turn"))
    }
    
    p5=length(which(result[]>roc5$AUC))/300
  }
  
  
  c(p1,p3,p5)
  
  
}


permutation_test(rt$futime,rt$fustat,rt$nomoRisk)###finally we will get 3 p values for the 1st, 2nd and 3rd years.






