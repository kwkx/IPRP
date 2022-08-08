#UTF-8 encoding

setwd("C:\\Users\\15103\\Downloads\\IPRP-main\\IPRP")####please change this path to the "IPRP" file!
load(".\\IPRPmodel.RData")

library(limma)
library(survival)
library(survivalROC)
library(survminer)
namedata=c("data")#
for (ID in namedata) {
  cox_gene_list=read.table("cox_gene_list.txt", header=T, sep="\t", check.names=F)
  cox_pair_list=read.table("cox_pair_list.txt", header=T, sep="\t", check.names=F)
  expdata=read.table("data.txt", header=T, sep="\t", check.names=F,row.names = 1)
  out=data.frame()
  for(i in 1:nrow(cox_gene_list))
  {
    num=which(row.names(expdata)[]==cox_gene_list[i,1])
    out=rbind(out,expdata[num,])
    
  }
  #expression data is in "out", then we are going to pair these genes
  tcgaPair=data.frame()
  rt=out
  sampleNum=ncol(rt)
  for(i in 1:(nrow(rt)-1)){
    for(j in (i+1):nrow(rt)){
      pair=ifelse(rt[i,]>rt[j,], 1, 0)
      rownames(pair)=paste0(rownames(rt)[i],"|",rownames(rt)[j])
      tcgaPair=rbind(tcgaPair, pair)
    }
  }
  tcgaOut=rbind(ID=colnames(tcgaPair), tcgaPair)
  tcgaOut=tcgaOut[which(rownames(tcgaOut)[]%in%cox_pair_list[,1]),]
  tcgaOut=t(tcgaOut)
  cli=read.table("cli.txt", header=T, sep="\t", check.names=F) ####input survival data of these sets
  intername=intersect(cli[,1],row.names(tcgaOut))
  cli=cli[which(cli[,1]%in%intername),]
  tcgaOut=tcgaOut[intername,]
  fustat=vector(length = nrow(tcgaOut))
  futime=vector(length = nrow(tcgaOut))
  for(i in 1:nrow(tcgaOut))
  {
    
    
    num=which(cli[,1]==rownames(tcgaOut)[i])
    fustat[i]=cli[num,3]
    futime[i]=cli[num,2]
    
  }
  tcgaOut=cbind(tcgaOut,fustat,futime)
  tcgaOut[,"futime"]=as.numeric(tcgaOut[,"futime"])/365 ####we used "year" in survival time
  tcgaOut[,"fustat"]=as.numeric(tcgaOut[,"fustat"])
  tcgaOut=as.data.frame(tcgaOut)
  for(i in 1:ncol(tcgaOut))
  {
    tcgaOut[,i]=as.numeric(as.character(tcgaOut[,i]))
  }
  #riskScoreTest=predict(multiCox,type="risk",newdata=tcgaOut) 
  riskScoreTest=iprpscore(tcgaOut)
  medianTrainRisk=0.684
  riskTest=as.vector(ifelse(riskScoreTest>medianTrainRisk,"high","low"))
  riskfile=cbind(id=rownames(cbind(tcgaOut,riskScoreTest,riskTest)),cbind(tcgaOut,risk=riskTest))
  write.table(riskfile, file="riskgroup.txt", sep="\t", quote=F, col.names=T,row.names = F)
  
}


