#path.to.fasta<-"/Users/herpworld/Desktop/coexp_scripts/dryad/4_exp_stats"
library(pegas)
library(ape)
library(PipeMaster)

setwd(path.to.fasta)

y<-list.files(pattern = ".fas")

tab<-NULL

for (j in 1:length(y)){
  x<-read.dna(y[j], format="fasta")
  pi<-nuc.div(x)
  TD<-tajima.test(x)$D
  TDp<-tajima.test(x)$Pval.normal
  H<-H.div(x)
  R2<-R2.test(x)$R2
  R2p<-R2.test(x)$P.val
  
  sumstat<-c(pi[[1]],H,TD[1],TDp[1],R2[1],R2p[1])
  tab<-rbind(tab,sumstat)
}
rownames(tab)<-y
colnames(tab)<-c("Pi","num.Hap", "Hap.div","TD", "TD.pval", "R2", "R2.pval")
write.csv(tab, "exp_stats.csv")
