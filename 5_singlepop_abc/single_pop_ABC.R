library(devtools)
library(ape)
library(pegas)
library(e1071)
library(PopGenome)
library(abc)
library(phyclust)
library(ggplot2)
library(PipeMaster)

setwd("~/Desktop/Arianna/take3")
observed<-observed.demog.sumstat("~/Desktop/Arianna/take3/observed")

setwd("~/Desktop/Arianna/take3/priors")
time.prior<-read.table("exp.time.txt",header=T)
Ne.prior<-read.table("Ne.txt",header=T)
NeA.prior<-read.table("NeA.txt",header=T)
gene.prior<-read.table("gene.txt",header=T)

Ne.prior[,3]<-1000
Ne.prior[,4]<-1000000

NeA.prior[,3]<-.01
NeA.prior[,4]<-.5

#x<-test.demog(nsims=500000,Ne.prior = Ne.prior[41:42,],time.prior = time.prior[41:42,],
#              gene.prior=gene.prior[41:42,],tol=0.01,path=getwd(),observed = observed[41:42,],alpha=F, nval=10, method="rejection",do.ABC = F,do.PCA = T,CV=F)


single.pop.demog<-(nsims=500000,
                           Ne.prior=Ne.prior,
                           time.prior=time.prior,
                           gene.prior=gene.prior,
                           observed=observed,
                           alpha=F,
                           tol=0.01,
                           nval=100,
                           do.ABC=F,
                           do.PCA=T,
                           CV=F,
                           mod=cbind(c(1,0.001,2),c(1,0.1,20)),
                           path=path)