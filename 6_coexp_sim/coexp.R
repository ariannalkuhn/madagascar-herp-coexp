library(pegas)
library(PipeMaster)
library(ape)
library(e1071)
library(PopGenome)
library(phyclust)

Ne.prior<-read.table(file="Ne.txt", header=T)
NeA.prior<-read.table(file="NeA.txt", header=T)
time.prior<-read.table(file="exp.time.txt", header=T)
gene.prior<-read.table(file="gene.txt", header=T)

#Chan et al. (2014) model
sim.coexp(nsims=400000,var.zeta="FREE",th=0,coexp.prior=(10000,200000),Ne.prior=Ne.prior, alpha=T,
       NeA.prior=NeA.prior,time.prior=coexp.prior,gene.prior=gene.prior,append.sims = F, path=getwd())


#Narrow Coexpansion Time model 
sim.coexp(nsims=400000,var.zeta="FREE",th=20000,coexp.prior=(10000,200000),Ne.prior=Ne.prior, alpha=T,
       NeA.prior=NeA.prior,time.prior=time.prior,gene.prior=gene.prior,append.sims = F, path=getwd())


#Partitioned Time model
sim.coexpPT(nsims=400000,var.zeta="FREE",coexp.prior=(10000,200000),Ne.prior=Ne.prior, alpha=F,
          NeA.prior=NeA.prior,time.prior=time.prior,gene.prior=gene.prior,append.sims = F, path=getwd())
  
  