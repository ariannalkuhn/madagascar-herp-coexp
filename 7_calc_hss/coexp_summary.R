library(e1071)
library(abc)
library(PipeMaster)
library(PopGenome)
library(ape)
library(bigmemory)
library(pegas)
library(phyclust)
library(nnet)

observed<-observed.coexp.sumstat(path.to.fasta="path_to_observed") 

setwd("path_to_priors")

simulated<-read.big.matrix(file="simulations.txt", header=T, type="float", sep="\t")

# abc rejection
abc<-abc(target=observed, param=simulated[,1:4], sumstat=simulated[,5:20], tol=0.00004, transf=c("none"), method="rejection", hcorr=F, sizenet=10, numnet=20, maxit=2000, trace=F)
rej<-summary(abc)
write.table(rej, "abc_summary.txt")

#abc rejection with neural net
abc.Nnet<-abc(target=observed, param=simulated[,1:4], sumstat=simulated[,5:20], tol=0.00004, transf=c("none"), method="neuralnet", hcorr=F, sizenet=10, numnet=20, maxit=2000, trace=F)
x<-summary(abc.Nnet)
pdf(file="NNET.pdf")
plot(abc.Nnet, param=simulated[,1:4])