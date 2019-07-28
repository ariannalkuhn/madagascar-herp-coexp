library(pegas)
library(PipeMaster)
library(ape)
library(e1071)
library(PopGenome)
library(phyclust)
library(bigmemory)
library(abc)
library(nnet)

#goodness of fit test plots hist of sim model with obs
#5:20 are my sum stats of interest, 1:4 are the 4 hyper params (zeta etc) in the simulation file

x<-gfit(observed,simulated[,5:20],nb.replicate = 200,tol=0.02)
plot(x)
plot.sim.obs(simulated[,5:20],observed)



#cut down your # of simulations to ~50k or this wont ever finish
simulated2<-simulated[1:100000,]
simulated3<-simulated2[,5:20]
x<-gfit(observed,simulated3,nb.replicate = 200,tol=0.02)
plot(x)
plot.sim.obs(simulated3,observed)


#goodness of fit for each sum stat (just click through with arrows to see each one)
model1_gfit=gfit(target=observed, sumstat=simulated3, statistic=mean, nb.replicate=100)
pdf(file="model1_histogram_Gfit.pdf")
plot(model1_gfit,main="Histogram of model1")
dev.off()
model1_gfit_sum<-summary(nobuff_gfit)
write.table(model1_gfit_sum, file="model1gfit.txt",quote=T,row.names=F)
plot.sim.obs(simulated3, observed)

#PCA goodness of fit
#need to concat the sim files from model1 & model 2, convert to matrix
#then make a models object that identifies rows w:x as model1 and rows y:z as model 2 
pcamodelfit<-gfitpca(target=observed, sumstat=simulated3, index=models, cprob=.1)
pdf(file="pcaGfit.pdf")
plot(pcamodelfit, main="PCA Gfit model1 vs model2")
dev.off
pcagfitsum<-summary(pcamodelfit)
write.table(pcagfitsum, "file=pcagfitsum.txt", quote-T, row.names=F)

