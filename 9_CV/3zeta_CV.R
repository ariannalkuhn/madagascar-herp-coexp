library(PipeMaster)
library(bigmemory)
library(abc)


observed<-observed.coexp.sumstat("/observed")
setwd("/simulations")
simulated<-read.big.matrix(file="simulations.txt", header=T, type="float", sep="\t")

x<-unique(simulated[,1])
x<-sort(x)


min.zeta <- simulated[which(simulated[,1] == min(x)),]
max.zeta <- simulated[which(simulated[,1] == max(x)),]
#chose x[] value that represents median zeta, e.g., 0.5 
x
middle.zeta <- simulated[which(simulated[,1] == x[6]),]

models<-rbind(min.zeta,middle.zeta,max.zeta)
index<-c(rep("min",nrow(min.zeta)),
         rep("middle",nrow(middle.zeta)),
         rep("max",nrow(max.zeta)))

prob <- postpr(target=observed,index=index, sumstat = models[,5:20],tol=100/nrow(models), method = "rejection")
#prob <- postpr(target=observed,index=index, sumstat = models[,5:20],tol=1000/nrow(models), method = "neuralnet")
summary(prob)

cv.prob <- cv4postpr(index=index,sumstat = models[,5:20],tol=100/nrow(models),method = "rejection", trace=T, nval=200)
#cv.prob <- cv4postpr(index=index,sumstat = models[,5:20],tol=1000/nrow(models),method = "neuralnet", trace=T, nval=200)
summary(cv.prob)


sum(diag(cv.prob$tol0.0001))/300

#par(mfrow=c(2,2))
pdf(file="Confusion_Matrix.pdf")
plot(cv.prob,xlab="NCT Model")
plot(cv.prob, main="NCT Model")
dev.off()