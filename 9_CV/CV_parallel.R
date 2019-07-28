library(devtools)
library(PipeMaster)
library(bigmemory)
library(abc)
library(doMC)

library(doMC)
registerDoMC(cores=10)
setwd("/simulations")
simulated <- read.big.matrix(file="simulations.txt", header=T, type="float", sep="\t")
reps <- 400
d <- list.dirs(recursive = F)

tab<-NULL
for(j in 1:length(d)){
  setwd(d[j])
  if(file.exists("simulations.txt")==F){
    setwd("/media/data2/Arianna/newpriors/sims/reptilesnct_extra")
   next
  }
  list.files()
  
splitIndex <- sample(1:nrow(simulated), reps)
  train <- simulated[-splitIndex,]
  test  <- simulated[splitIndex,]
  
  probs <- foreach::foreach  (n = 1:nrow(test)) %dopar% {
    if(sum(is.na(test[n,]))>0) next
    x <- abc(target = test[n,5:20], sumstat =  train[,5:20], param = train[,1:4], method = "rejection", tol = 100/nrow(train))
    y <- summary(x)
    y <- y[5,]
    y
  }
  
  
  test<-test[-which(sapply(probs, is.null)),]
  test[which(sapply(probs, is.null)),]
  
  probs2<-data.frame(matrix(unlist(probs), nrow=nrow(test), byrow=T))
  
  tab2 <- rbind(tab, getwd())

  rownames(tab)[nrow(tab)]<-d[j]
  setwd("/media/data2/Arianna/newpriors/sims/extrasims")
  print(tab)
}
  probs<-apply(probs,2,as.numeric)

rsq <- function (x, y) {
  res<-NULL 
  for(i in 1:ncol(x)) res<-c(res, cor(x[,i], y[,i]) ^ 2)
  names(res) <- colnames(x)
  return(res)
}


rsq(probs2, test[,1:4])


plots<-function(x,y){
  par(mfrow=c(2,2))
  for(i in 1:ncol(x)){
    plot(x[,i]~y[,i], xlab="true", ylab="estimated", main=colnames(x)[i])
    abline(lm(x[,i]~y[,i]), col=2)
    points(c(min(x[,i]), max(x[,i])), c(min(x[,i]), max(x[,i])), type="l", col=1) 
    mtext("Cross Validation Reptiles rejection NCT Mode Model")
  }
}
par(cex.lab=1.5, cex.axis=1.2, cex.main=1.5, cex.sub=1.5)
plots(probs2, test[,1:4])
