
parallel <- function (ncores){
  
  library(bigmemory)
  
    table<-NULL
    for(i in 1:ncores)
      {
      setwd(paste("./",i,sep=""))
      x<-read.big.matrix(file="simulations.txt",header=T,type="float",sep="\t")
      table<-rbind(table,x[,])
      setwd("../")
      print(i)
      }
    write.table(table,"simulations.txt",quote=F,col.names = T, row.names = F, sep="\t")                                 
    }  

parallel(20)


