parallel <- function (ncores){
  
  for(i in 1:ncores){
      dir.create(paste('./',i, sep=""))
      system(paste('cp *',' ./',i, sep=""))
    }
    
  for(i in 1:ncores)
    {
    setwd(paste("./",i,sep=""))
    system('Rscript coexp.R', wait=F)
    setwd("../")
    }
                                     
}  
parallel(20)

