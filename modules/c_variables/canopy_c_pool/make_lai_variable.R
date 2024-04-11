make_lai_variable <- function(){

  #res <- download_lai_variable()
  
  #res <- subset(res, select=c(Date, Ring, LAI))
  #names(res)[3] <- "lai_variable"
  
  #- return a number for ring
  #res$Ring <- as.numeric(res$Ring)
  
  res <- read.csv("data/raw/lai_data.csv", header=T)
  
  write.csv(res, "data/processed/lai_data.csv", row.names=F)
  
  return(res)
}


