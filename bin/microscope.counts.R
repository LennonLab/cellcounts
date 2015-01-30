#--
# title: microscope.counts
# author: ML Larsen
# last updated: 2015 Jan 29
#
# purpose: source code is designed for converting microscope counts into cellular 
#  densities for time series analyis
#   
#--


microscope.counts <- function(input = " ", output = " ", data.col = " "){

  # Input data
  data.in <- as.matrix(read.csv(input, header = FALSE))

  # Initialize data storage
  results <- matrix(NA,nrow=c(length(unique(data.in[,4]))+length(unique(data.in[,5]))),ncol=ncol(data.in))
  #colnames(results) <- c()
  #results <- as.data.frame(results)

  # Create Output
  #outfile.trt <- paste("../output/",output,".txt", sep="")
  #titles.trt <- c("time","trt","org","abundance", "sem")
  #write.table(as.matrix(t(titles.trt)), file=outfile.trt, append=T, row.names=F, col.names=F, sep=",", quote=FALSE)

  outfile.id <- paste("./output/",output,".txt", sep="")
  titles.id <- c("time","trt","identifier","org","abundance", "sem")
  write.table(as.matrix(t(titles.id)), file=outfile.id, append=T, row.names=F, col.names=F, sep=",", quote=FALSE)


  # pull out relevant information
  scope <- data.in[1,1]
  time <- as.numeric(data.in[3, data.col:ncol(data.in)])

  data.in <- data.in[4:nrow(data.in),]
  identifier <- unique(data.in[,4])
  treatments <- unique(data.in[,2])

  # field dimensions by microscope

  if(scope == "Nikon"){
      x10 <- 581.80
      x20 <- NA
      x40 <- 9889.61
      x100 <- 7451.08

  }else if (scope == "Zeiss"){
      x10 <- 350.32
      x20 <- 1614.74
      x40 <- 6122.52
      x100 <- 34939.95
      
  }else{
      x10 <- NA
      x20 <- 4897.85
      x40 <- 18783.42
      x100 <- 118540.63
  }

  # counts per mL

  for(i in 1:length(identifier)){
   cur <- data.in[data.in[,4] == identifier[i],]
 
     ids <- cur[1,2:6]
     cur <- cur[,data.col:ncol(cur)]
     fields <- as.numeric(cur[12,])
     fields[fields == 10] <- x10
     fields[fields == 20] <- x20
     fields[fields == 40] <- x40
     fields[fields == 100] <- x100
     cpm <- colMeans(matrix(as.numeric(cur[1:10,]),10,ncol(cur)),na.rm=TRUE)/(as.numeric(cur[11,])*(1-as.numeric(cur[13,])))*1000*fields
     results <- rbind(results,c(ids,cpm))

  }

  results1 <- results
  write.csv(results,"./output/results.csv")

}

