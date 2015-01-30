#--
# title: read.ts
# author: ML Larsen
# last updated: 2015 Jan 29
#
# purpose: source code is designed for reading in a time series data file
#   
#--

read.ts <- function(input = " ", skip = ""){
  data.in <- read.csv(input, skip=skip, header=T, as.is=T)
  results.start <- which(data.in == "Results")
  data.out <- data.in[1:(results.start - 1),]
  colnames(data.out)[2] <- "Temp"
  t.h <- as.numeric(lapply(strsplit(data.out$Time, "\\:"), "[", 1))
  t.m <- as.numeric(lapply(strsplit(data.out$Time, "\\:"), "[", 2))
  data.out$Time <- round(t.h + t.m/60, 2)
  for (i in 1:dim(data.out)[2]){
    if (is.numeric(data.out[,i]) == FALSE){data.out[,i] = as.numeric(data.out[,i])}}
  return(data.out)
}