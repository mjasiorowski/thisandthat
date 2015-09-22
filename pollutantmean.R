pollutantmean <- function(directory, pollutant, id = 1:332) {
  tmp <- c()
  directory <- "C:/Users/AP_470.AP470/Documents/thisandthat/specdata"
  for( i in id ) {
    #print(sprintf("%s/specdata%03d.csv",directory,i))
    a <- read.csv(sprintf("%s/%03d.csv",directory,i))
    tmp <- c(tmp,a[[pollutant]])
  }
  
  mean(tmp,na.rm=T)
}
