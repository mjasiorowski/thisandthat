pollutantmean <- function(directory, pollutant, id = 1:332) {
  
  tmp <- c()
  directory <- "C:/Users/AP_470.AP470/Documents/thisandthat/specdata"
  
  for( i in id ) {
    
    a <- read.csv(sprintf("%s/%03d.csv",directory,i))
    tmp <- c(tmp,a[[pollutant]])
    
  }
  
  mean(tmp,na.rm=T)
  
}



complete <- function(directory, id = 1:332) {
  
  if(grep("specdata", directory) == 1) {
    directory <- ("C:/Users/AP_470.AP470/Documents/thisandthat/specdata")
  }
  
  id_len <- length(id)
  complete_data <- rep(0, id_len)
  all_files <- as.character( list.files(directory) )
  file_paths <- paste(all_files, sep="")
  j <- 1 
  
  for (i in id) {
    
    current_file <- read.csv(file_paths[i], header=T, sep=",")
    complete_data[j] <- sum(complete.cases(current_file))
    j <- j + 1
    
  }
  
  result <- data.frame(id = id, nobs = complete_data)
  return(result)
  
} 



corr <- function(directory, threshold = 0) {
  
  directory <- "C:/Users/AP_470.AP470/Documents/thisandthat/specdata" 
  complete_table <- complete("specdata", 1:332)
  nobs <- complete_table$nobs
  ids <- complete_table$id[nobs > threshold]
  id_len <- length(ids)
  corr_vector <- rep(0, id_len)
  all_files <- as.character( list.files(directory) )
  file_paths <- paste(directory, all_files, sep="")
  j <- 1
  
  for(i in ids) {
    
    current_file <- read.csv(sprintf("%s/%03d.csv",directory,i))
    corr_vector[j] <- cor(current_file$sulfate, current_file$nitrate, use="complete.obs")
    j <- j + 1
    
  }
  
  result <- corr_vector
  return(result)  
  
}