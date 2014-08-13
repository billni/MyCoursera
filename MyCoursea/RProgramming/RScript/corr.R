## A function that takes a directory of data files and a threshold for complete cases 
## and calculates the correlation between sulfate and nitrate for monitor locations 
## where the number of completely observed cases (on all variables) is greater than the threshold. 
corr <- function(directory,threshold=0){
  filenames <- list.files(directory, full.names=TRUE)
  n <-length(filenames)
  cr <- numeric()
  
  #### 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  for (i in 1:n) {
    dat <- data.frame(lapply(filenames[i], read.csv))   
    datcomplete <- subset(dat, complete.cases(dat))
    check <- length(datcomplete$ID) 
    
    ## 'threshold' is a numeric vector of length 1 indicating the
    ## number of completely observed observations (on all
    ## variables) required to compute the correlation between
    ## nitrate and sulfate; the default is 0
    if (check >= threshold & check>0) {       
      cal <- cor(datcomplete$sulfate,datcomplete$nitrate)
      cr <- c(cr, cal)
    }   
  }
  ## Return a numeric vector of correlations
  return(cr)
}