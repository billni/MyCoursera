## A function named 'pollutantmean' that calculates the mean of 
## a pollutant (sulfate or nitrate) across a specified list of monitors. 

pollutantmean <- function(directory, pollutant, id=1:332){  
	options(digits=4)
	subdata <- data.frame()    
	filename<-NULL
	
	## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files  
	## 'id' is an integer vector indicating the monitor ID numbers
  ## to be used    
	for(i in id) {
		if (nchar(i)==1) filename<-paste(directory, "/00", i, ".csv", sep="")
		if (nchar(i)==2) filename<-paste(directory, "/0", i, ".csv", sep="")		 
		if (nchar(i)==3) filename<-paste(directory, "/", i, ".csv", sep="")		 
		subdata <- rbind(subdata, read.csv(filename, header=T))
	}
	   
  ## 'pollutant' is a character vector of length 1 indicating
  ## the name of the pollutant for which we will calculate the
  ## mean; either "sulfate" or "nitrate".
  
  switch(pollutant,
   sulfate = result<-mean(subdata$sulfate, na.rm=T),
   nitrate = result<-mean(subdata$nitrate, na.rm=T)
  )
  
  ## Return the mean of the pollutant across all monitors list
  ## in the 'id' vector (ignoring NA values)
  return(result)
}
