## A function that reads a directory full of files and reports 
## 	the number of completely observed cases in each data file. 
complete<- function(directory, id=1:332){
	options(digits=4)
	csvdata <- data.frame(c("id", "nobs"))    
	filename<-NULL
	nobs <- vector()
	
	## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files  
	## 'id' is an integer vector indicating the monitor ID numbers
  ## to be used    
	for(i in id) {
		if (nchar(i)==1) filename<-paste(directory, "/00", i, ".csv", sep="")		
		if (nchar(i)==2) filename<-paste(directory, "/0", i, ".csv", sep="")		 
		if (nchar(i)==3) filename<-paste(directory, "/", i, ".csv", sep="")
		casenum <- sum(complete.cases(read.csv(filename, header=T)))		 
		nobs <- append(nobs, casenum)		
	}
  
  ## where 'id' is the monitor ID number and 'nobs' is the
  ## number of complete cases
  csvdata<-data.frame(id, nobs)
  return(csvdata)
}
