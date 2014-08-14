best <- function(state, outcome) {
	## Read outcome data
	hdata <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
	## Check that state and outcome are valid
	if (!any(hdata$State==state)) 	stop("invaild state")		
	disease<-c("heart attack","heart failure","pneumonia")	
	if(!any(disease==outcome)) stop("invaild outcome")
	
	num<-c(11,17,23)
	names(num)<-disease
	
	statelist <- which(hdata$State==state) 
	ill <- as.numeric(hdata[,num[outcome]][statelist])
  names(ill) <- hdata$Hospital.Name[statelist]
  ill=sort(ill)
  names(ill[1])	
}
