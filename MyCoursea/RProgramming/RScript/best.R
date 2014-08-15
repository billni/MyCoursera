best <- function(state, outcome) {
	## Read outcome data
	hdata <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
	
	## Check that state and outcome are valid
	if (!any(hdata$State==state)) 	stop("invalid state")		
	
	disease_name<-c("heart attack","heart failure","pneumonia")	
	if(!any(disease_name==outcome)) stop("invalid outcome")
	
	disease_col<-c(11,17,23)
	names(disease_col)<-disease_name
	
	statelist <- which(hdata$State==state) 
	disease_data <- hdata[statelist,disease_col[outcome]]
	names(disease_data) <- hdata$Hospital.Name[statelist]
	
	disease_data <- disease_data[disease_data != "Not Available"]
	disease_name <- names(disease_data)
	
	disease_data <- as.numeric(disease_data)
  names(disease_data) <- disease_name
  
	#names(disease_data) <- hdata$Hospital.Name[statelist]	
	disease_data <- sort(disease_data)
  names(disease_data[1])	
}

