
rankhospital <- function(state, outcome, num="best"){

## Read outcome data
	hdata <- read.csv("outcome-of-care-measures.csv", colClasses = "character", header=T)
	
	## Check that state and outcome are valid
	if (!any(hdata$State==state)) 	stop("invalid state")		
	
	disease_name<-c("heart attack","heart failure","pneumonia")	
	if(!any(disease_name==outcome)) stop("invalid outcome")
	
	disease_col <- c(11,17,23)
	names(disease_col) <- disease_name
	
	statelist <- which(hdata$State==state) 
		
	disease_data <- hdata[statelist,disease_col[outcome]]
	names(disease_data) <- hdata$Hospital.Name[statelist]
	
	disease_data <- disease_data[disease_data != "Not Available"]
	disease_name <- names(disease_data)
	disease_data <- as.numeric(disease_data)
	
	disease_data_frame <- data.frame(disease_name, disease_data, stringsAsFactors = F)	   
  disease_data_frame <- disease_data_frame[order(disease_data_frame$disease_name, disease_data_frame$disease_data),]    
	
	if (num=="worst") num <- nrow(disease_data_frame)
	if (num=="best") num <- 1
	
  result <- disease_data_frame[rank(disease_data_frame$disease_data, ties.method = "first")==num,]  
  
  if(nrow(result)==0) {
  	NA
  } else{
  	result$disease_name
  }
}
