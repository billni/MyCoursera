rankall <- function(outcome, num="best"){

	## Read outcome data
	hdata <- read.csv("outcome-of-care-measures.csv", colClasses = "character", header=T)
	
	disease_name<-c("heart attack","heart failure","pneumonia")	
	if(!any(disease_name==outcome)) stop("invalid outcome")
	
	disease_col <- c(11,17,23)
	names(disease_col) <- disease_name	
		
	disease_data_frame <- hdata[,c(2, 7, disease_col[outcome]	)]
	disease_data_frame$Rate <- as.numeric(disease_data_frame[[3]])
	disease_data_frame$Rank <- rank(disease_data_frame$Rate, ties.method="min")
	browser()
	if (num=="worst") num <- nrow(disease_data_frame)
	if (num=="best") num <- 1
		
	result <- disease_data_frame[disease_data_frame$Rank==num,]
	browser()
	result <- result[,c(1,2)]
	  
}

head(rankall("heart attack", 20), 10)



tail(rankall("pneumonia", "worst"), 3)
tail(rankall("heart failure"), 10)
