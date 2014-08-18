rankall <- function(outcome, num="best"){
	hdata <- read.csv("outcome-of-care-measures.csv", colClasses = "character", header=T)
			
	disease_name_list <- c("heart attack","heart failure","pneumonia")	
	
	if(!any(disease_name_list == outcome)) stop("invalid outcome")
  
  ## appear state list uniquely that was sorted in alphabetical order
  statelist <- sort(unique(hdata$State))
  ## initial result
  result <- data.frame(NA, statelist, NA, NA)
  names(result) <- c("hospital", "state", "ill", "rate")
      
  ## appear ill vector with respective name    
	mortality <- c(11,17,23)
	names(mortality) <- disease_name_list
	      
	## appear data frame with 3 columns , hospital_name , state name and mortality rate
	amd <- hdata[, c(2, 7, mortality[outcome])]	   
		
  for (state in statelist) {
    ## mortality data frame in each state     
    esd <- amd[amd$State == state, ]        
  	esd <- data.frame(esd, "Rate"=as.numeric(esd[,3]), stringsAsFactors=FALSE)   	
		if (num=="worst") num <- nrow(esd)
		if (num=="best") num <- 1	  	 
  	
  	##order 4th and 1th column
  	esd <- esd[order(esd[,4], esd[,1]),]  	
  	
  	## assemble result data frame
  	if (num <= nrow(esd)) {      	
  	   result[result$state==state,] <- esd[num,]
  	}
  }    
  return(result)
}

head(rankall("heart attack", 20), 10)
tail(rankall("pneumonia", "worst"), 3)
tail(rankall("heart failure"), 10)
