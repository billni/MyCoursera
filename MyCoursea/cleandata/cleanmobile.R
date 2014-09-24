cleanmobile <- function() {
	library(RODBC)
	## Read data from db
	myconn <- odbcConnect("tstest", uid="tsdev", pwd="uat123")
	dbmobile <- sqlQuery(myconn, "select mobilephone from CCP_CONTACT")
  names(dbmobile) <- "mobile"
  close(myconn)
	print(paste("Mobile rows: ", nrow(dbmobile), ", Take over(MB): ",format(object.size(dbmobile), "Mb")))
	
	## Read data that need cleaned from csv
	unclean<-read.table("csv/unclean.txt", header=F,col.names="mobile")
	
	## verify data if match with dbmobile
	cleaned <- data.frame(unclean[which(!(unclean$mobile %in% dbmobile$mobile)),])
  names(cleaned) <- "mobile"
  print(paste("Mobile rows: ", nrow(cleaned), ", Take over(MB): ",format(object.size(cleaned), "Mb")))
  
  ## write cleaned data into csv
  write.csv(cleaned, "csv/cleaned.csv")
}

