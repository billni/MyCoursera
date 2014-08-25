## read csv file
ssqread <- function(file) {
	ssq <- read.csv(file, header=T, stringsAsFactors=F)
	ssq$B.cumsum <- cumsum(ssq$B)
	x <- ssq$B.cumsum / ssq$B	
  y <- x-floor(x)
  ssq$B.y <- y
  ssq
}

##compute every number ratio
ssqcompute <- function(o = data.frame(), lastrownum=nrow(o)) {
	  options(ditigs=5)	  
    o[,3] <- cumsum(o[,2])
    x <- o[,3]/o[,2]
    y <- x-floor(x)   
    o[,4] <- y    
    ret <- o[lastrownum,]        
    lastrowdata <- o[lastrownum,]
    
    for (i in 1:16) {    	
      newperiod <- lastrowdata[1,1]+1
      x <- lastrowdata[1,3] + i
      x <- x / i
      y <- x-floor(x)      
    	ret <- rbind(ret, list(newperiod, i, lastrowdata[1,3] + i, y))        	    	    	    	
    }    
    ret[order(ret[,4]),]
}


## target is a number to be tested.
## pre is a number to look up previous period
ssqtable <- function(o = data.frame(), target=1, pre=1, prob=0.8) {
	pos <- which(o[,2]==target)	
	v <- NULL	
	ret <- vector()	
	for(i in 1:pre){   
			pos <- pos[pos-i>0]
			v <- o[pos-i, 2]
			x <- table(v)
		  y <- which(x==quantile(x, prob))
		  ret <- c(ret, y)
	}
	unique(names(ret))
}


