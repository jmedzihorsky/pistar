#	Pooling of jackknifed estimates
#	Juraj Medzihorsky
#	06 September 2013


pool.jack <-
	function(data, 
			 ct 	= TRUE, 
			 estimate, 
			 jack_est,
			 side 	= NULL,
			 conf 	= 0.95,
			 lower 	= -Inf,
			 upper 	= Inf,
			 bias 	= FALSE)
{
	j <- jack_est			
	e <- estimate
	
	n <- ifelse(ct, sum(data), length(data))	
	k <- ifelse(ct, data, 1)
	m <- ifelse(ct, sum(j*k)/n, mean(j))			
		
	if (bias) {
		se <- sqrt( (n-1)/n * sum( k * (j-m)^2 ) )
		bias <- (n-1)*(m-e)							# unfinished
	} else {
		se <- sqrt( (n-1)/n * sum( k * (j-e)^2 ) )			
	}
		
	side <- tolower(side)
			
	if (length(grep('au', side))==1) {
		side <- ifelse(e > 0, 'lower', 'upper')
	}
		
	
	if (length(grep('lo', side))==1) {
		
		side <- 'lower'
		low <- max(e - qnorm(conf)*se, lower)
	   	upp <- upper
		
	} else if (length(grep('up', side))==1) {
		
		side <- 'upper'
		low <- lower 
		upp <- min(e + qnorm(conf)*se, upper)
		
	} else if (length(side)==0) {							#	!!!!!!!!! 
		
		side <- 'both'
		low <- max(e - qnorm(1-(1-conf)/2)*se, lower)
		upp <- min(e + qnorm(1-(1-conf)/2)*se, upper)
		
	} else {
		
		stop('Unsupported confidence interval type.\n')
		
	}
		
	
	out <- list(se = se, 
				low = low,
				upp = upp,				
				conf = conf,
				side = side,
				bias = bias)
	
	return(out)
}



