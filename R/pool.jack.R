#	Pooling of jackknifed estimates
#	Juraj Medzihorsky
#	2014-12-09


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
	p <- 1 - (1-conf)/2
	e <- estimate
	j <- jack_est			
	
	n <- ifelse(ct, sum(data), length(data))	#	No. cases
	#	Weight
	if (ct) {
		w <- as.vector(data)	
		if ( (length(w)-length(j)) == sum(w==0) ) {
			w <- w[w!=0]						#	assumes no js for empty cells
		}
	} else {
		w <- rep(1, n)
	}
	#
	m <- ifelse(ct, sum(j*w)/n, mean(j))		#	Weighted mean jack value


	if (bias) {
		se <- sqrt( (n-1)/n * sum( w * (j-m)^2 ) )
		bias <- (n-1)*(m-e)						# 	bias
		theta_bc <- n*e - (n-1)*m				#	bias corrected stat
		theta <- theta_bc
	} else {
		se <- sqrt( (n-1)/n * sum( w * (j-e)^2 ) )
		bias <- as.numeric(NA)
		theta <- e		
	}
		
	side <- tolower(side)
			
	if (length(grep('au', side))==1) {
		side <- ifelse(theta > 0, 'lower', 'upper')
	}
		
	
	if (length(grep('lo', side))==1) {
		
		side <- 'lower'
		low <- max(theta - qnorm(p)*se, lower)
	   	upp <- upper
		
	} else if (length(grep('up', side))==1) {
		
		side <- 'upper'
		low <- lower 
		upp <- min(theta + qnorm(p)*se, upper)
		
	} else if (length(side)==0) {							#	!!!!!!!!! 
		
		side <- 'both'
		low <- max(theta - qnorm(1-(1-conf)/2)*se, lower)
		upp <- min(theta + qnorm(1-(1-conf)/2)*se, upper)
		
	} else {
		
		stop('Unsupported confidence interval type.\n')
		
	}
		
	
	out <- list(se = se, 
				low = low,
				upp = upp,				
				theta = theta,
				conf = conf,
				side = side,
				bias = bias)
	
	return(out)
}



