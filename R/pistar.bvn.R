#	`pistar.bvn` with S4 class output
#	Juraj Medzihorsky
#	06 September 2013


pistar.bvn <-
	function(x			, 
			 y			,
			 conf 		= 0.95,
			 na.action	,
			 alt 		= 'two.sided',
			 verbose 	= TRUE)
{
	thecall <- match.call()
	
	res <- cor.test(x, y, 
					alternative = alt,
					conf.level = conf,	
					na.action = na.action, 
					method = 'pearson')
	
	cin <- as.vector(res$conf.int)

	if( (abs(cin[1])/cin[1]) == (abs(cin[2])/cin[2]) ){
		int <- sort(pearson2pistar(cin)) 	
	} else {
		up <- abs(cin)[abs(cin)==max(abs(cin))] 
		int <- c(0, pearson2pistar(up))
	}
	
	pistar_list <- list(est = pearson2pistar(res$estimate))
	
	if (verbose) {
		cat(sprintf('\npi* = %.2f \n\n', pistar_list$est))
		cat(conf*1e2, '% confidence interval: ', 
			round(int[1], 2),', ', round(int[2], 2),'\n\n' , sep='')		
	}
 

	pred_list  <- list(model = NA, unres = NA, combi = NA)
	param_list <- list(est = numeric(0))		#	necessary for consistency
	
	out <- PistarBVN(call 		= thecall,
					 pistar 	= pistar_list,
					 pred 		= pred_list,
					 data 		= data.frame(x, y),
					 param		= param_list,
					 interval 	= int,
					 conf		= conf,
					 alt		= alt)
	
	return(out)
}
