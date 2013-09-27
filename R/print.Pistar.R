#	method `print` for S4 class `Pistar`
#	Juraj Medzihorsky
#	27 September 2013


setMethod('print',
		  'Pistar',
	function(x, 
			 digits = 2,
			 ...)
	{	
		obj <- x
	    cat('\n\nCall:\n', paste(deparse(obj@call), sep='\n', collapse='\n'), 
			'\n\n', sep='')

		cat('\n', rep('-', 24+digits), '\n', sep='')	
		cat('\n\tpi* = ', round(obj@pistar$est, digits), '\n', sep='')
		cat('\n', rep('-', 24+digits), '\n\n\n', sep='')	

		n_par <- length(obj@param$est)
		
		if (n_par!=0) {

			if (is.null(names(obj@param$est))) {
				names(obj@param$est) <- paste('Par[', 1:n_par, ']', sep='')	
			}
		
			cat('Other reported quantities:\n')
			print(round(obj@param$est, digits))
			cat('\n\n')
		
		}

		if (!is.null(obj@pistar$jack)) {
   			cat('Jackknife estimates available for pi*')
			if (!is.null(obj@param$jack)) {
				cat(' and other quantities')
			}
			cat(" using method 'summary'\n\n\n")	
		}
	
	}

)
