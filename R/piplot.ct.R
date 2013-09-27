#	`piplot.ct`
#	Juraj Medzihorsky
#	06 September 2013


piplot.ct <-
	function(fn,
			 data,
			 ... ,
			 from 		= .Machine$double.neg.eps^0.25,
			 to 		= 1-.Machine$double.neg.eps^0.25,
			 n 			= 1e1,
			 draw		= TRUE,
			 color 		= 'black',
			 zero_line 	= TRUE,
			 add 		= FALSE,
			 values 	= FALSE)
{

	thecall <- match.call()
	
	k <- seq(from, to, length.out=n)
	y <- sapply(k, function(x) rcl.em(x, FNEM=fn, data=data, ... , lr_only=TRUE))

	l <- list( ... )
	
	if (is.null(l$lr_eps)) {
		l$lr_eps <- .Machine$double.neg.eps^0.25
	}
	
	
	out <- PiplotCT(call = thecall,
					pi = k,
					lr = y, 
					lr_plus_eps = y + l$lr_eps)

	
	if (draw) {

		plot(out, 
			 add = add,
			 color = color,
			 zero_line = zero_line )		 
	
	}

		
	if (values) {
		
		return(out)
		
	}

}
