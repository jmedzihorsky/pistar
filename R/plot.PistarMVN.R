#	`plot` method for class `PistarMVN`
#	Juraj Medzihorsky
#	28 August 2013


setMethod('plot', 
		  'PistarMVN',		
	function(x,
			 lty = 1,
			 lwd = 1,
			 col = 'black',
			 ... )
	{
		o <- x
		
		plot(x		= 1:o@iter$est, 
			 y		= o@trace$est$pi, 
			 type	= 'l', 
			 lty	= lty, 
			 lwd	= lwd, 
			 col	= col,
			 main	= paste('Stopped at iteration no.', o@iter$est),
#			 las 	= 1, 
			 xlab	= 'Iteration', 
			 ylab	= expression(pi))
	}	
)
