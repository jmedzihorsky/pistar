#	`plot` method for class `pistarCUV`
#	Juraj Medzihorsky
#	28 August 2013


setMethod('plot', 
		  'PistarCUV',		
	function(x,
			 model_col = 'blue',
			 unres_col = 'grey',
			 combi_col = 'black',
			 lty = 1,
			 lwd = 1,
			 pos = 'topright',			 
			 bty = 'n',
			 ... )
	{
		o <- x
	
		nms <- c('combined', 'model', 'unrestricted')
		
		plot(o@pred$combi, 
			 type	= 'l', 
			 frame	= FALSE, 
			 col	= combi_col, 
			 ylab	= 'Density', 
			 xlab	= 'Value', 
			 lty	= lty, 
			 lwd	= lwd, 
			 ... )
		lines(o@pred$model, col=model_col, lty=lty, lwd=lwd)
		lines(o@pred$unres, col=unres_col, lty=lty, lwd=lwd)
		legend(pos, 
			   bty		= bty,
			   lty		= rep(lty, 3), 
			   lwd		= rep(lwd, 3),
			   col		= c(combi_col, model_col, unres_col),
			   legend	= nms)

	}
)
