#	`plot` method for class `PistarDUV`
#	Juraj Medzihorsky
#	07 September 2013


setMethod('plot', 
		  'PistarDUV',		
	function(x,
			 model_col = 'blue',
			 unres_col = 'grey',
			 combi_col = 'black',
			 pos = 'topright',			 
			 bty = 'n',
			 ... )
	{
		o <- x
	
		nms <- c('combined', 'model', 'unrestricted')
		
		s <- sum(o@pred$combi)
		
		barplot(height	= o@pred$combi/s, 
				col		= unres_col, 
				ylab	= 'Density', 
				xlab	= 'Value', 
				... )

		barplot(height	= o@pred$model/s, 
				col		= model_col, 
				add		= TRUE)

		legend(pos, 
			   bty		= bty,
			   pch		= rep(15, 2), 
			   col		= c(model_col, unres_col),
			   legend	= nms[2:3])
	}
)
