#	class `PiplotCT` 
#		with definition, constructor, `print`, `show` and `plot`
#	Juraj Medzihorsky
#	06 September 2013


#	----------
#	definition
#	----------

setClass('PiplotCT',
		 representation(
						call = 'language',
						pi = 'numeric',
						lr = 'numeric',
						lr_plus_eps = 'numeric'
						)
		 )



#	constructor

PiplotCT <-
	function(call,
			 pi, 
			 lr, 
			 lr_plus_eps)
	{
	new('PiplotCT',
		call = call,	
		pi = pi,
		lr = lr,
		lr_plus_eps = lr_plus_eps)
	}


#	--------------
#	method `print`
#	--------------

setMethod('print',
		  'PiplotCT', 
		  function(x, 
				   ...)
		  {
			  obj <- x
			  cat('\n\nCall:\n', 
				  paste(deparse(obj@call), sep='\n', collapse='\n'), 
				  '\n\n', sep='')			  
		  } 
		  )


#	-------------
#	method `show`
#	-------------

setMethod('show',
		  'PiplotCT',
		  function(object) print(object))



#	-------------
#	method `plot`
#	-------------

setMethod('plot', 
		  'PiplotCT',
	function(x,
			 y,
			 add = FALSE,
			 color = 'black',
			 zero_line = TRUE,
			 ...)
{
		o <- x
	
		if (!add) {
			plot(x		= o@pi, 
				 y		= o@lr_plus_eps, 
				 type	= 'n', 
				 frame	= FALSE, 
				 xlim	= range(o@pi),
				 xlab	= expression(symbol(pi)), 
				 ylab	= 'Log-likelihood ratio statistic')
		}
	
		lines(o@pi, o@lr_plus_eps, col=color)

		if (zero_line) {
			lines(x=range(o@pi), y=c(0, 0), lty=3, col='grey')
		}
	
	
}		)
