#	class `SummaryPistar`
#	Juraj Medzihorsky
#	06 September 2013


#	-------------
#	SummaryPistar
#	-------------


#	definition

setClass('SummaryPistar',
		 representation(
						oldcall	= 'language',
						pred	= 'list',
						est		= 'data.frame'
						)
		 )


#	constructor

SummaryPistar <- 
	function(oldcall, 
			 pred,
			 est)
	{
		new('SummaryPistar',
			oldcall	= oldcall,
			pred	= pred,
			est		= est)
	}



#	method `print` for class `SummaryPistar`

setMethod('print',
		  'SummaryPistar',
	function(x, 
			 digits = 2,
			 ...)
{
	obj <- x
	
    cat('\n\nCall:\n', 
		paste(deparse(obj@oldcall), sep='\n', collapse='\n'), 
		'\n\n', sep='')
	
	S <- obj@est
	j <- ifelse(ncol(S)==1, 1, 4)	
	A <- round(S[1, 1:j], digits)		
	
	if (j>1) {
		
		conf <- S[1, 5]*1e2	
		side <- S[1, 6]
		
		if (side=='lower'){
	
			ci_msg <- paste('lower bound of ', conf,
							'% one-sided confidence interval = ',
							A[3], '\n', sep='')

		} else if (side=='upper') {
	
			ci_msg <- paste('upper bound of ', conf,
							'% one-sided confidence interval = ',
							A[4], '\n', sep='')
		
		} else if (side=='both') {

			ci_msg <- paste('two sided ', conf,
							'% confidence interval = ', 
							A[3], ', ', A[4], '\n', sep='')
		
		}
		

		aux.side <- function(x) {
			x <- as.character(x)
			x[x=='lower'] <- '|---'	
			x[x=='upper'] <- '---|'	
			x[x=='both' ] <- '|--|'	
			return(x)
		}
		
		S[, 6] <- aux.side(S[, 6])
		
	}

	
	P <- S
	P[, 1:j] <- format(round(S[, 1:j], digits), nsmall=2)
	

	cat('\nReported quantities:\n')
	ifelse(j==4, print(P[, -c(5, 7)]), print(P))	#	!!!!!
		
	if (ncol(S)==7) {	
	
		cat('---\nConfidence intervals:',
			'\n    |--| two-sided',
			'\n    ---| one-sided upper', 
			'\n    |--- one-sided lower\n',	
			sep='')
		
	}
	
	
	cat('\n\n')
}
	)
