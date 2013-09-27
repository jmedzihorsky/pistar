#	`pistar.2by2` for S4 class
#	Juraj Medzihorsky
#	27 August 2013


pistar.2by2 <-
	function(data, 
			 alpha 		= 1,
			 jack 		= FALSE,
			 verbose 	= TRUE)
{	
	thecall <- match.call()
	
	O <- data
	
	d_o <- dim(O)
	
	if (! ((length(O)==4) & (d_o[1]==2) & (d_o[2]==2)) ){
		stop('Data must be 2-by-2 table.')
	}
	
	if (!is.numeric(alpha)){
		stop('alpha must be numeric')
	}
	
	
	M <- O
	N <- sum(O)
	ahat <- (O[1,1]*O[2,2])/(O[1,2]*O[2,1]) 
	
	if (ahat>alpha) {
		
		g <- min(c(O[1,1], O[2,2]))
		d <- g * (1 - alpha/ahat)
		ifelse(O[1,1]==g, M[1,1]<-M[1,1]-d, M[2,2]<-M[2,2]-d)	
		
	} else if (ahat<alpha) {
		
		h <- min(c(O[1,2], O[2,1]))
		d <- h * (1 - ahat/alpha)
		ifelse(O[1,2]==h, M[1,2]<-M[1,2]-d, M[2,1]<-M[2,1]-d)

	} else if (ahat==alpha) {
		
		d <- 0
		
	} else {
		
		stop('Invalid alpha.')
		
	}


	pistar_list <- list(est = d/N)
	
	if (verbose) {
		cat(paste('\npi* =', round(pistar_list$est, 3), '\n\n'))	
	}

	
	if (jack) {
		
		aux.1 <- function(i){				#	rename ?
			data[i] <- data[i] - 1 
			return(data)
		}

		aux.2 <- function(x){
			pistar.2by2(data 	= x, 
						alpha 	= alpha, 
						verbose = FALSE, 
						jack 	= FALSE)
		}

		B <- lapply(1:prod(dim(O)), aux.1)	#	length(O) instead ?

		jack_all <- lapply(B, aux.2)

		pistar_list$jack <- sapply(jack_all, function(x) x@pistar$est)
			
	} 

	
	param_list <- list(est = unlist(list('alpha' = alpha)))	
	
	
	out <- Pistar2by2(
					call 	= thecall,
					pistar	= pistar_list, 
					pred	= list(model = M, 
								   unres = O - M, 
								   combi = O),
					data 	= O, 					
					param	= param_list
					)

	
	return(out)
}
