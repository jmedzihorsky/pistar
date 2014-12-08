#	`pistar.uv` with S4 class output
#	Juraj Medzihorsky
#	2014-12-08

pistar.uv <-
	function(data, 
			 dfn, 
			 n_par 		= NULL, 
			 inits 		= NULL,
			 discrete 	= FALSE,
			 freq 		= FALSE,
			 lower 		= NULL,
			 upper 		= NULL,
			 jack 		= FALSE,
			 method 	= "Nelder-Mead",
			 control 	= list(maxit = 2e3),
			 verbose 	= TRUE,
			 npk 		= 1e3,
			 eps 		= .Machine$double.neg.eps^0.5)

{ 
	thecall <- match.call()
	
	
	if (discrete) {

		if (!freq) {
			data <- freq.table(data)
			freq <- TRUE
		}

#		ged <- function( ... , 
		pid <- function( ... , 		# to comply 						
						dist.fun = dfn, 
						Y = data,
						asn = eps,
						r_only = TRUE)
		{		
			p <- unlist(list(...))
			k <- paste('p[', 1:length(p), ']', sep='', collapse=',')
			z <- numeric(0)			
			f <- paste('z <- dist.fun(y_min:y_max,', k, ')')
		
			y_val <- as.numeric(names(Y))
			y_min <- min(y_val) - 1
			y_max <- max(y_val)	+ 1
			
			eval(parse(text=f))
			
			z <- z * sum(Y)
			z[z==0] <- asn
			O <- c(asn, Y, asn)
					
			a <- max(z/O)
			r <- 1 - 1/a   	
		
			if (r_only) {
				return(r)		
			} else {
				m <- z[2:(length(z)-1)]/a
				return(list(pistar = r,
							model = m,
							unrestricted = Y-m,
							predicted = Y))
			}	
		}

		ged <- pid		# to comply		
		
	} else {
		
#		ged <- function( ... ,
		pic <- function( ... ,			#	to comply					   	
						dist.fun = dfn, 
						y = data,
						n = npk,									
						r_only = TRUE)
		{
			K <- density(y, n = n)
			
			p <- unlist(list(...))
			k <- paste('p[', 1:length(p), ']', sep='', collapse=',')	
			z <- numeric(0)						
			f <- paste('z <- dist.fun(K$x,', k, ')')
			
			eval(parse(text=f))
			
			a <- max(z/K$y)
			r <- 1 - 1/a
			
			if (r_only) {
				return(r)	
			} else {
				m <- z/a
				return(list(pistar = r,
							model = data.frame(val=K$x, p=m),
							unrestricted = data.frame(val=K$x, p=K$y-m),
							predicted = data.frame(val=K$x, p=K$y)))
			}	
		}	

		ged <- pic			# to cpmply		

	}


	
	if ( is.null(n_par) * (!is.null(inits)) ){
		n_par <- length(inits)
	}
	

	if ( (!is.null(lower)) & (length(lower)!=n_par) ) {
		stop('lower must be supplied for each parameter')
	}
	if ( (!is.null(upper)) & (length(upper)!=n_par) ) {
		stop('upper must be supplied for each parameter')
	}

	
	
	if (n_par==1) {
	
		method <- 'Brent'	
		
		if ( is.null(lower) ) {
			lower <- min(data)
		}

		if ( is.null(upper) ) {
			upper <- max(data)
		}
		
		if (is.null(inits)){
			inits <- mean(lower, upper)
		}
		
		o <- optim(par = inits, 
				   fn = ged, 
				   method = 'Brent', 
				   lower = lower, 
				   upper = upper,
				   control = control)	
		
	} else if (n_par>1) {
		
		if (is.null(inits)){
			inits <- rep(0.5, n_par)
		}
		
		
		if (method=='L-BFGS-B') {
		
			o <- optim(par = inits,
					   fn = ged,
					   method = method,
					   lower = lower,
					   upper = upper,
					   control = control) 
			
		} else {
		
			o <- optim(par = inits, 
				   fn = ged, 
				   method = method,
				   control = control) 	
				
		}
		
	}
	
	if (o$convergence!=0) {
		warning('Optimization algorithm did not converge.')
	}
	
	
	res <- ged(o$par, r_only = FALSE)

		
	pistar_list <- list(est = res$pistar)
	param_list  <- list(est = o$par)
	conv_list   <- list(est = o$convergence)
	mess_list   <- list(est = o$mess)
	pred_list   <- list(model = res$model,
						unres = res$unrestricted,
						combi = res$predicted)

				 	 

	if (verbose) {
		cat(paste('\npi* = ', round(res$pistar, 3), '\n\n', sep=''))
	}

	
	
	if (jack) {

		aux.1.disc <- function(x){
			data[x] <- data[x] - 1
			return(data)
		}

		aux.1.cont <- function(x){
			return(data[-x])
		}
		
		
		aux.2 <- function(x){
			pistar.uv(data = x,
					  dfn = dfn,
					  n_par = n_par,
					  inits = inits,
					  discrete = discrete,
					  freq = freq,
					  lower = lower,
					  upper = upper,
					  jack = FALSE,
					  control = control,
					  verbose = FALSE,
					  eps = eps)
		}


		if (discrete) {
			rep_vec <- which(as.vector(data)!=0)
			B <- lapply(rep_vec, aux.1.disc)	
		} else {
			rep_vec <- 1:length(data)
			B <- lapply(rep_vec, aux.1.cont)
		}
		
		if (verbose) {
			cat('Jackknife in progress, no. rep. =', length(rep_vec), '\n...')
		}
						
		jack_all <- lapply(B, aux.2)
			

		j_param <- sapply(jack_all, function(x) x@param$est)
		
		if (n_par==1) {
			j_param <- matrix(j_param)
		} else if (n_par > 1){
			j_param <- t(j_param)
		}
		
		param_list$jack <- j_param
		pistar_list$jack <- sapply(jack_all, function(x) x@pistar$est)
		conv_list$jack   <- sapply(jack_all, function(x) x@conv$est)
		mess_list$jack   <- unlist(sapply(jack_all, function(x) x@mess$est))
		
		
		if (verbose) {
			cat(' done.\n\n')
		}			
	} 
	
	
	if (discrete) {
		
		out <- PistarDUV(call 	= thecall,
						 data	= data,
						 pistar	= pistar_list,
						 pred	= pred_list,
						 param	= param_list,
						 meth	= method,
						 conv	= conv_list,
						 mess	= mess_list)
		
	} else {
		
		out <- PistarCUV(call 	= thecall,
						 data	= data,
						 pistar	= pistar_list,
						 pred	= pred_list,
						 param	= param_list,
						 meth	= method,
						 conv	= conv_list,
						 mess	= mess_list)
		
	}
	

	return(out)

}
