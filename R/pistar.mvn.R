#	`pistar.mvn` with S4 class output
#	Juraj Medzihorsky
#	2014-12-02

pistar.mvn <-
	function(data		,
			 cor_matrix = FALSE,
			 max_dif 	= .Machine$double.neg.eps^0.5,
			 jack 		= FALSE,
			 seed 		= 1989,
			 lag 		= c(5, 10),
			 verbose 	= TRUE)
{	
	thecall <- match.call()
	
	if (cor_matrix==FALSE) {
		M <- cor(data, use='everything', method='pearson')
	} else if(cor_matrix==TRUE){
		M <- data
	}
	
	d <- dim(M)[1] 

	set.seed(seed)
	E <- c(rnorm(d*d))
	dim(E) <- c(d, d)

	theta <- 1e-2

	z <- rep(0, max(lag))

	dif_pi <- rep(1, length(lag))

	aux <- function(i, z){
		abs( z[length(z)] - z[length(z)-lag[i]] )
	}

	while( mean(dif_pi > max_dif) == 1 ) {
		
		obj		<- -sum(log(diag(E%*%t(E)))) + sum(diag(M%*%E%*%t(E))) - d
		dif		<- solve( diag( diag( E%*%t(E) ) ) ) - M
		E1		<- E + theta * dif%*%E
		obj_new	<- -sum(log(diag(E1%*%t(E1)))) + sum(diag(M%*%E1%*%t(E1))) - d

		if(obj > obj_new){ 
			E <- E1 
			theta <- 2*theta
		} else {
			theta <- theta/2
		}

		pistar <- 1 - exp(-sum(log(diag(E%*%t(E))))/2-sum(log(eigen(M)$values))/2)

		z <- c(z, pistar)
		
		dif_pi <- sapply(1:length(lag), aux, z=z)
		
	}

	test <- eigen(solve(diag(diag(E%*%t(E))))-M)$values
	
	pistar_list <- list(est = pistar)	
	trace_list  <- list(est = list('pi'=z[-c(1:max(lag))]))	
	iter_list   <- list(est = length(z)-max(lag))
	test_list   <- list(est = test)

	if (verbose) {
		cat(sprintf('\niterations: %.0f\n\npi* = %.3f\n\n', iter_list$est, pistar))
	}
	
			
	if ((jack) & (!cor_matrix)) {
		
		if (verbose) {
			cat('jackknife in progress ...')
		}
			
		aux.1.matrix <- function(x, y=data){
			return(y[-x, ])
		}

		aux.2 <- function(x){
			pistar.mvn(data = x,
					   cor_matrix = FALSE,
					   max_dif = max_dif,
					   jack = FALSE,
					   seed = seed,
					   lag = lag,
					   verbose = FALSE)
		}

		B <- lapply(1:nrow(data), aux.1.matrix)	
		
		jack_all <- lapply(B, aux.2)
		
		pistar_list$jack <- sapply(jack_all, function(x) x@pistar$est)
		trace_list$jack  <- sapply(jack_all, function(x) x@trace$est)
		iter_list$jack   <- sapply(jack_all, function(x) x@iter$est)
		test_list$jack   <- sapply(jack_all, function(x) x@test$est)
		
		
		if (verbose) {
			cat(' done\n\n')
		}
	
	} else if ((jack) & (cor_matrix)) {
		cat('Data for jackknife must not be a correlation matrix.\n')			
	}

	pred_list  <- list(model = NA, unres = NA, combi = NA)
	param_list <- list(est = numeric(0))

	out <- PistarMVN(
					 call 	= thecall, 
					 pistar = pistar_list, 
					 pred 	= pred_list, 
					 data 	= as.data.frame(data),					 
					 param 	= param_list,
					 trace 	= trace_list,
					 iter 	= iter_list,
					 test 	= test_list
					 )

	
	
	return(out)
}
