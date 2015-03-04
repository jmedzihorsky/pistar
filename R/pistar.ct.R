#	`pistar.ct` with S4 class output
#	Juraj Medzihorsky
#	2014-12-08


pistar.ct <-
	function(data,
			 fn,
			 from 		= .Machine$double.neg.eps^0.25,
			 to 		= 1-.Machine$double.neg.eps^0.25,				 
			 jack 		= FALSE,
 			 method 	= 'uniroot',
			 u_iter 	= 1e3,		
			 zeta 		= 1,
			 lr_eps 	= .Machine$double.neg.eps^0.25,			 
			 max_dif 	= .Machine$double.neg.eps,
			 chi_stat 	= 0,			 
			 verbose 	= TRUE)
{
	thecall <- match.call()
	
	O <- data
	
	if (method=='uniroot') {
			
		u <- uniroot(f = rcl.em, 
					 interval = c(from, to), 
					 FNEM = fn, 
					 data = O, 
					 zeta = zeta, 
					 max_dif = max_dif, 
					 chi_stat = chi_stat, 
					 lr_eps = lr_eps, 
					 lr_only = TRUE,
					 maxiter = u_iter)

		if (u$iter==u_iter) {		
			warning('Permitted number of uniroot iterations reached, try increasing it.')
		}
				
		n_iter <- u$iter
		ps_aux <- u$root

			
	} else if (method=='split') {
		
		y <- rcl.s(FNS = rcl.em, 
				   FNEM = fn, 
				   data = O,
				   zeta = zeta, 
				   max_dif = max_dif, 
				   chi_stat = chi_stat, 
				   lr_eps = lr_eps, 
				   x_lo = from, 
				   x_up = to, 
				   s_mit = 1e2) 

		n_iter <- nrow(y)
		ps_aux <- y$pi[nrow(y)]
		
	} else {
		
		stop('Unsupported method. Only methods "uniroot" and "split" supported.')
		
	}

	
	if (verbose) {
		cat(sprintf('\n%s iterations: %.0f\n\npi* = %.3f\n\n', method, n_iter, ps_aux))
	}
	

	res <- rcl.em(pi_out = ps_aux,
				  FNEM = fn,
				  data = O,
				  zeta = zeta,
				  max_dif = max_dif,
				  chi_stat = chi_stat,
				  lr_eps = lr_eps,
				  lr_only = FALSE)
	
	
	pistar_list <- list(est = res$pi_out)
	param_list  <- list(est = res$param)
	llrs_list   <- list(est = res$lr)
	iter_list   <- list(est = n_iter)
	pred_list   <- list(model = res$model, 
						unres = res$unrestricted, 
						combi = res$predicted)
	
	
	if (jack) {
		
		auxjack <- function(x){
			data[x] <- data[x] - 1 
			return(data)
		}

		auxpi <- function(x){
			pistar.ct(fn = fn, 
					  data = x, 
  					  from = from,
					  to = to,
					  method = method,
					  zeta = zeta, 
					  max_dif = max_dif, 
					  chi_stat = chi_stat, 
					  lr_eps = lr_eps, 
					  jack = FALSE,
					  verbose = FALSE)
		}

		rep_vec <- which(as.vector(O)!=0)

		if (verbose) {
			cat('Jackknife in progress, no. rep. =', length(rep_vec), '\n...')
		}
			
		B <- lapply(rep_vec, auxjack)

		jack_all <- lapply(B, auxpi)
			

		pistar_list$jack <- sapply(jack_all, function(x) x@pistar$est)
		iter_list$jack <- sapply(jack_all, function(x) x@iter$est)
		llrs_list$jack <- sapply(jack_all, function(x) x@llrs$est)
		
		
		if (!is.null(param_list$est)) {
		
			j_param <- sapply(jack_all, function(x) x@param$est)

			n_par <- length(param_list$est)
		
			if (n_par==1) {
				j_param <- matrix(j_param)
			} else if (n_par > 1) {
				j_param <- t(j_param)
			}
		
			param_list$jack <- j_param
		}

		if (verbose) {
			cat(' done.\n\n')
		}	
	}

	
	out <- PistarRCL(call	= thecall,
					 data	= O,
					 pistar	= pistar_list,
					 param	= param_list,
					 pred	= pred_list,
					 llrs	= llrs_list,
					 iter	= iter_list)

	
	return(out)	
	
	
}
