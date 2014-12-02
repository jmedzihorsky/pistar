#	`pistar.ll` for S4 class
#	Juraj Medzihorsky
#	2014-12-02


pistar.ll <-
	function(data,
			 margin 	= NA,
			 start 		= rep(1, length(data)),
			 eps 		= 1e-1,
			 iter 		= 1e3,
			 param 		= TRUE,
			 print 		= FALSE,
			 from 		= .Machine$double.neg.eps^0.25,
			 to 		= 1-.Machine$double.neg.eps^0.25,				 
			 jack 		= FALSE,		 
			 lr_eps 	= .Machine$double.neg.eps^0.25,			 
			 max_dif 	= .Machine$double.neg.eps^0.5,
			 chi_stat 	= 0,
			 u_iter 	= 1e3,
			 tol 		= .Machine$double.eps^0.25,			 
			 verbose 	= TRUE)
{
	thecall <- match.call()	
	O <- data

	if (is.na(margin)[1]) {
		margin <- as.list(1:length(dim(O)))
	}

	rcl.em.loglin <-
		function(pi_out,
				 lr_only = TRUE)
	{		
		d_o <- dim(O)
 		s_o <- sum(O)  	
 				
		f1 <- loglin(table = O,
					 margin = margin,
					 start = start,
					 eps = eps,
					 iter = iter,
					 fit=TRUE,
					 print = FALSE)$fit

		M_s <- (1-pi_out)*f1
		U_s <- array(pi_out/prod(d_o), d_o)
	
		dif <- 1e0
		
		while (abs(dif) > max_dif) {
			M_w <- O*M_s/(M_s+U_s)
			U_w <- O*U_s/(M_s+U_s)	
			f2 <- loglin(table = M_w,
						 margin = margin,
						 start = start,
						 eps = eps,
						 iter = iter,
						 fit = TRUE,
						 print = FALSE)$fit


			M_n <- (1-pi_out)*f2/sum(f2)
 			U_n <- pi_out*U_w/sum(U_w)
			dif <- sum(abs((M_s+U_s)-(M_n+U_n)))				
			M_s <- M_n
			U_s <- U_n
		}
		
		E <- s_o*(M_n+U_n)
		
		if (lr_only) {
			out <- 2*sum(O*log(O/E)) - lr_eps - chi_stat	
		} else {	
			f3 <- loglin(table = s_o*M_n/(1-pi_out),
						 margin = margin,
						 start = start,
						 eps = eps,
						 iter = iter,
						 param = param,
						 print = FALSE)$param

			out <- list(pi_out = pi_out,
						param = unlist(f3),		
						lr = 2*sum(O*log(O/E)),
						model = s_o*M_n,				   	
						unrestricted = s_o*U_n,
						predicted = s_o*(M_n+U_n))
		}	
			return(out)
	}

			
	u <- uniroot(f = rcl.em.loglin, 
				 interval = c(from, to),
				 tol = tol,
				 maxiter = u_iter)

	if (u$iter==u_iter) {		
		warning('Permitted number of uniroot iterations reached, try increasing it.')
	}
						
	if (verbose) {
		cat(sprintf('\nuniroot iterations: %.0f\n\npi* = %.3f\n\n', u$iter, u$root))
	}
	

	res <- rcl.em.loglin(pi_out = u$root, 
						 lr_only = FALSE)
	
	
	pistar_list <- list(est = res$pi_out)
	param_list  <- list(est = res$param)
	llrs_list   <- list(est = res$lr)
	iter_list   <- list(est = u$iter)
	pred_list   <- list(model = res$model, 
						unres = res$unrestricted, 
						combi = res$predicted)
	
	
	
	if (jack) {
		
		auxjack <- function(x){
			O[x] <- O[x] - 1 
			return(O)
		}

		auxpi <- function(x){
			pistar.ll(data = x,
						  margin = margin,
						  start = start,
						  eps = eps,
						  iter = iter,
						  param = param,
						  print = FALSE,
						  from = from,
						  to = to,
						  jack = FALSE,
						  lr_eps = lr_eps,
						  max_dif = max_dif,
						  chi_stat = chi_stat,
						  u_iter = u_iter, 
						  tol = tol,						  
						  verbose = FALSE)	
		}

		rep_vec <- which(as.vector(O)!=0)

		if (verbose) {
			cat('Jackknife in progress, no. rep. =', length(rep_vec), '\n...\n\n')
		}

		B <- lapply(rep_vec, auxjack)

		jack_all <- lapply(B, auxpi)
			
		pistar_list$jack <- sapply(jack_all, function(x) x@pistar$est)
		iter_list$jack   <- sapply(jack_all, function(x) x@iter$est)
		llrs_list$jack   <- sapply(jack_all, function(x) x@llrs$est)

		
		if (!is.null(param_list$est)) {

			n_par <- length(param_list$est)

			j_param <- sapply(jack_all, function(x) x@param$est)
	
			if (n_par==1) {
				j_param <- matrix(j_param)
			} else if (n_par > 1) {
				j_param <- t(j_param)
			}
		
			param_list$jack <- j_param
		}

		if (verbose) {
			cat(' done\n\n')
		}	
	}




	out <- PistarLL(
					call	= thecall,
					data	= O,	
					pistar	= pistar_list,
					param	= param_list,
					pred	= pred_list,
					llrs	= llrs_list,
					iter	= iter_list	
					)

	
	return(out)	
	
}
