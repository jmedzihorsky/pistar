rcl.em <-
function(pi_out,
			 FNEM,
			 data,
			 max_dif = .Machine$double.neg.eps^0.5,
			 zeta = 1,
			 lr_only = TRUE,
			 chi_stat = 0,
			 lr_eps = .Machine$double.neg.eps^0.25)
{
	Z <- data*zeta
	
	d_o <- dim(data)
	s_o <- sum(data)  	
			
	M_s <- (1-pi_out)*FNEM(data)$fit
	U_s <- array(pi_out/prod(d_o), d_o)

	dif <- 1e0
	
	while (abs(dif) > max_dif) {

		M_w <- Z*M_s/(M_s+U_s)
		U_w <- Z*U_s/(M_s+U_s)	

		f2 <- FNEM(M_w)$fit
		
		M_n <- (1-pi_out)*f2/sum(f2)
		U_n <- pi_out*U_w/sum(U_w)
		
		dif <- sum(abs((M_s+U_s)-(M_n+U_n)))
				
		M_s <- M_n
		U_s <- U_n
	}

	
	E <- s_o*(M_n+U_n)

	
	if (lr_only) {
		
		out <- 2*sum(data*log(data/E)) - lr_eps - chi_stat
		
	} else {
		
		out <- list(pi_out = pi_out,
					param = FNEM(s_o*M_n/(1-pi_out))$param,		# ----
					lr = 2*sum(data*log(data/E)),
					model = s_o*M_n,				   	
					unrestricted = s_o*U_n,
					predicted = s_o*(M_n+U_n))

	}
		
		return(out)
		
}
