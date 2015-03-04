rcl.em <-
function(pi_out,
			 FNEM,
			 data,
			 max_dif = .Machine$double.neg.eps,
			 zeta = 1,
			 lr_only = TRUE,
			 chi_stat = 0,
			 lr_eps = .Machine$double.neg.eps^0.25)
{
	Z <- data*zeta
	
	d_o <- dim(data)
	s_o <- sum(data)  	
		
	pi_in <- 1-pi_out	
	M_s <- pi_in*FNEM(data)$fit/s_o
	U_s <- array(pi_out/prod(d_o), d_o)

	dif <- 1e0
	
	while (dif > max_dif) {

		B_s <- M_s + U_s
		M_w <- Z*M_s/B_s
		U_w <- Z*U_s/B_s	

		f2 <- FNEM(M_w/pi_in)$fit
		
		M_n <- pi_in*f2/sum(f2)
		U_n <- pi_out*U_w/sum(U_w)
		
		dif <- sum((B_s-M_n-U_n)^2)
				
		M_s <- M_n
		U_s <- U_n
	}

	
	E <- s_o*(M_n+U_n)

	fil <- data!=0
	lr_stat <- 2*sum(data[fil]*log(data[fil]/E[fil]))

	if (lr_only) {

		out <- lr_stat - lr_eps - chi_stat

	} else {

		out <- list(pi_out = pi_out,
					param = FNEM(s_o*M_n/pi_in)$param,		# ----
					lr = lr_stat,
					model = s_o*M_n,				   	
					unrestricted = s_o*U_n,
					predicted = s_o*(M_n+U_n))

	}
	
	return(out)
}
