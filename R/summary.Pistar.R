#	`summary` function for class `Pistar`
#	Juraj Medzihorsky	
#	2014-12-08


setMethod('summary',
		  'Pistar',
	function(object,
			 conf 		= 0.95,
			 pi_side 	= NULL,
			 par_side 	= NULL,
			 lower 		= NULL,	#	???
			 upper 		= NULL,	#	???
			 bias 		= FALSE,
			 ...)
	{
	
	o <- object

	if ( is(o, 'PistarCT') ) {
		ct_jack <- TRUE
	} else if ( is(o, 'PistarDUV') ) {	
		ct_jack <- TRUE		
	} else if ( is(o, 'PistarCUV') ) {	
		ct_jack <- FALSE
	} else if ( is(o, 'PistarMVN') ) {	
		ct_jack <- FALSE		
	} else {
		stop('This subclass of class `Pistar` is not yet supported.')	
	}		
	
	if ( (conf>1) & (conf<=1e2) ){
		conf <- conf/1e2
	} else if ( (conf<0) | (conf>1e2) ){
		stop('Invalid confidence level.')
	}
	
	n_par <- length(o@param$est)

	
	if (!is.null(o@pistar$jack)) {
	
		pi_p <- pool.jack(data 		= o@data,
						  ct 		= ct_jack,
						  estimate 	= o@pistar$est,
						  jack_est 	= o@pistar$jack,
						  conf 		= conf,
						  lower 	= 0,	#	`lower` arg used only for `other quantities`
						  upper 	= 1,	#	`upper` arg used only for `other quantities`
						  side 		= pi_side,
						  bias 		= bias)
		
		o_pistar <- data.frame(est 		= o@pistar$est,
							   se 		= pi_p$se,
							   lower 	= pi_p$low,
							   upper 	= pi_p$upp,
							   theta 	= pi_p$theta,
							   conf 	= pi_p$conf,
							   side 	= pi_p$side,
							   bias 	= pi_p$bias)

	} else {
		
		o_pistar <- data.frame(est = o@pistar$est)
		
	}

	
	if ((!is.null(o@param$est)) & (!is.null(o@param$jack))) {
	
		l_param <- list()
	
		for(i in 1:n_par){
			t_p <- 
				pool.jack(data 		= o@data,
						  ct 		= ct_jack,
						  estimate 	= o@param$est[i],
						  jack_est 	= o@param$jack[, i],
						  conf 		= conf,
						  lower 	= ifelse(is.null(lower), -Inf, lower[i]),
						  upper 	= ifelse(is.null(upper),  Inf, upper[i]),
						  side 		= par_side,
						  bias 		= bias)
			
			l_param[[i]] <-  
				data.frame(est 		= o@param$est[i],
						   se 		= t_p$se,
						   lower 	= t_p$low,
						   upper 	= t_p$upp,
						   theta	= t_p$theta,
						   conf 	= t_p$conf,
						   side 	= t_p$side,
						   bias 	= t_p$bias)
		}

		o_param <- do.call('rbind', l_param)
		p_n <- names(o@param$est)
		
	} else if ((!is.null(o@param$est)) & (is.null(o@param$jack))) { 
		
		if ( is(o, 'Pistar2by2') ) {
			o_param <- p_n <- NULL		
		}	else {
			o_param <- data.frame(est = o@param$est)	
			p_n <- names(o@param$est)		
		}
		
		
	} else {
		
		o_param <- data.frame(est = o@param$est)	
		p_n <- names(o@param$est)
		
	}


	if (ncol(o_pistar)>1) {	
		if (is.na(o_pistar$bias)) {
			take <- c('est', 'se', 'lower', 'upper', 'conf', 'side', 'bias')
			S <- rbind(o_pistar[, take], o_param[, take])
		} else {
			take <- c('theta', 'se', 'lower', 'upper', 'conf', 'side', 'bias')
			S <- rbind(o_pistar[, take], o_param[, take])
		}
	} else {
		S <- rbind(o_pistar, o_param)
	} 
						  

	if ( nrow(S)>1 ) {

		if (is.null(p_n)) {
			p_n <- 1:n_par
		} 
	   	
		if ( mean(p_n==1:n_par)==1 ) {
			p_n <- paste('Par[', 1:n_par, ']', sep='')
		}
		
	}

	
	rownames(S) <- c('pi*', p_n) 
		
	c_n <- c('Estimate', 
			 'Std.Error', 
			 sprintf('%.2f Low.', conf),
			 sprintf('%.2f Upp.', conf),			 
			 'Conf.', 
			 'Sided', 
			 'Bias')


	colnames(S) <- c_n[1:ncol(S)]
	
  	
	out <- SummaryPistar(oldcall= o@call,
						 pred	= o@pred, 
						 est	= S)

	return(out)
	
	})
