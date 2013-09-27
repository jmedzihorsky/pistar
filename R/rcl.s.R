rcl.s <-
function(FNS,
			 ... ,
			 y_goal = .Machine$double.neg.eps^0.5,
			 x_lo = .Machine$double.neg.eps^0.25,
			 x_up = 1-.Machine$double.neg.eps^0.25,
			 s_tol = .Machine$double.neg.eps^0.5,
			 s_mit = 1e2,
			 trace_plot = FALSE)
{
	y <- p <- numeric(0)

	i <- 0

	d_y <- s_tol + 1
	
	while( (i < s_mit) & (d_y > s_tol) ){

		i <- i + 1
		
		m <- (x_lo + x_up)/2
		p[i] <- m
		y[i] <- FNS(m, ... )
				
		d_y <- abs(y[i] - y_goal)
		
		ifelse(y[i] < y_goal, x_up <- m, x_lo <- m)
	
	}
	

	if (i==s_mit) {
		warning('Permitted number of search iterations reached, try increasing it.')
	}

	if (trace_plot) {
		plot(1:i, p, type='l', xlab='iteration', ylab=expression(symbol(pi)))
	}
		
	return(data.frame('pi'=p, 'lr'=y))
}
