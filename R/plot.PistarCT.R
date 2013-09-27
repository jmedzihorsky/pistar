#	method `plot` for S4 class `PistarCT`
#	Juraj Medzihorsky
#	28 August 2013


setMethod('plot',
		  'PistarCT',
	function(x,
		 ... )
	{	
		o <- x
	
	  	d <- dim(o@pred$model)
	
		if (length(d)!=2){
			stop('implemented only for 2-way tables')
		}

		s <- sum(o@pred$combi)

		swap.rows <- function(data){
			data[nrow(data):1, ]
		}
	
		m_t <- swap.rows(o@pred$model)
		u_t <- swap.rows(o@pred$unres)
		o_t <- swap.rows(o@pred$combi)
	

		l_x <- colnames(m_t)	#	columns on x-axis
		l_y <- rownames(u_t)	#	rows	on y-axis

		if (is.null(l_x)) {
			l_x <- 1:d[2]
		}
	
		if (is.null(l_y)) {
			l_y <- d[1]:1
		}
	
		a_x <- (1:d[2])-0.5
		a_y <- (1:d[1])-0.5

		g <- lapply(d, function(x) 1:x)
		G <- expand.grid(g) - 1 		; colnames(G) <- c('y', 'x')
		M <- cbind(G, as.vector(m_t)/s) ; colnames(M)[3] <- 'm'
		U <- cbind(G, as.vector(u_t)/s) ; colnames(U)[3] <- 'u'
		O <- cbind(G, as.vector(o_t)/s) ; colnames(O)[3] <- 'o'

		x0 <- c(0, 1, 1, 0)
		y0 <- c(0, 0, 1, 1)

		aux.l <- function(x) list('x'=x0+x[2], 'y'=y0+x[1], 'c'=x[3])

		l_m <- apply(M, 1, aux.l)
		l_u <- apply(U, 1, aux.l)
		l_o <- apply(O, 1, aux.l)


		aux.p <- function(main='', ...){
			plot(x=c(0, d[2]), y=c(0, d[1]), type='n', frame=FALSE, asp=1, 
				 xlab='', ylab='', xaxt='n', yaxt='n', main=main, ... )
		}


		aux.c <- function(x, color){
			if (color=='red') {
				color <- rgb(1, 1-x$c, 1-x$c)	
			} else if (color=='green') {
				color <- rgb(1-x$c, 1, 1-x$c)	
			} else if (color=='blue') {
				color <- rgb(1-x$c, 1-x$c, 1)
			} else if (color=='black') {
				color <- rgb(1-x$c, 1-x$c, 1-x$c)
			}
			polygon(x$x, x$y, col=color)
		}

		aux.f <- function(){	
			l <- 6
#			s_c <- (l:0)/l
			s_c <- 1 - c(0.000, 0.050, 0.100, 0.150, 0.250, 0.500, 1.000)
			x_p <- rep(0.5, l+1)
			y_p <- (0:l)+0.5
			l_y <- format(round(1-s_c, 2), nsmall=2)
			l_x <- c('C', 'M', 'U')
		
			plot(x=c(0, 3), y=c(-2, l+3), type='n', frame=F, asp=1, 
				 xlab='', ylab='', xaxt='n', yaxt='n')
			points(x_p+0, y_p, cex=4, pch=22, bg=rgb(s_c, s_c, s_c))			
			points(x_p+1, y_p, cex=4, pch=22, bg=rgb(s_c, s_c,   1))
			points(x_p+2, y_p, cex=4, pch=22, bg=rgb(  1, s_c, s_c))
			text(x_p-1, y_p, labels=l_y, cex=1)	
			text((0:2)+0.5, rep(-0.25, 3), labels=l_x)
		}
	

		par(mfrow=c(2, 2), mar=c(0, 1, 2, 1))
		aux.p('Combined')		
			lapply(l_o, aux.c, col='black')
			axis(1, lty=0, las=0, at=a_x, labels=l_x)		
		aux.p('Model') 			
			lapply(l_m, aux.c, col='blue')
		aux.f()		
		aux.p('Unrestricted')	
			lapply(l_u, aux.c, col='red')
			axis(2, lty=0, las=2, at=a_y, labels=l_y)
	 	}
)
