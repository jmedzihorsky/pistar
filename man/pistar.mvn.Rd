%%
%%
%%

\name{pistar.mvn}
\alias{pistar.mvn}

\title{
	The Mixture Index of Fit for Multivariate Normal Independence
}
\description{
	\code{pistar.mvn} is used to estimate the value of the \eqn{\pi^{*}}{pi*} 
	mixture index of fit for independence in multivariate normal distribution 
	using the procedure of Knott (2005).  Standard errors can be obtained 
	using jackknife as proposed by Dayton (2003).
}
\usage{
pistar.mvn(data, cor_matrix = FALSE, max_dif = .Machine$double.neg.eps^0.5, 
           jack = FALSE, seed = 1989, lag = c(5, 10), verbose = TRUE)
}
\arguments{
	\item{data}{
		a matrix, or a correlation matrix.
	}
	\item{cor_matrix}{
		logical: is the supplied data a correlation matrix?  If \code{TRUE}
		jackknife cannot be performed.
	}
	\item{max_dif}{
		numeric: maximal acceptable difference between selected iterations
		for the convergence diagnostic.  See \sQuote{Details}.
	}
	\item{jack}{
		logical: perform jackknife?
	}
 	\item{seed}{
		seed for random number generation.
	}
	\item{lag}{
		parameters of the convergence diagnostic, see \sQuote{Details}.
	}
	\item{verbose}{
		logical: print during estimation?
	}
}
\details{
	The function was developed from code published by Knott (2005). 
	
	A simple convergence diagnostic was added to Knott's (2005) procedure.
	The absolute values of the differences between the value of 
	\eqn{\pi^{*}}{pi*} at the current iteration and between \code{lag} 
	iterations and stops the iterations as successful if all the differences 
	are smaller than the constant supplied as \code{max_dif} argument.  
	To check if the algorithm has converged to a global or a local minimum 
	check \code{test} in the output and restart the procedure with
	a different \code{seed} if needed (see Knott 2005 for more details).
}
\value{
	Object of \code{class} \code{"Pistar"}, and \code{"PistarMVN"}
	with the following components:
	\item{call}{
		the matched call.
	}
	\item{pistar}{
		a list of estimated values of the mixture index of fit:
	   	\describe{		
			\item{est}{for the supplied data.}
			\item{jack}{vector of values from jackknife.}
		}
	}
	\item{pred}{
		not yet implemented.
	}
	\item{data}{
		the supplied data.
	}
	\item{param}{
		not yet implemented.
	}
	\item{trace}{
		a list of traces from the iterations:
	   	\describe{		
			\item{est}{vector from estimation with supplied data.}
			\item{jack}{list of traces from jackknife.}
		}
	}
	\item{iter}{
		a list of numbers of the iterations:
	   	\describe{		
			\item{est}{from estimation with supplied data.}
			\item{jack}{a list of traces from jackknife.}
		}
	}
	\item{test}{
		a test statistic to evaluate if the procedure has converged
		to a local or global optimum. See Knott (2005) for more details.
	   	\describe{		
			\item{est}{Vector from estimation with supplied data.}
			\item{jack}{List of vectors from jackknife.}
		}
	}
}
\references{
	Dayton, C. M. (2003) Applications and computational strategies for the 
	two-point mixture index of fit. \emph{British Journal of Mathematical & 
	Statistical Psychology}, 56, 1-13.

	Knott, M. (2005) A measure of independence for a multivariate normal 
	distribution and some connections with factor analysis, 
	\emph{Journal of Multivariate Analysis}, 96, 374-383. 
}
\author{
	Juraj Medzihorsky

	Developed from code published by Knott (2005).
}
\note{}


\seealso{}
\examples{
	#	simulate data
	set.seed(1989)
	n <- 1e2
	A <- cbind(rnorm(n), rnorm(n))

	#	find pi*
	a <- pistar(proc='mvn', data=A, jack=FALSE)

	a

	summary(a)

	plot(a)

}

\keyword{pistar}
\keyword{multivariate normal}
