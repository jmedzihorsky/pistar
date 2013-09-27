%%
%%
%%

\name{pistar.uv}
\alias{pistar.uv}


\title{
	The Mixture Index of Fit for Univariate Distributions
}
\description{
	\code{pistar.uv} is used to estimate the \eqn{\pi^{*}}{pi*} index of fit
	for any user-supplied univariate distribution.  The user must supply a 
	probability mass or density function that inputs the data as the first 
	argument and the parameters as the next arguments. See \sQuote{Details} 
	for the estimation procedures.
	Standard errors available via jackknife as suggested by Dayton (2003).
}
\usage{
pistar.uv(data, dfn, n_par = NULL, inits = NULL, discrete = FALSE, 
          freq = FALSE, lower = NULL, upper = NULL, jack = FALSE, 
          method = "Nelder-Mead", control = list(maxit = 2000), 
          verbose = TRUE, npk = 1e3, eps = .Machine$double.neg.eps^0.5)
}
\arguments{
	\item{data}{
		a vector or a frequency table.
	}
	\item{dfn}{
		function:  probability mass or density function that inputs	the data as 
		the first argument and the parameters as the arguments that immediately 
		follow it.
	}
	\item{n_par}{
		numeric:  number of parameters.  Either \code{n_par} or \code{inits}
		must be supplied.  If only \code{n_par} is supplied initial values are 
		generated internally and might not always be suitable.
	}
	\item{inits}{
		a vector or list of initial values of parameters supplied to \code{optim}.
		If named the parameter names are preserved in the output.
	}
	\item{discrete}{
		logical: is the distribution discrete?
	}
	\item{freq}{
		logical: is the supplied data a frequency table? Relevant only if
		\code{discrete} is \code{TRUE}.
	}
	\item{lower}{
		numeric: a vector of lower bounds for parameters.
	}
	\item{upper}{
		numeric: a vector of upper bounds for parameters.
	}
	\item{jack}{
		logical: perform jackknife?
	}
	\item{method}{
		\code{method} argument for optim.  Default \code{"Brent"} for 
		mono-parameter functions and \code{"Nelder-Mead"} for 
		multi-parameter functions. See \code{optim} for details on 
		methods.
	}
	\item{control}{
		list supplied to \code{optim}, see \code{optim}.
	}
	\item{verbose}{
		logical: print during estimation?
	}
	\item{npk}{
		an integer indicating the number of points for \code{density}; used
		if \code{discrete = FALSE}.
	}
  	\item{eps}{
		numeric: the smallest number practically indistinguishable from 0. 
		Used only if \code{discrete = TRUE}.
	}
}
\details{
	The general procedure for discrete and continuous distributions is the
	same: a general purpose optimization method is used to find such values
	of the parameters of the supplied distribution that minimize the following
	quantity: 1 minus the inverse of the ratio of the model and the observed
	density at the point of their supports where this ratio is highest. This
	quantity is \eqn{\pi^{*}}{pi*}.	

	The procedure for discrete distributions differs from the one for the
	continuous distributions in the method used to obtain the observed 
	density. In the discrete case the observed frequencies are used for the 
	observed density.  In the continuous case a kernel density is estimated 
	using \code{density} with gaussian kernel.
}
\value{
	Object of \code{class} \code{"Pistar"}, and \code{"PistarUV"} and
   	depending on the \code{discrete}  argument of the function 
	either \code{"PistarDUV"} or \code{"PistarCUV"}
	with the following slots:
	\item{call}{
		the matched call.
	}
	\item{pistar}{
		a list of  estimated values of the mixture index of fit.
	   	\describe{		
			\item{est}{for the supplied data.}
			\item{jack}{vector of values from jackknife.}
		}
	}  
	\item{pred}{
		if \code{discrete = TRUE} a list of predicted values with three items:
	   	\describe{
			\item{model}{the model component multiplied by 
				\eqn{(1-\pi^{*})}{(1-pi*)}}
			\item{unres}{the unrestricted component multiplied by 
				\eqn{\pi^{*}}{pi*}}
			\item{combi}{the two-point mixture, i.e.
				\eqn{(1-\pi^{*})M + \pi^{*}U  }{(1-pi*)M + pi*U}}
		}
		if \code{discrete = FALSE} the list also contains three components with
		the same names, but they contain the values of the scaled densities at
		\code{npk} (i.e. by default \code{1e3}) points. 
	}
	\item{data}{
		the supplied data.
	}
	\item{param}{
		a list of parameter estimates of interest:
	   	\describe{		
			\item{est}{the estimated values.}
			\item{jack}{from each jackknife replication.}
		}
	} 
	\item{meth}{
		\code{method} of \code{optim} used.
	}
	\item{conv}{
		a list of integer codes from \code{optim} that indicate convergence of the
		optimization algorithm. Any value that is not 0 suggests problems.  See 
		\code{optim} for details.
	   	\describe{		
			\item{est}{from estimation with the supplied data.}
			\item{jack}{from jackknife replications.}
		}
	}
	\item{mess}{
		a list of messages pased from \code{optim}. See \code{optim} for details.
	   	\describe{		
			\item{est}{From the main estimation.}
			\item{jack}{From jackknife replications.}
		}
	}
}
\references{
	Dayton, C. M. (2003) Applications and computational strategies for the 
	two-point mixture index of fit. \emph{British Journal of Mathematical & 
	Statistical Psychology}, 56, 1-13.
}
\author{
	Juraj Medzihorsky
}
\note{
	The application of the mixture index of fit for discrete distributions was 
	proposed by Dayton (2003).
}


\seealso{
	\code{\link[stats:optim]{optim}}
	\code{\link[stats:density]{density}}	
	\code{\link[pistar:freq.table]{freq.table}}	
}
\examples{
	#	(1)	discrete
	#	simulate data
	set.seed(1989)
	e <- c(rpois(1e3, 2), rpois(2e2, 5))

	#	make a frequency table
	te <- freq.table(e)

	#	define a funcion for a slice from Poisson
	md <- function(x, l, lo=0, up=5){
		z <- dpois(x, l)
		z[x<lo] <- 0
		z[x>up] <- 0
		z <- z/sum(z)
		return(z)
	}

	#	find pi*
	pe <- pistar(proc='uv', data=te, dfn=md, n_par=1,
				 discrete=TRUE, freq=TRUE, jack=FALSE)

	pe

	summary(pe)

	plot(pe)


	#	(2)	continuous
	#	simulate data
	set.seed(1989)
	y <- c(rnorm(1e2, 0, 2), runif(2e1, -1, 1))

	#	find pi* and parameters for normal dist.
	py <- pistar(proc='uv', data=y, dfn=dnorm, n_par=2, discrete=FALSE, 
				 jack=FALSE)

	py

	summary(py)

	plot(py)	

}

\keyword{pistar}
\keyword{univariate}

