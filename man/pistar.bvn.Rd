%%
%%
%%

\name{pistar.bvn}
\alias{pistar.bvn}

\title{
	The Mixture Index of Fit for Bivariate Normal Independence
}
\description{
	\code{pistar.bvn} is used to estimate the \eqn{\pi^{*}}{pi*} mixture index
   	of fit for independence in a bivariate normal distribution using the 
	relationship between \eqn{\pi^{*}}{pi*} and Pearson correlation coefficient.
}
\usage{
	pistar.bvn(x, y, conf = 0.95, na.action, alt = "two.sided", verbose = TRUE)
}
\arguments{
	\item{x}{
		a numeric vector containing the first variable.
	}
	\item{y}{
		a numeric vector containing the second variable.	
	}
	\item{conf}{
		confidence level.
	}
	\item{na.action}{
		passed to \code{cor.test}.
	}
	\item{alt}{
		alternative hypothesis, determines the shape of the confidence interval;
		passed to \code{cor.test}.
	}
	\item{verbose}{
		logical: print during estimation?
	}
}
\details{
}
\value{
	Object of \code{class} \code{"Pistar"}, and \code{"PistarBVN"}
	with the following slots:
	\item{call}{
		the matched call.
	}
	\item{pistar}{
		a list of  estimated values of the mixture index of fit:
	   	\describe{		
			\item{est}{for the supplied data.}
			\item{jack}{vector of values from jackknife.}
		}
	}
	\item{pred}{
		not yet implemented.
	}
	\item{data}{
		a \code{data.frame} with the supplied data.
	}
	\item{param}{
		not yet implemented.
	}
	\item{interval}{
		confidence interval for pi*.
	}
	\item{conf}{
		confidence level.
	}
	\item{alt}{
		the supplied alternative hypothesis.
	}

}
\references{
	Rudas, T., Clogg, C. C., Lindsay, B. G. (1994) A New Index of Fit Based on 
	Mixture Methods for the Analysis of Contingency Tables. \emph{Journal of 
	the Royal Statistical Society. Series B (Methodological)}, 
	Vol. 56, No. 4, 623-639.
}
\author{
	Juraj Medzihorsky
}
\note{}

\seealso{
	\code{\link[stats:cor.test]{cor.test}}
	\code{\link[stats:cor.test]{pearson2pistar}}	
}
\examples{
	#	simlate data
	set.seed(1989)
	n <- 1e2
	a <- rnorm(n)
	b <- rnorm(n)

	#	find pi*
	o <- pistar.bvn(x=a, y=b)

	o

}
\keyword{pistar}

