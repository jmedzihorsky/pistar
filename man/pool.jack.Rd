%%
%%
%%

\name{pool.jack}
\alias{pool.jack}

\title{
	Jackknife Standard Errors and Confidence Intervals
}
\description{
	\code{pool.jack} is used to obtain standard errors and confidence intervals
	from the output of jackknife procedures.
}
\usage{
pool.jack(data, ct = TRUE, estimate, jack_est, side = NULL, conf = 0.95, 
          lower = -Inf, upper = Inf, bias = FALSE)
}
\arguments{
	\item{data}{
		the data.
	}
	\item{ct}{
		logical: is the data a contingency table?
	}
 	\item{estimate}{
		the value of the estimate.
	}
	\item{jack_est}{
		the vector of estimates from jackknife replications.
	}
 	\item{side}{
		\code{NULL} for a two-sided c.i., \code{"lower"} or \code{"upper"} for
		a one-sided interval. \code{"auto"} selects \code{"lower"} if the 
		estimate is larger than 0 and \code{"upper"} if it is smaller than 0.
	}
 	\item{conf}{
		confidence level expressed as a number between 0 and 1.
		e.g. for a 95\% confidence interval supply 0.95
	}
	\item{lower}{
		lowest possible value for one-sided c.i.		
	}
	\item{upper}{
		largest possible value for one-sided c.i.		
	}
  	\item{bias}{
		logical: apply bias correction?  Bias correction currently 
		not implemented.
  	}
}
\details{
}
\value{
	A named list with the following components:
	\item{se  }{
		standard error.
	}
	\item{low }{
		lower endpoint of c.i.
	}
	\item{upp }{
		upper endpoint of c.i.	
	}
	\item{conf}{
		confidence level of the c.i.
	}
	\item{side}{
		\code{"lower"} and \code{"upper"} for one-sided c.i., 
		\code{"both"} for a two-sided c.i.
	}
	\item{bias}{
		implementation of bias correction not finished.
	}
}
\references{
	Efron, Bradley, and Robert Tibshirani. (1993). An introduction to the 
	bootstrap. Vol. 57. CRC press.
}
\author{
	Juraj Medzihorsky
}
\note{}
\seealso{}
\examples{}
\keyword{jackknife}
