%%
%%
%%	

\name{summary.Pistar}

\alias{summary,Pistar-method}
\alias{print,SummaryPistar-method}


\title{
	Summarizing \code{"Pistar"} Objects
}
\description{
	\code{summary} method for \code{class} \code{"Pistar"}	
}
\usage{
\S4method{summary}{Pistar}(object, conf = 0.95, pi_side = NULL, par_side = NULL,
        lower = NULL, upper = NULL, bias = FALSE, ...)

\S4method{print}{SummaryPistar}(x, digits = 2, ...)	
}
\arguments{
	\item{object}{
		an object of \code{class} \code{"pistar"}	
	}
	\item{x}{
		an object of \code{class} \code{"summary.pistar"}
	}
	\item{digits}{
		integer indicating the number of decimal places to print
	}
 	\item{conf}{
		Confidence level expressed as a number between 0 and 1.
		e.g. for a 95\% confidence interval supply 0.95
	}
	\item{pi_side}{
		Sidedness of c.i. for \eqn{\pi^{*}}{pi*}.
		\code{NULL} for a two-sided c.i., \code{"lower"} or \code{"upper"} 
		for a one-sided interval. \code{"auto"} selects \code{"lower"} if
		the estimate is larger than 0 and \code{"upper"} if it is
		smaller than 0.
	}
	\item{par_side}{
		Sidedness of c.i. for reported estimates of parameters other than 
		\eqn{\pi^{*}}{pi*}.		
		\code{NULL} for a two-sided c.i., \code{"lower"} or \code{"upper"}
	   	for a one-sided interval. \code{"auto"} selects \code{"lower"} if
		the estimate is larger than 0 and \code{"upper"} if it is
		smaller than 0.
	}
	\item{lower}{
		lowest possible value for one-sided c.i.		
	}
	\item{upper}{
		largest possible value for one-sided c.i.		
	}
	\item{bias}{
		logical:  Apply bias correction?  Bias correction currently 
		not implemented.
	}
	\item{\dots}{
		arguments to be passed to methods; not used
	}	
}
\details{
}
\value{
	Object of \code{class} \code{"SummaryPistar"}, a list with the following
	slots
	\item{oldcall}{
		the matched call inherited from the input object of \code{class} 
		\code{"Pistar"} 	
	}
	\item{pred}{
		inherited from  the input object of \code{class} \code{"Pistar"} 	
	}
	\item{est}{
		A \code{data.frame} with the parameter estimates, optionally also 
		with standard errors, lower and upper endpoints of c.i., sidedness
		of the c.i., and bias (the last is not currently fully implemented)
	}
}
\references{
}
\author{
	Juraj Medzihorsky
}
\note{
}

\seealso{
}
\examples{
	#	create data:
	H <- matrix((1:4)*1e1, byrow=TRUE, ncol=2)

	#	pi* for independence in a 2-by-2 table
	h <- pistar(proc="2by2", data=H, alpha=1, jack=TRUE)

	#	print 'pistar' object
	h

	#	summarize 'pistar' object
	s <- summary(h)

	#	print 'summary.pistar' object
	s

	#	print the 'summary.pistar' object to 4 decimal places
	print(s, digits=4)

	#	compare the structure of the objects
	str(h)
	str(s)

}
\keyword{pistar}
