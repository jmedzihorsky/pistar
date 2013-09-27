%%
%%
%%

\name{piplot.ct}
\alias{piplot.ct}

\title{
	Pre-analysis Plot for \code{\link[pistar:pistar.ct]{pistar.ct}}	
}
\description{
	\code{piplot.ct} is used to inform the choice of the interval on which to
   	look for \eqn{\pi^{*}}{pi*}	with \code{pistar.ct}.  It plots log-likelihood 
	ratio statistic values of a two-point mixture of a user-supplied model and 
	an unrestricted component fit to a contingency table on a specified number 
	of equally-spaced points on a supplied interval.
}                                                                                 
\usage{
piplot.ct(fn, data, ..., from = .Machine$double.neg.eps^0.25,
          to = 1 - .Machine$double.neg.eps^0.25,  n = 10, draw = TRUE, 
          color = "black", zero_line = TRUE, add = FALSE, values = FALSE)
}
\arguments{
	\item{fn}{
	  	a user-supplied function that inputs a contingency table of observed
	   	values and outputs a contingency table of predicted values. The 
		function must output a named list with the contingency table of 
		predicted values named \code{'fit'}.	
	}
	\item{data}{
		a contingency table.	
	}
	\item{\dots}{
		further arguments passed to the user-supplied function.
	}
	\item{from}{
		numeric: lower bound of the interval of out-of-model proportions to be 
		explored.
	}
	\item{to}{
		numeric: upper bound of the interval of out-of-model proportions to be 
		explored.
	}
	\item{n}{
		numeric: number of equally-spaced points from the interval of 
		out-of-model proportions to be explored.
	}
	\item{draw}{
		logical: draw a plot or lines?
	}
	\item{color}{
		color of the line.
	}
	\item{zero_line}{
		logical: plot a horizontal line at 0?
	}
	\item{add}{
		logical: add the line to a plot?
	}
  \item{values}{
		logical: return a dataframe with explored out-of-model proportions and 
		their corresponding log-likelihood ratio statistics?
  }
}
\details{
	Developed from John M. Grego's \code{clr.plot}
}
\value{
	A plot.
	If \code{values = TRUE} returns also an object of class 'PiplotCT' with the 
	following slots:
	\item{pi}{numeric vector of explored out-of-model proportions}
	\item{lr}{numeric vector of corresponding log-likelihood ratio statistics}
	\item{lr_plus_eps}{\code{lr} plus the value of \code{eps}}
}
\references{
	Grego, J.M. \code{clr.plot} function available at 
	\url{http://www.stat.sc.edu/~grego/courses/stat770/CLR.txt}
}
\author{
	Juraj Medzihorsky, developed from John M. Grego's \code{clr.plot}
}
\note{}
\seealso{
	\code{\link[pistar:pistar.ct]{pistar.ct}}	
	\code{\link[pistar:rcl.em]{rcl.em}}	
}
\examples{
	data(Fienberg1980a)

	mf <- function(data) loglin(data, list(1, 2), fit=TRUE, print=FALSE)

	a <- piplot.ct(fn=mf, data=Fienberg1980a, values=TRUE)

	a

	plot(a, color='red')
}
\keyword{pistar}
