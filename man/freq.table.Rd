%%
%%
%%

\name{freq.table}
\alias{freq.table}

\title{
	Frequency Table of Integers in a Vector
}
\description{
	\code{freq.table} is used to obtain a frequency table of values from a 
	numeric vector.  Unlike \code{table} it includes the frequency of each 
	integer value on the interval from the smallest to the largest observed 
	value, and thus can contain also zeros.
}
\usage{
	freq.table(x)
}
\arguments{
 	\item{x}{
		a numeric vector.
	}
}
\details{
	\code{freq.table} first coerces the input vector into type \code{integer} 
	and then creates a table of frequencies of all integers on the closed 
	interval from the smallest to the largest inputed integer.
}
\value{
	A named vector of frequencies.
}
\references{}
\author{
	Juraj Medzihorsky
}
\note{}

\seealso{
	\code{\link[base:table]{table}}
}
\examples{
	set.seed(1989)
	y <- c(rpois(1e1, 3), rpois(1e1, 1e1))
	freq.table(y)	
	#	compare with table()
	table(y)
}
\keyword{table}
\keyword{frequency}

