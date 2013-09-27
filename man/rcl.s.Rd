%%
%%
%%


\name{rcl.s}
\alias{rcl.s}

\title{
	Simple Line-Splitting Function
}
\description{
	'rcl.s' is used to find the input values under which a user-supplied
	function outputs a specified value by searching on a user-specified 
	interval
}
\usage{
rcl.s(FNS, ..., y_goal = .Machine$double.neg.eps^0.5, 
      x_lo = .Machine$double.neg.eps^0.25, 
      x_up = 1 - .Machine$double.neg.eps^0.25, 
      s_tol = .Machine$double.neg.eps^0.5, 
      s_mit = 1e2, trace_plot = FALSE)
}
\arguments{
	\item{FNS}{
		user-supplied function.  Must input only a single number and
		output only a single number.
	}
	\item{\dots}{
		arguments passed to the user-supplied function.
	}
	\item{y_goal}{
		the goal output value.
	}
	\item{x_lo}{
		the lower endpoint of the interval for the input value.
	}
  	\item{x_up}{
		the upper endpoint of the interval for the input value.		
	}
  	\item{s_tol}{
		tolerance value for convergence diagnostic.  
		If the absolute value of the difference
		between the output value at the current iteration and the
		goal output value is less than this tolerance value the
		algorithm terminates successfully. 
	}
  	\item{s_mit}{
		maximum number of iterations.
	}
  	\item{trace_plot}{
		logical:  return also a traceplot?
	}
}
\details{
}
\value{
	A dataframe with the following columns:
	\item{pi}{
		input values.
	}
	\item{lr}{
		output values.
	}
	If \code{trace_plot = TRUE} also a plot.
}
\references{
}
\author{
	Juraj Medzihorsky
}
\note{
}

\seealso{}
\examples{}
\keyword{line-splitting}
