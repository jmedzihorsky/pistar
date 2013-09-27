\name{rcl.em}
\alias{rcl.em}
\title{
	Rudas-Clogg-Lindsay EM Algorithm
}
\description{
	\code{rcl.em} is used to fit a two-point mixture composed of a 
	user-supplied model of interest and an unrestricted distribution fit to a 
	contingency table with supplied mixing proportions using the 
	Rudas-Clogg-Lindsay (1994) EM algorithm.
}
\usage{
rcl.em(pi_out, FNEM, data, max_dif = .Machine$double.neg.eps^0.5, 
       zeta = 1, lr_only = TRUE, chi_stat = 0, 
       lr_eps = .Machine$double.neg.eps^0.25)
}
\arguments{
	\item{pi_out}{
		out-of-model proportion, i.e. the mixing weight of the unrestricted
		component
	}
	\item{FNEM}{
		user-supplied function that estimates the model of interests.  
		Must input only the observed values as a contingency table.
		Must output the predicted values as a contingency table as 
		item named \code{"fit"} in a named list.  
		Optionally can output parameter estimates of interest as a vector 
		named \code{"param"} in the outputed named list. 
	}
	\item{data}{
		a contingency table.
	}
	\item{max_dif}{
		largest acceptable difference, i.e. the largest number practically
		indistinguishable from 0.
	}
	\item{zeta}{
		weighing constant; default is 1.  The EM algorithm might crash due to 
		very low cell values; in such case increasing the zeta might help.
	}
	\item{lr_only}{
		logical: return only the value of the log-likelihood ratio statistic?
	}
	\item{chi_stat}{
		\eqn{\chi^2}{Chi squared} statistic penalty; default 0. Supply a 
		different value e.g. if you want to find the lower endpoint of a 
		one-sided confidence interval for \eqn{\pi^{*}}{pi*}.
	}
	\item{lr_eps}{
		penalty for finding \eqn{\pi^{*}}{pi*}, the largest small positive 
		number that can be still considered practically indistinguishable 
		from 0.
	}
}
\details{
}
\value{
	A named list with the following components:
	(if \code{lr_only} is \code{TRUE} then the list contains only 
	the \code{"lr"} component)
	\item{pi_out}{
		the out-of-model proportion, i.e. the mixing weight of the 
		unrestricted component
	}
	\item{param}{
		a vector of the estimated parameter values fit to an \strong{unscaled}
		model density, i.e. to \eqn{M}{M} and \strong{not} 
		\eqn{(1-\pi)M}{(1-pi) x M}.
	}
	\item{lr}{
		general contingency table log-likelihood ratio statistic for the 
		two-point mixture.
	}
	\item{model}{
		scaled density of predicted values following the model of
		interest, i.e. \eqn{(1-\pi)M}{(1-pi) x M}.
	}
	\item{unrestricted}{
		Scaled density of predicted values following unrestricted
		component, i.e. \eqn{\pi U}{pi x U}
	}
	\item{predicted}{
		values predicted by the two-point mixture, i.e. 
		\eqn{(1-\pi)M + \pi U}{(1-pi) x M + pi x U}.
	}
}
\references{
	Rudas, T., Clogg, C. C., Lindsay, B. G. (1994) A New Index of Fit Based on 
	Mixture Methods for the Analysis of Contingency Tables. \emph{Journal of 
	the Royal Statistical Society. Series B (Methodological)}, Vol. 56, No. 4, 
	623-639.

	Grego, J. M. \code{clr} and \code{clr.root} functions available at 
	\url{http://www.stat.sc.edu/~grego/courses/stat770/CLR.txt}
}
\author{
	Juraj Medzihorsky

	Developed from J.M.Grego's functions, see \sQuote{References}
}
\note{
}

\seealso{
	\code{\link[pistar:pistar.ct]{pistar.ct}}
}
\examples{}
\keyword{mixture}

