%%
%%
%%

\name{pistar.ct}
\alias{pistar.ct}


\title{
	The Mixture Index of Fit for a Contingency Table
}
\description{
	\code{pistar.ct} is used to find the value of the \eqn{\pi^{*}}{pi*} for 
	any user-supplied model fit to a contingency table. The only requirements 
	are (1) that the model inputs only a contingency table with non-negative 
	continuous cell values, and (2) outputs a named
	list in which the predicted values are a contingency table named \code{"fit"}.
	Optionally parameter estimates of interest can be outputed as a vector named
	\code{"param"} in the output list.  
	
	\eqn{\pi^{*}}{pi*} for the model of interest is estimated using the algorithm 
	of Rudas, Clogg, and Lindsay (1994).  Standard errors for \eqn{\pi^{*}}{pi*} 
	and any other parameter estimates of interest can be obtained by jackknife as 
	proposed by Dayton (2003).
}
\usage{
pistar.ct(data, fn, from = .Machine$double.neg.eps^0.25, 
          to = 1 - .Machine$double.neg.eps^0.25, jack = FALSE, 
          method = "uniroot", u_iter = 1e3, zeta = 1, 
          lr_eps = .Machine$double.neg.eps^0.25, 
          max_dif = .Machine$double.neg.eps, chi_stat = 0, verbose = TRUE)
}
\arguments{
	\item{data}{
		a contingency table.
	}
	\item{fn}{
		a user supplied function that estimates the model of interests.  
		Must input only the observed values as a contingency table containging
		non-negative continuous cell values. Must output the predicted values 
		as a contingency table as item named \code{"fit"} in a named list.  
		Optionally can output parameter estimates of interest as a vector 
		named \code{"param"} in the output named list. 
	}
	\item{from}{
		numeric: lower bound of the interval of out-of-model proportions to be 
		explored.
	}
	\item{to}{
		numeric: upper bound of the interval of out-of-model proportions to be 
		explored.
	}
	\item{jack}{
		logical: perform jackknife?
	}
	\item{method}{
		character: method with to look for \eqn{\pi^{*}}{pi*}. 
		\code{"uniroot"} uses \code{uniroot}, can be expected to be faster 
		\code{"split"} uses a simple binary search from \code{rcl.s}.
	}
	\item{u_iter}{
		maximum number of iterations for method \code{uniroot} if 
		method \code{"uniroot"}.
	}
	\item{zeta}{
		weighing constant; default is 1.  The EM algorithm might crash due to 
		very low cell values and in such case increasing the zeta might help.
	}
	\item{lr_eps}{
		penalty for finding \eqn{\pi^{*}}{pi*}, the largest small positive 
		number that can be still considered practically indistinguishable 
		from 0.
	}
	\item{max_dif}{
		largest acceptable difference, passed to \code{rcl.em} and \code{rcl.s}.
	}
	\item{chi_stat}{
		\eqn{\chi^2}{Chi squared} statistic penalty; default 0. Supply a 
		different e.g. if you want to find the lower endpoint of a one-sided 
		confidence interval for \eqn{\pi^{*}}{pi*}.
	}
	\item{verbose}{
		logical: print during estimation?
	}
}
\details{
	The EM algorithm implemented here was proposed by Rudas, Clogg and Lindsay 
	(1994).  The jackknife procedure was proposed by Dayton (2003).  The function 
	is developed from J.M.Grego's \code{clr} and \code{clr.root} functions. 
}
\value{
	Object of \code{class} \code{"Pistar"}, \code{"PistarCT"}, and \code{"PistarRCL"} with the 
	following slots:
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
		a list of predicted values with three items:
	   	\describe{
			\item{model}{the model component multiplied by 
				\eqn{(1-\pi^{*})}{(1-pi*)}}
			\item{unres}{the unrestricted component multiplied by 
				\eqn{\pi^{*}}{pi*}}
			\item{combi}{the two-point mixture, i.e.
				\eqn{(1-\pi^{*})M + \pi^{*}U  }{(1-pi*)M + pi*U}}
		}
	}
	\item{data}{
		an \code{array} with the supplied data.
	}
	\item{param}{
		a list of requested estimates of the parameters of interest 
		of the model fit to an \strong{unscaled} model density, i.e. 
		to \eqn{M}{M} and \strong{not} \eqn{(1-\pi)M}{(1-pi) x M}.
	   	\describe{		
			\item{est}{the estimated values.}
			\item{jack}{from each jackknife replication.}
		}
	} 
	\item{llrs}{
		a list of values of log-likelihood ratio statistics
	   	\describe{		
			\item{est}{for the supplied data.}
			\item{jack}{vector of values from each jackknife.}
		}
	}
	\item{iter}{
		a list of the numbers of iterations of either \code{uniroot} or 
		\code{rcl.s}
	   	\describe{		
			\item{est}{for the supplied data.}
			\item{jack}{from each jackknife replication.}
		}
	}
}
\references{
	Dayton, C. M. (2003) Applications and computational strategies for the 
	two-point mixture index of fit. \emph{British Journal of Mathematical & 
	Statistical Psychology}, 56, 1-13.

	Grego, J. M. \code{clr} and \code{clr.root} functions available at 
	\url{http://www.stat.sc.edu/~grego/courses/stat770/CLR.txt}

	Rudas, T., Clogg, C. C., Lindsay, B. G. (1994) A New Index of Fit Based on 
	Mixture Methods for the Analysis of Contingency Tables. \emph{Journal of 
	the Royal Statistical Society. Series B (Methodological)}, Vol. 56, No. 4, 
	623-639.

	Rudas, T. (2002) `A Latent Class Approach to Measuring the Fit of a 
	Statistical Model' in Hagenaars, J. A. and McCutcheon, A. L. (eds.) Applied 
	Latent Class Analysis. Cambridge University Press. 345-365.
}
\author{
	Juraj Medzihorsky

	Developed from J.M.Grego's \code{clr} and \code{clr.root} functions.
}
\note{}

\seealso{
	\code{\link[pistar:piplot.ct]{piplot.ct}}
	\code{\link[pistar:rcl.em]{rcl.em}}
	\code{\link[pistar:rcl.s]{rcl.s}}	
}
\examples{
	#	load data
	data(Fienberg1980a)

	#	define a function: log-linear model of independence in a
	#	2-way table
	mf <- function(x){
		loglin(table=x, margin=list(1,2), fit=TRUE, print=FALSE)
	}

	#	find pi*
	p <- pistar(proc="ct", data=Fienberg1980a, fn=mf, jack=FALSE)

	p

	summary(p)

	plot(p)

}
\keyword{ pistar }
\keyword{ mixture }
\keyword{ LCA }
\keyword{ latent class analysis }


