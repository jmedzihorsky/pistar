%%
%%
%%

\name{pistar.ll}
\alias{pistar.ll}

\title{
	The Mixture Index of Fit for Log-linear Models
}
\description{
	\code{pistar.ll} is used to find the value of the \eqn{\pi^{*}}{pi*} index 
	of fit for any log-linear model estimated with \code{loglin}; 
	\eqn{\pi^{*}}{pi*} is estimated using the algorithm of Rudas, Clogg, and 
	Lindsay (1994).  Standard errors for \eqn{\pi^{*}}{pi*} and any other 
	estimates of parameters of interest can be obtained by jackknife as 
	proposed by Dayton (2003).
}
\usage{
pistar.ll(data, margin = list(1, 2), start = rep(1, length(data)), eps = 0.1,
          iter = 1e3, param = TRUE, print = FALSE, 
          from = .Machine$double.neg.eps^0.25, 
          to = 1 - .Machine$double.neg.eps^0.25, jack = FALSE, 
          lr_eps = .Machine$double.neg.eps^0.25, 
          max_dif = .Machine$double.neg.eps^0.5, chi_stat = 0, u_iter = 1e3, 
          tol = .Machine$double.eps^0.25, verbose = TRUE)
}
\arguments{
	\item{data}{
		a contingency table.
	}
	\item{margin}{
  		passed to \code{loglin}.
	}
	\item{start}{
		\code{start} argument of \code{loglin}.
	}
	\item{eps}{
		\code{eps} argument of \code{loglin}.
	}
	\item{iter}{
		maximum number of iterations for \code{loglin}.
	}
	\item{param}{
		logical: return parameter estimates? 
	}
	\item{print}{
		logical: should \code{loglin} print during fitting?
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
	\item{lr_eps}{
		penalty for finding \eqn{\pi^{*}}{pi*}, the largest small positive number 
		that can be still considered practically indistinguishable from 0.
	}
	\item{max_dif}{
		largest acceptable difference, passed to \code{rcl.em}.
	}
	\item{chi_stat}{
		\eqn{\chi^2}{Chi squared} statistic penalty; default 0. Supply a 
		different e.g. if you want to find the lower endpoint of a one-sided 
		confidence interval for \eqn{\pi^{*}}{pi*}.
	}
	\item{u_iter}{
		maximum number of iterations for method \code{uniroot}.
	}
	\item{tol}{
		tolerance passed to \code{loglin}.
	}
	\item{verbose}{
		logical: print during estimation?
	}
}
\details{
	This is a version of the algorithm implemented in 
	\code{pistar.ct} for log-linear models that is speed-optimized.
}
\value{
	Object of 
	\code{class} \code{"Pistar"}, \code{"PistarCT"}, , \code{"PistarRCL"}, and \code{"PistarLL"} 
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
		the supplied data.
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
	\code{\link[pistar:pistar.ct]{pistar.ct}}
	\code{\link[stats:loglin]{loglin}}
}
\examples{
	data(HairEyeColor)

	#	check if the data is an "array"
	is(HairEyeColor, "array")

	#	it is not, so it first needs to be converted:
	HEC <- array(HairEyeColor, 
				 dim=dim(HairEyeColor), 
				 dimnames=dimnames(HairEyeColor))

	#	find pi* for independence in a 3-way table
	p <- pistar(proc="ll", data=HEC, margin=list(1, 2, 3), jack=FALSE)

	p

	summary(p)

	#	plot does not work for n-way tables if n > 2
	#	plot(p)

	
	#	create data
	H <- matrix((1:4)*1e1, byrow=TRUE, ncol=2)

	#	find pi* and model parameter estimates and perform jackknife
	h <- pistar(proc="ll", data=H, margin=list(1, 2), param=TRUE, jack=TRUE)

	h

	summary(h)

}
\keyword{ pistar }
\keyword{ mixture }
\keyword{ loglin }
