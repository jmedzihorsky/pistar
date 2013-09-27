%%
%%
%%

\name{pistar.2by2}
\alias{pistar.2by2}

\title{
	 The Mixture Index of Fit for Odds Ratios in 2-by-2 Tables
}
\description{
	\code{pistar.2by2} is used to estimate \eqn{\pi^{*}}{pi*} for a given 
	cross-product ratio (i.e. odds ratio) in a 2-by-2 table using the method
	devised by Clogg, Rudas, and Xi (1995).
}
\usage{
pistar.2by2(data, alpha = 1, jack = FALSE, verbose = TRUE)
}
\arguments{
	\item{data}{
		a 2-by-2 contingency table.
	}
	\item{alpha}{
		numeric: cross-product ratio
	}
	\item{jack}{
		logical: perform jackknife?
	}
	\item{verbose}{
		logical: print during estimation?
	}
}
\details{
}
\value{
	Object of \code{class} \code{"Pistar"}, \code{"PistarCT"}, and \code{"Pistar2by2"} 
	with the following slots:
	\item{call}{
		the matched call.
	}
	\item{pistar}{
		a list of  estimated values of the mixture index of fit.
	   	\describe{		
			\item{est}{for the supplied data and odds ratio.}
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
		a list of with a single item
	   	\describe{		
			\item{est}{numeric: the supplied value of cross-product ratio.}
		}
	}
}
\references{
	Clogg, C. C., Rudas, T., & Xi, L. (1995). A new index of structure for the 
	analysis of models for mobility tables and other cross-classifications. 
	\emph{Sociological Methodology}, 197-222.	

	Rudas. T., Zwick. R., (1997) Estimating the Importance of Differential Item
   	Functioning. \emph{Journal of Educational and Behavioral Statistics}, 
	Vol. 22, No. 1 (Spring, 1997), pp. 31-45
}
\author{
}
\note{
}


\seealso{}
\examples{
	H <- matrix((1:4)*1e1, byrow=TRUE, ncol=2)

	h <- pistar.2by2(H, alpha=1, jack=TRUE)

	h

	str(h)

	plot(h)

	summary(h)
}
\keyword{pistar}

