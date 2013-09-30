%%
%%
%%

\name{pearson2pistar}
\alias{pearson2pistar}

\title{
	Pearson to \eqn{\pi^*}{pi*} Conversion
}
\description{
	\code{pearson2pistar} is used to convert Pearson product-moment correlation
   	coefficient into the \eqn{\pi^{*}}{pi*} mixture index of fit using the 
	relationship between them described by Rudas, Clogg, and Lindsay (1994).
}
\usage{
	pearson2pistar(coeff)
}
\arguments{
 	\item{coeff}{
		value of Pearson product-moment correlation coefficient to be converted.
	}
}
\details{
	The relationship between \eqn{\pi^{*}}{pi*} and Pearson \eqn{\rho}{r} is:
	\deqn{\pi^{*} = 1 - \sqrt{ \frac{1-|\rho|}{1+|\rho|} } }{ pi* = 1 - ((1-abs(r))/(1+abs(r)))^(1/2) }
}
\value{
	Value of \eqn{\pi^{*}}{pi*} corresponding to the supplied value of Pearson 
	product-moment correlation coefficient.
}
\references{ 
	Rudas, T., Clogg, C. C., Lindsay, B. G. (1994) A New Index of Fit Based on 
	Mixture Methods for the Analysis of Contingency Tables. \emph{Journal of 
	the Royal Statistical Society. Series B (Methodological)}, Vol. 56, No. 4, 
	623-639.
}
\author{
	Juraj Medzihorsky
}
\note{}

\seealso{
	\code{\link[stats:cor]{cor}}	
	\code{\link[pistar:pistar.bvn]{pistar.bvn}}
}
\examples{
    pearson2pistar(0.5)

    curve(pearson2pistar, from=-1,
          xlab=expression(rho), 
          ylab=expression(pi^symbol("*")))

}

\keyword{pistar}
\keyword{pearson}
\keyword{correlation}

