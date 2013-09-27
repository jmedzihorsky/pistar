%%
%%
%%

\name{pistar}
\alias{pistar}

\title{
	The Mixture Index of Fit
}
\description{
	\code{pistar} is a wrapper function for all functions estimating the
	Rudas-Clogg-Lindsay \eqn{\pi^{*}}{pi*} mixture index of fit from 
	package \pkg{pistar}.
}
\usage{
pistar(proc, ...)
}
\arguments{
	\item{proc}{
		\describe{
		\item{\code{"uv"}}{
			for \code{\link[pistar:pistar.uv]{pistar.uv}}
		}
		\item{\code{"ct"}}{
			for \code{\link[pistar:pistar.ct]{pistar.ct}}
		}
		\item{\code{"ll"}}{
			for \code{\link[pistar:pistar.ll]{pistar.ll}}
		}
		\item{\code{"2by2"}}{
			for \code{\link[pistar:pistar.2by2]{pistar.2by2}}
		}
		\item{\code{"mvn"}}{
			for \code{\link[pistar:pistar.mvn]{pistar.mvn}}
		}		
		\item{\code{"bvn"}}{
			for \code{\link[pistar:pistar.bvn]{pistar.bvn}}
		}
		}
	}
	\item{\dots}{
		arguments passed to the function.
	}
}
\details{
}
\value{
	An object of \code{class} \code{"Pistar"}.
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
	# 	create data in a 2-by-2 table
	H <- matrix(rpois(4, 20), ncol=2)

	#	find pi* for independence (i.e. odds ratio of 1)
	h <- pistar(proc='2by2', data=H, alpha=1, jack=FALSE)

	h
}
\keyword{pistar}
\keyword{index of fit}


