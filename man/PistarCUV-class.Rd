\name{PistarCUV-class}
\Rdversion{1.1}
\docType{class}

\alias{PistarCUV-class}
\alias{PistarCUV}
\alias{plot,PistarCUV-method}

\title{Class \code{"PistarCUV"}}
\description{
%%  ~~ A concise (1-5 lines) description of what the class is. ~~
}
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("PistarCUV", ...)} or \code{PistarCUV(...)}.
%%  ~~ describe objects here ~~ 
}
\usage{
\S4method{plot}{PistarCUV}(x, model_col = "blue", unres_col = "grey", combi_col = "black", 
lty = 1, lwd = 1, pos = "topright", bty = "n", ... )
}
\arguments{
	\item{x}{
		object of class \code{"PistarCUV"}.
	}
	\item{model_col}{
		color for the model component. 
	}
  	\item{unres_col}{
		color for the unrestricted component. 
	}
  	\item{combi_col}{
		color for the predicted values from the two-point mixture. 
	}
 	\item{lty}{
		line type.
	}
 	\item{lwd}{
		line width.
	}
 	\item{pos}{
		position of the legend, see \code{legend}.
	}
 	\item{bty}{
		box type of the legend, see \code{legend}.
	}
 	\item{\dots}{
		optional arguments passed further to \code{plot}.
	}
}
\section{Slots}{
  \describe{
    \item{\code{data}:}{Object of class \code{"numeric"} ~~ }
    \item{\code{meth}:}{Object of class \code{"character"} ~~ }
    \item{\code{conv}:}{Object of class \code{"list"} ~~ }
    \item{\code{mess}:}{Object of class \code{"list"} ~~ }
    \item{\code{call}:}{Object of class \code{"language"} ~~ }
    \item{\code{pistar}:}{Object of class \code{"list"} ~~ }
    \item{\code{pred}:}{Object of class \code{"list"} ~~ }
    \item{\code{param}:}{Object of class \code{"list"} ~~ }
  }
}
\section{Extends}{
Class \code{"\linkS4class{PistarUV}"}, directly.
Class \code{"\linkS4class{Pistar}"}, by class "PistarUV", distance 2.
}
\section{Methods}{
  \describe{
    \item{plot}{\code{signature(x = "PistarCUV")}: ... }
	 }
}
\references{
%%  ~~put references to the literature/web site here~~
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%%  ~~objects to See Also as \code{\link{~~fun~~}}, ~~~
%%  ~~or \code{\linkS4class{CLASSNAME}} for links to other classes ~~~
}
\examples{
showClass("PistarCUV")
}
\keyword{classes}
