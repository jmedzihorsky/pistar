\name{PistarMVN-class}
\Rdversion{1.1}
\docType{class}


\alias{PistarMVN-class}
\alias{PistarMVN}
\alias{plot,PistarMVN-method}

\title{Class \code{"PistarMVN"}}
\description{
%%  ~~ A concise (1-5 lines) description of what the class is. ~~
}
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("PistarMVN", ...)} or \code{PistarMVN()}.
%%  ~~ describe objects here ~~ 
}
\usage{
\S4method{plot}{PistarMVN}(x, lty = 1, lwd = 1, col = 'black', ... )
}
\arguments{
	\item{x}{
		object of \code{class} \code{"PistarMVN"}.
	}
 	\item{lty}{
		line type.
	}
 	\item{lwd}{
		line width.
	}
 	\item{col}{
		color of the line.
	}
 	\item{\dots}{
		optional arguments passed further.
	}
}
\section{Slots}{
  \describe{
    \item{\code{data}:}{Object of class \code{"data.frame"} ~~ }
    \item{\code{trace}:}{Object of class \code{"list"} ~~ }
    \item{\code{iter}:}{Object of class \code{"list"} ~~ }
    \item{\code{test}:}{Object of class \code{"list"} ~~ }
    \item{\code{call}:}{Object of class \code{"language"} ~~ }
    \item{\code{pistar}:}{Object of class \code{"list"} ~~ }
    \item{\code{pred}:}{Object of class \code{"list"} ~~ }
    \item{\code{param}:}{Object of class \code{"list"} ~~ }
  }
}
\section{Extends}{
Class \code{"\linkS4class{Pistar}"}, directly.
}
\section{Methods}{
  \describe{
    \item{plot}{\code{signature(x = "PistarMVN")}: ... }
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
showClass("PistarMVN")
}
\keyword{classes}
