\name{PistarBVN-class}
\Rdversion{1.1}
\docType{class}
\alias{PistarBVN-class}
\alias{PistarBVN}

\title{Class \code{"PistarBVN"}}
\description{
%%  ~~ A concise (1-5 lines) description of what the class is. ~~
}
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("PistarBVN", ...)}.
%%  ~~ describe objects here ~~ 
}
\section{Slots}{
  \describe{
    \item{\code{data}:}{Object of class \code{"data.frame"} ~~ }
    \item{\code{interval}:}{Object of class \code{"numeric"} ~~ }
    \item{\code{conf}:}{Object of class \code{"numeric"} ~~ }
    \item{\code{alt}:}{Object of class \code{"character"} ~~ }
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
    \item{plot}{\code{signature(x = "PistarBVN")}: ... }
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
showClass("PistarBVN")
}
\keyword{classes}
