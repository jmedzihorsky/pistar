%%
%%
%%

\name{PistarCT-class}
\Rdversion{1.1}
\docType{class}

\alias{PistarCT}
\alias{PistarCT-class}
\alias{plot,PistarCT-method}

\title{Class \code{"PistarCT"}}
\description{
%%  ~~ A concise (1-5 lines) description of what the class is. ~~
}
\usage{
\S4method{plot}{PistarCT}(x, y, ...)
}
\arguments{
	\item{x}{object of class \code{PistarCT}}
	\item{y}{ignored}
	\item{...}{optional}
}
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("PistarCT", ...)} or \code{PistarCT(...)}.
%%  ~~ describe objects here ~~ 
}
\section{Slots}{
  \describe{
    \item{\code{data}:}{Object of class \code{"array"} ~~ }
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
    \item{plot}{\code{signature(x = "PistarCT")}: ... }
	 }
}
\references{
%%  ~~put references to the literature/web site here~~
}
\author{
	Juraj Medzihorsky
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
showClass("PistarCT")
}
\keyword{classes}
