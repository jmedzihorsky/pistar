%%
%%
%%

\name{Fienberg1980a}
\alias{Fienberg1980a}

\docType{data}
\title{
	Citation Practices in Two Operations Research Journals
}
\description{
	Citation practices in two operations reserch journals from Fienberg 
	(1980a) analyzed by Rudas (2002) using the mixture index of fit.	

	Reprinted by permission of The Applied Probability Trust. First published 
	in Fienberg, S. E. (1980a) Using loglinear models to analyze 
	cross-classified categorical data, \emph{The Mathematical Scientist}, 5,
	13-30.  Copyright (c) Applied Probability Trust 1980. 
}
\usage{
data(Fienberg1980a)
}
\format{
	A 2-dimensional 2-by-4 array cross-tabulating observations of 336 papers
	in two operations research journals, \emph{Management Science} (MS) and 
	\emph{Operations Research} (OR), from their 1969 and 1970 issues.
	Each papers is classified into one of four categories based on whether
	it refers to other papers from these two journals.

	The variables and their levels are as follows:
	\tabular{rll}{
		No	\tab	Name	\tab	Levels				\cr
		1 	\tab	citing	\tab	MS, OR 				\cr
		2 	\tab	cited	\tab	none, MS, OR, both 	\cr
	}
}
\details{
}
\source{
	Fienberg, S. E. (1980a) Using loglinear models to analyze cross-classified 
	categorical data, \emph{The Mathematical Scientist}, 5, 13-30.
}
\references{
	Fienberg, S. E. (1980a) Using loglinear models to analyze cross-classified 
	categorical data, \emph{The Mathematical Scientist}, 5, 13-30.

	Rudas, T. (2002) 'A Latent Class Approach to Measuring the Fit of a 
	Statistical Model' in Hagenaars, J. A. and McCutcheon, A. L. (eds.) Applied
	Latent Class Analysis. Cambridge University Press. 345-365.
}
\examples{
	data(Fienberg1980a)
	Fienberg1980a
}
\keyword{datasets}
