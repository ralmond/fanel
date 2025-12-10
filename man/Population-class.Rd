\name{Population-class}
\docType{class}
\alias{Population-class}
\alias{as_longform,Population-method}

\title{Class \code{"Population"}}
\description{

  A class that contains a collection of
  \code{\linkS4class{PopulationModel}} object.

}
\section{Constructor}{

  The constructor is \code{Population$new(name, popModels, groups=1L,
    tname="theta", wname="w")}.

  
  


}
\section{Slots}{
  \describe{
    \item{\code{.S3Class}:}{Object of class \code{"character"} ~~ }
  }
}
\section{Extends}{
Class \code{"\linkS4class{oldClass}"}, directly.
}
\section{Methods}{
  \describe{
    \item{as_longform}{\code{signature(x = "Population")}: ... }
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
showClass("Population")
}
\keyword{classes}
