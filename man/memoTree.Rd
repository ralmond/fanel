\name{memoTree}
\alias{memoTree}
\docType{data}
\title{
A Capitalized Title for the Data Set
}
\description{
%%  ~~ A concise (1-5 lines) description of the dataset. ~~
}
\usage{data("memoTree")}
\format{
  The format is:
Class 'R6ClassGenerator' <memoTree> object generator
  Public:
    depth: 1
    initialize: function (depth = 1L) 
    exists: function (key) 
    get: function (key) 
    assign: function (key, value) 
    clear: function () 
    clone: function (deep = FALSE) 
  Private:
    cache: NULL
  Parent env: <environment: namespace:fanel>
  Locked objects: TRUE
  Locked class: FALSE
  Portable: TRUE 
 - attr(*, "name")= chr "memoTree_generator"
}
\details{
%%  ~~ If necessary, more details than the __description__ above ~~
}
\source{
%%  ~~ reference to a publication or URL from which the data were obtained ~~
}
\references{
%%  ~~ possibly secondary sources and usages ~~
}
\examples{
data(memoTree)
## maybe str(memoTree) ; plot(memoTree) ...
}
\keyword{datasets}
