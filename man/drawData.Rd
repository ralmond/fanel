\name{drawData}
\alias{drawData}
\alias{drawInitial}
\alias{drawGrowth}
\alias{evalEvidence}
\alias{advanceWeights}
\alias{retreatWeights}
\alias{probInit}
\title{Generic functions for filters}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
drawInitial(model, isubj, npart, covar = NULL)
probInit(model, isubj, npart, covar = NULL)
drawData(model, isubj, iocc, theta, covar = NULL)
evalEvidence(model, isubj, iocc, theta, data, covar = NULL)
drawGrowth(model, isubj, iocc, theta, covar = NULL)
advanceWeights(model, isubj, iocc, lweights, covar = NULL)
retreatWeights(model, isubj, iocc, rweights, covar = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{model}{
%%     ~~Describe \code{model} here~~
}
  \item{isubj}{
%%     ~~Describe \code{isubj} here~~
}
  \item{iocc}{
%%     ~~Describe \code{iocc} here~~
}
  \item{theta}{
%%     ~~Describe \code{theta} here~~
}
  \item{covar}{
%%     ~~Describe \code{covar} here~~
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or standard data sets, see data().

## The function is currently defined as
new("standardGeneric", .Data = function (model, isubj, iocc,
    theta, covar = NULL)
standardGeneric("drawData"), generic = structure("drawData", package = "fanel"),
    package = "fanel", group = list(), valueClass = character(0),
    signature = c("model", "isubj", "iocc", "theta", "covar"),
    default = new("derivedDefaultMethod", .Data = function (model,
        isubj, iocc, theta, covar = NULL)
    {
        UseMethod("drawData")
    }, target = new("signature", .Data = "ANY", names = "model",
        package = "methods"), defined = new("signature", .Data = "ANY",
        names = "model", package = "methods"), generic = structure("drawData", package = "fanel")),
    skeleton = (new("derivedDefaultMethod", .Data = function (model,
        isubj, iocc, theta, covar = NULL)
    {
        UseMethod("drawData")
    }, target = new("signature", .Data = "ANY", names = "model",
        package = "methods"), defined = new("signature", .Data = "ANY",
        names = "model", package = "methods"), generic = structure("drawData", package = "fanel")))(model,
        isubj, iocc, theta, covar))
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory (show via RShowDoc("KEYWORDS")):
% \keyword{ ~kwd1 }
% \keyword{ ~kwd2 }
% Use only one keyword per line.
% For non-standard keywords, use \concept instead of \keyword:
% \concept{ ~cpt1 }
% \concept{ ~cpt2 }
% Use only one concept per line.
