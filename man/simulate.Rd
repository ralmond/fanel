\name{simulate.POMDP}
\alias{simulate.POMDP}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
A Capitalized Title (ideally limited to 65 characters)
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
simulate.POMDP(object, nsim, seed, ..., covars = NULL, workers = Workers$new())
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{object}{
%%     ~~Describe \code{object} here~~
}
  \item{nsim}{
%%     ~~Describe \code{nsim} here~~
}
  \item{seed}{
%%     ~~Describe \code{seed} here~~
}
  \item{\dots}{
%%     ~~Describe \code{\dots} here~~
}
  \item{covars}{
%%     ~~Describe \code{covars} here~~
}
  \item{workers}{
%%     ~~Describe \code{workers} here~~
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
function (object, nsim, seed, ..., covars = NULL, workers = Workers$new()) 
{
    if (missing(covars)) {
        stop("Covariates must be supplied.")
    }
    if (missing(nsim)) 
        nsim = nsubj(object)
    if (!missing(seed)) {
        workers$seed <- seed
    }
    workers$start()
    ldata <- as_longform(covars)
    result <- purrr::list_rbind(workers$lapply(split(ldata, ~subj), 
        function(cov, hmm) {
            isubj <- cov$subj[1]
            tnames <- tnames(hmm)
            theta <- as.data.frame(lapply(tnames, function(n) rep(NA, 
                nocc(cov))))
            names(theta) <- tnames
            dnames <- dnames(hmm)
            data <- as.data.frame(lapply(dnames, function(n) rep(NA, 
                nocc(cov))))
            names(data) <- dnames
            theta[1L, ] <- drawInitial(hmm, isubj, 1L, cov[1L, 
                ])
            for (iocc in 1L:maxocc(cov)) {
                theta[iocc + 1L, ] <- drawGrowth(hmm, isubj, 
                  iocc, theta[iocc, ], cov[iocc + 1L, ])
                data[iocc, ] <- drawData(hmm, isubj, iocc, theta[iocc, 
                  ], cov[iocc + 1L, ])
            }
            names(theta) <- paste0(tnames, "_sim")
            cbind(cov, theta, data)
        }, object))
    workers$stopFlag()
    long2panel(result, dnames = evidence(object)$dnames, invcols = covars$invarnames)
  }
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
