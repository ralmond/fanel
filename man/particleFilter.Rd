\name{particleFilter}
\alias{particleFilter}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
A Capitalized Title (ideally limited to 65 characters)
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
particleFilter(object, covars, quad = NULL, nquad = nquad(quad), ..., workers = Workers$new())
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{object}{
%%     ~~Describe \code{object} here~~
}
  \item{covars}{
%%     ~~Describe \code{covars} here~~
}
  \item{quad}{
%%     ~~Describe \code{quad} here~~
}
  \item{nquad}{
%%     ~~Describe \code{nquad} here~~
}
  \item{\dots}{
%%     ~~Describe \code{\dots} here~~
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
function (object, covars, quad = NULL, nquad = nquad(quad), ..., 
    workers = Workers$new()) 
{
    if (missing(quad)) {
        if (missing(nquad)) {
            stop("Either 'quad' or 'nquad' must be supplied.")
        }
        quad <- particleQuad(covars$time, nquad, population(object)$tnames)
    }
    workers$start()
    qlist <- workers$lapply(split_subj(covars), function(cov, 
        hmm, qua) {
        nq <- nquad(qua)
        isubj <- cov$isubj
        qua <- qua$clone()
        qua$isubj <- isubj
        qua$resetWeights()
        lweights <- rep(0, nq)
        thetas <- drawInitial(hmm, isubj, nq, cov$getInvar(isubj))
        lweights <- evalEvidence(hmm, isubj, 0L, thetas, cov$getVar(isubj, 
            0L))
        qua$theta(1L, 0L, , thetas)
        qua$lweight(1L, 0L, , lweights)
        for (iocc in 1L:hmm$maxocc) {
            thetas <- drawGrowth(hmm, isubj, iocc, thetas, cov$getVar(isubj, 
                iocc))
            lweights <- lweights + evalEvidence(hmm, isubj, iocc, 
                cov$getData(isubj, iocc), thetas, cov$getVar(isubj, 
                  iocc))
            qua$theta(1L, iocc, , thetas)
            qua$lweights(1L, iocc, , lweights)
        }
        qua
    }, object, quad)
    workers$stopFlag()
    bind_subj(qlist)
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
