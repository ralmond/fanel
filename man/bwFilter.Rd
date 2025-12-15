\name{bwFilter}
\alias{bwFilter}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
A Capitalized Title (ideally limited to 65 characters)
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
bwFilter(object, covars, quad, ..., workers = Workers$new())
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
function (object, covars, quad, ..., workers = Workers$new()) 
{
    workers$start()
    qlist <- workers$lapply(split_subj(covars), function(cov, 
        hmm, qua) {
        isubj <- cov$isubj
        qua <- get_subj(qua, isubj)
        maxocc(qua) <- maxocc(cov)
        qua$isubj <- isubj
        qua$resetWeights()
        thetas <- qua$theta(1L, 1L)
        lweights <- ProbInit(hmm, isubj, thetas, cov$getInvar(isubj))
        lweights <- lweights * exp(evalEvidence(hmm, isubj, 0L, 
            cov$getData(isubj, 0L), thetas, cov$getVar(isubj, 
                0L)))
        qua$lweights[1L, 1L, ] <- lweights
        for (iocc in 1L:maxocc(cov)) {
            lweights <- advanceWeights(hmm, isubj, iocc, lweights, 
                cov$getVar(isubj, iocc))
            lweights <- lweights * exp(evalEvidence(hmm, isubj, 
                iocc, cov$getData(isubj, iocc), thetas, cov$getVar(isubj, 
                  iocc)))
            qua$lweights[1L, iocc + 1L, ] <- lweights
        }
        iocc <- maxocc(cov)
        rweights <- rep(1, nquad(qua))
        rweights <- rweights * exp(evalEvidence(hmm, isubj, iocc, 
            cov$getData(isubj, iocc), thetas, cov$getVar(isubj, 
                iocc)))
        qua$rweights[1L, iocc + 1L, ] <- rweights
        for (iocc in maxocc(cov):1L) {
            rweights <- retreatWeights(hmm, isubj, iocc, rweights, 
                cov$getVar(isubj, iocc))
            rweights <- rweights * exp(evalEvidence(hmm, isubj, 
                iocc - 1L, cov$getData(isubj, iocc - 1L), thetas, 
                cov$getVar(isubj, iocc - 1L)))
            qua$rweights[1L, iocc, ] <- rweights
        }
        qua
    }, object, get_subj(quad, 1))
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
