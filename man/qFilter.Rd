\name{qFilter}
\alias{qFilter}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
A Capitalized Title (ideally limited to 65 characters)
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
qFilter(object, covars, quad, wfun, ..., workers = Workers$new())
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
  \item{wfun}{
%%     ~~Describe \code{wfun} here~~
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
function (object, covars, quad, wfun, ..., workers = Workers$new()) 
{
    workers$start()
    qlist <- workers$lapply(split_subj(covars), function(cov, 
        hmm, qua, wf) {
        isubj <- cov$isubj
        qua <- get_subj(qua, isubj)
        maxocc(qua) <- maxocc(cov)
        qua$isubj <- isubj
        qua$resetWeights()
        thetas <- qua$theta(1L, 1L)
        tstar <- qua$times[1L, ]
        nt <- length(tstar)
        nq <- nquad(qua)
        iprobs <- ProbInit(hmm, isubj, thetas, cov$getInvar(isubj))
        qua$lweights[isubj, , ] <- outer(log(iprobs), wf(cov$dt[isubj, 
            0L], tstar), "*")
        for (iocc in 0L:maxocc(cov)) {
            qua$lweights[isubj, , ] <- qua$lweights[isubj, , 
                ] + outer(evalEvidence(hmm, isubj, iocc, cov$getData(isubj, 
                iocc), thetas, cov$getVar(isubj, iocc)), wf(cov$dt[isubj, 
                iocc], tstar), "*")
        }
        qua
    }, object, get_subj(quad, 1), wfun)
    workers$stopFlag()
    bind_subj(qlist)
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
