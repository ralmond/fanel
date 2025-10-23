\name{time2occ}
\alias{time2occ}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
A Capitalized Title (ideally limited to 65 characters)
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
time2occ(data, idcol = "subj", timecol = "time", occcol = "occ", zerobase = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{
%%     ~~Describe \code{data} here~~
}
  \item{idcol}{
%%     ~~Describe \code{idcol} here~~
}
  \item{timecol}{
%%     ~~Describe \code{timecol} here~~
}
  \item{occcol}{
%%     ~~Describe \code{occcol} here~~
}
  \item{zerobase}{
%%     ~~Describe \code{zerobase} here~~
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
function (data, idcol = "subj", timecol = "time", occcol = "occ", 
    zerobase = FALSE) 
{
    data <- as.data.frame(data)
    data[[occcol]] <- NA_integer_
    subj <- data[[idcol]]
    if (is.null(subj)) {
        occ <- seq_len(data) - as.integer(zerobase)
        data[[occcol]] <- occ[order(data[[timecol]])]
    }
    else {
        for (isubj in unique(subj)) {
            xsubj <- isubj == subj
            occ <- 1L:sum(xsubj) - as.integer(zerobase)
            data[xsubj, occcol] <- occ[order(data[[xsubj, timecol]])]
        }
    }
    data
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
