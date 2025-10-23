\name{as_longform}
\alias{as_longform}
\alias{as_longform,Panmat-method}
\alias{as_longform,Panel_Frame-method}
\alias{as_longform,Panel_Data-method}
\title{Converts panel data to a longform data frame.}
\description{

  This unwinds panel data (objects of class \code{\linkS4class{Panmat}},
  \code{\linkS4class{Panel_Frame}} and \code{\linkS4class{Panel_Data}})
  into a data frame in long form data frame (or tibble) with rows
  corresponding to subjects and occasions.
  
}
\usage{
as_longform(x, n = nsubj(x), maxocc = nocc(x), minocc = 1L,
            name = deparse(substitute(x)))
}
\arguments{
  \item{x}{The data object to be converted into long form data.}
  \item{n}{The number of subjects.}
  \item{maxocc}{The maximum occurance value.}
  \item{minocc}{The minimum occurance value, usually 1 or 0.}
  \item{name}{The name to give the column for a \code{Panmat}.}
}
\details{

  The panel data has indexes for both subject and occassion, this turns
  the data into a long form tibble with with two index columns,
  \dQuote{subj} and \dQuote{occ}.  These are suitable for use with
  \code{\link[dplyr]{left_join}} to build up long form data.  

}
\value{
  A \code{\link[tibble]{tibble}} with columns \dQuote{subj} and
  \dQuote{occ} giving the subject and occasion index.
}
\author{Russell Almond}
\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or standard data sets, see data().

## The function is currently defined as
new("nonstandardGenericFunction", .Data = function (x, n = nsubj(x), 
    maxocc = nocc(x), minocc = 1L, name = deparse(substitute(x))) 
{
    standardGeneric("as_longform")
  }, generic = structure("as_longform", package = "fanel"), package = "fanel", 
    group = list(), valueClass = character(0), signature = c("x", 
    "n", "maxocc", "minocc", "name"), default = NULL, skeleton = (function (x, 
        n = nsubj(x), maxocc = nocc(x), minocc = 1L, name = deparse(substitute(x))) 
    stop(gettextf("invalid call in method dispatch to '\%s' (no default method)", 
        "as_longform"), domain = NA))(x, n, maxocc, minocc, name))
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
