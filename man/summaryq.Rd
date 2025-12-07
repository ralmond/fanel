\name{summaryq}
\alias{summaryq}
\alias{summaryqq}
\title{Summarizes quadrature data over quadrature points.}
\description{

  Quadrature data has indexes:  \dQuote{subj}, \dQuote{occ} and
  \dQuote{quad}. The \code{\linkS4class{Panel_Data}} and
  \code{\linkS4class{ModelSet}} classes only have the first two indexes.  To
  join them together, there is a need summarize across the quadrature
  points.  The \code{summaryq} function provides the mean and standard
  deviation (error) as summaries.  The \code{summaryqq} function
  provides quantiles.


}
\usage{
summaryq(quad, sumfuns = list(m = ~wtd.mean(.data[[.x]], .data[[wname]]),
                              sd = ~sqrt(wtd.var(.data[[.x]], .data[[wname]],
                                         normwt = TRUE))),
         thetas = quad$tnames, wname = quad$wname[1])
summaryqq(quad, probs = c(.025, 0.5, 0.975), thetas=quad$tnames,
          wname = quad$wname[1])
}
\arguments{
  \item{quad}{An object of type \code{\linkS4class{Quadrature}} to be
    summarized.}
  \item{sumfuns}{A list of summaries to pass to
    \code{\link[dplyr]{across}} to generate summaries, see details.}
  \item{probs}{A vector of probalities whose quantiles will be reported.}
  \item{thetas}{A character vector giving the names of the latent variables.}
  \item{wname}{A character scalar giving the name of the weight variable.}
}
\details{

  The reduction over the quadrature can be done by a combination of
  using the \code{\link[dplyr:dplyr-package]{dplyr}} and the weighted
  statistics borrowed from the \code{Hmisc} package
  (\code{\link{wtd.stats}}). The general logic is:

  \code{
    as_longform(quad) |>
    dplyr::group_by(subj, occ)
    dplyr::summarize(time=dplyr::first(time),...)
  }
  The difference between the three variants, \code{summaryq},
  \code{summaryqq} and \code{summarylowess} is what goes in the dots.

  The next catch is that there may be more than one latent variable, so
  \code{\link[dplyr]{across}(thetas,sumfuns)} is used to generate the
  summaries.  This should be a named list of unevaluated expressions,
  the names are uses as suffixes for the final colums, which have the
  format \code{\var{theta}_\var{fun}}.  In the lambda expression use
  \code{.data[[.x]]} to refer to the latent variable column, and
  \code{.data[[wname]]} to refer to the weight column.

  The function \code{summaryqq} uses the \code{\link{wtd.quantile}}
  function instead of the mean and sd.  It fills in the dots with:
  \code{across(thetas, q=~wtd.quantiles(.data[[.x]], .data[[wname]],
    probs=probs))}.

}
\value{
  A tibble or data frame with columns (depending on function):
  \item{\dQuote{subj}}{Subject index}
  \item{\dQuote{occ}}{Occasion index}
  \item{\dQuote{time}}{Time corresponding to subject/occasion}
  \item{\dQuote{theta[i]_m}}{Mean of ith theta variable (\code{summaryq})}
  \item{\dQuote{theta[i]_sd}}{SD of ith theta variable (\code{summaryq})}
  \item{\dQuote{theta[i]_fn}}{User supplied function of ith theta
      variable (\code{summaryq})}
  \item{\dQuote{theta[i]_q_[prob]}}{Quantile of ith theta variable
      (\code{summaryqq})}
}
\author{Russell Almond}
\seealso{
  \code{\link{as_longform}}, \code{\link{wtd.stats}},
  \code{\linkS4class{Quadrature}}
}
\examples{
}
\keyword{ univar }
\keyword{ ts }
\concept{ panel_data }
\concept{ quadrature }

