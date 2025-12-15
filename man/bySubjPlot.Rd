\name{bySubjPlot}
\alias{bySubjPlot}
\alias{byTimePlot}
\title{Plots of Panel Data}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
bySubjPlot(results, theta = "theta", weight = "w",
  error_bars = TRUE, theta0 = NULL, lowcol = "gray90",
  highcol = "gray10", aveshape = 4, avecol = "red",
  avesize = 2, trueshape = 8, truecol = "blue", truesize = 2)
byTimePlot(results, theta = "theta", weight = "w", error_bars = TRUE,
    theta0 = NULL, lowcol = "gray90", highcol = "gray10", avetype = 2,
    avecol = "red", linewidth = 1, truetype = 1, truecol = "blue")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{results}{
%%     ~~Describe \code{results} here~~
}
  \item{theta}{
%%     ~~Describe \code{theta} here~~
}
  \item{weight}{
%%     ~~Describe \code{weight} here~~
}
  \item{error_bars}{
%%     ~~Describe \code{error_bars} here~~
}
  \item{theta0}{
%%     ~~Describe \code{theta0} here~~
}
  \item{lowcol}{
%%     ~~Describe \code{lowcol} here~~
}
  \item{highcol}{
%%     ~~Describe \code{highcol} here~~
}
  \item{aveshape}{
%%     ~~Describe \code{aveshape} here~~
}
  \item{avecol}{
%%     ~~Describe \code{avecol} here~~
}
  \item{avesize}{
%%     ~~Describe \code{avesize} here~~
}
  \item{trueshape}{
%%     ~~Describe \code{trueshape} here~~
}
  \item{truecol}{
%%     ~~Describe \code{truecol} here~~
}
  \item{truesize}{
%%     ~~Describe \code{truesize} here~~
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
function (results, theta = "theta", weight = "w", error_bars = TRUE,
    theta0 = NULL, lowcol = "gray90", highcol = "gray10", aveshape = 4,
    avecol = "red", avesize = 2, trueshape = 8, truecol = "blue",
    truesize = 2)
{
    summar <- dplyr::summarize(results, theta_bar = wtd.mean({
        {
            theta
        }
    }, {
        {
            weight
        }
    }), theta_w = 2 * sqrt(wtd.var({
        {
            theta
        }
    }, {
        {
            weight
        }
    }, normwt = TRUE)))
    if (!missing(theta0))
        summar$theta0 <- theta0
    plot <- ggplot2::ggplot(results, ggplot2::aes(x = results$subj,
        y = results[[theta]], colour = results[[weight]], frame = results$occ)) +
        ggplot2::geom_point() + ggplot2::scale_colour_gradient(low = lowcol,
        high = highcol) + ggplot2::geom_point(data = summar,
        mapping = ggplot2::aes(x = summar$subj, y = summar$theta_bar,
            frame = summar$occ), shape = aveshape, col = avecol,
        size = avesize)
    if (error_bars) {
        plot <- plot + ggplot2::geom_linerange(data = summar,
            mapping = ggplot2::aes(x = summar$subj, ymin = summar$theta_bar -
                summar$theta_w, ymax = summar$theta_bar + summar$theta_w,
                frame = summar$occ), col = avecol, size = avesize)
    }
    if (!missing(theta0)) {
        plot <- plot + ggplot2::geom_point(data = summar, mapping = ggplot2::aes(x = summar$subj,
            y = summar$theta0, frame = summar$occ), shape = trueshape,
            col = truecol, size = truesize)
    }
    plot
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
