\name{methodTimeGadget}
\alias{methodTimeGadget}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
A Capitalized Title (ideally limited to 65 characters)
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
methodTimeGadget(results, summary, methods)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{results}{
%%     ~~Describe \code{results} here~~
}
  \item{summary}{
%%     ~~Describe \code{summary} here~~
}
  \item{methods}{
%%     ~~Describe \code{methods} here~~
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
function (results, summary, methods) 
{
    subjlist <- unique(results$subj)
    ui <- shiny::fillPage(shiny::inputPanel(shiny::selectInput("method", 
        "Method: ", methods, selected = methods[1], multiple = FALSE), 
        shiny::selectInput("subj", "Subject:", subjlist, subjlist[1]), 
        shiny::selectInput("traces", "Selected Traces: ", methods, 
            selected = methods[1], multiple = TRUE, selectize = FALSE)), 
        shiny::plotOutput("plot"))
    server <- function(input, output, session) {
        output$plot <- shiny::renderPlot({
            results <- dplyr::filter(results, results$method == 
                input$method, results$subj == input$subj)
            summary <- dplyr::filter(summary, summary$method \%in\% 
                input$traces, summary$subj == input$subj)
            ggplot2::ggplot(results, ggplot2::aes(x = results$time, 
                y = results$theta, colour = results$weights)) + 
                ggplot2::geom_point() + ggplot2::scale_colour_gradient(low = "gray90", 
                high = "gray10") + ggplot2::geom_line(data = summary, 
                mapping = ggplot2::aes(x = summary$time, y = summary$theta_bar, 
                  linetype = summary$method), color = "blue") + 
                ggplot2::geom_line(data = summary, mapping = ggplot2::aes(x = summary$time, 
                  y = summary$theta0), color = "red") + ggplot2::guides(colour = "none")
        })
    }
    shiny::shinyApp(ui, server)
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
