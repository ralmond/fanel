\name{biasDisplayGadget}
\alias{biasDisplayGadget}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
A Capitalized Title (ideally limited to 65 characters)
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
biasDisplayGadget(biastab)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{biastab}{
%%     ~~Describe \code{biastab} here~~
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
function (biastab) 
{
    subjlist <- unique(biastab$subj)
    ui <- shiny::fillPage(shiny::inputPanel(shiny::selectInput("subj", 
        "Subject:", 1:10, multiple = TRUE, selectize = FALSE)), 
        shiny::plotOutput("plot"), shiny::tableOutput("table"))
    server <- function(input, output, session) {
        output$plot <- shiny::renderPlot(ggplot2::ggplot(biastab, 
            ggplot2::aes(y = biastab$scaledBias, x = biastab$method, 
                colour = biastab$subj)) + ggplot2::scale_y_continuous(limits = c(-3, 
            3)) + ggplot2::geom_point() + ggplot2::geom_line(data = dplyr::filter(biastab, 
            biastab$subj \%in\% input$subjcc), mapping = ggplot2::aes(x = as.numeric(.data$method), 
            y = .data$scaledBias, colour = .data$subj)) + ggplot2::geom_hline(ggplot2::aes(yintercept = 0)))
        output$table <- shiny::renderTable(round(dplyr::summarize(dplyr::group_by(biastab, 
            .data$method), bias = mean(.data$bias), mse = mean(.data$bias^2), 
            t = mean(.data$scaledBias), t2 = sum(.data$scaledBias^2)), 
            3))
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
