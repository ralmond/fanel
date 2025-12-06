## importFrom(Hmisc,wtd.Ecdf,wtd.loess.noiter,
##            wtd.quantile,wtd.rank,wtd.table,
##            wtd.var,wtd.mean)
## We are borrowing these from Hmisc, but resetting them to fix the defaults.
wtd.mean <- function (x, weights = NULL, normwt = "ignored", na.rm = TRUE) {
  Hmisc::wtd.mean(x,weights,normwt,na.rm)
}
wtd.var <- function(x, weights = NULL, normwt = TRUE,
                    na.rm = TRUE, method = c("unbiased", "ML")) {
  Hmisc::wtd.var(x,weights,normwt,na.rm,method)
}
wtd.sd <- function(x, weights = NULL, normwt = TRUE,
                    na.rm = TRUE, method = c("unbiased", "ML")) {
  sqrt(Hmisc::wtd.var(x,weights,normwt,na.rm,method))
}
wtd.quantile <- function(x, weights = NULL, probs = c(0, 0.25, 0.5, 0.75, 1),
                         type = c("quantile", "(i-1)/(n-1)", "i/(n+1)", "i/n"),
                         normwt = TRUE, na.rm = TRUE) {
  data.frame(
      val=Hmisc::wtd.quantile(x,weights,probs,type,normwt,na.rm),
      prob=probs
  )
}
wtd.Ecdf <- function(x, weights = NULL, type = c("i/n", "(i-1)/(n-1)",
                                                 "i/(n+1)"),
                     normwt = TRUE, na.rm = TRUE) {
  Hmisc::wtd.Ecdf(x, weights, type, normwt, na.rm)
}
wtd.rank <- function(x, weights = NULL, normwt = TRUE, na.rm = TRUE) {
  Hmisc::wtd.rank(x, weights, normwt, na.rm)
}
wtd.table <- function(x, weights = NULL, type = c("list", "table"),
                      normwt = TRUE, na.rm = TRUE) {
  Hmisc::wtd.table(x,weights,type,normwt,na.rm)
}
wtd.loess.noiter <- function(x, y, weights = rep(1, length(x)), span = 2/3,
                             degree = 1, cell = 0.13333,
                             type = c("all", "ordered all", "evaluate"),
                             evaluation = 100, na.rm = TRUE) {
  Hmisc::wtd.loess.noiter(x,y,weights,span,degree,cell,type,
                           evaluation,na.rm)
}








summaryq <- function(quad,
                     sumfuns=list(m=~wtd.mean(.data[[.x]],
                                              .data[[wname]]),
                                  sd=~sqrt(wtd.var(.data[[.x]],
                                                   .data[[wname]],
                                                   normwt=TRUE))),
                  thetas=quad$tnames,
                  wname=quad$wname[1]) {
  as_longform(quad) |>
    dplyr::group_by(.data$subj,.data$occ) |>
    dplyr::summarize(time=dplyr::first(.data$time),
                     dplyr::across(thetas,sumfuns))
}

summaryqq <- function(quad,probs=c(.025,.5,.975),
                  thetas=quad$tnames,
                  wname=quad$wname[1]) {
  pnames <- paste0(thetas,"_p")
  as_longform(quad) |>
    dplyr::group_by(.data$subj,.data$occ) |>
    dplyr::reframe(time=dplyr::first(.data$time),
                   dplyr::across(thetas,
                                 q=~wtd.quantiles(.data[[.x]],
                                                  .data[[wname]],
                                                  probs=probs))) |>
    dplyr::select(!tidyselect::any_of(pnames[-1])) |>
    tidyr::pivot_wider(names_from=tidyselect::any_of(pnames[1]),
                       values_from=tidyselect::ends_with("_q"))
}



## avePart <- function (restab) {
##   dplyr::group_by(restab,restab$occ,restab$subj) |>
##     dplyr::summarize(time=min(.data$time),tasks=min(.data$tasks),
##                      Y=min(.data$Y),
##                      theta_bar=wtd.mean(.data$theta,.data$weights),
##                      theta_sd=sqrt(wtd.var(.data$theta,.data$weights,
##                                            normwt=TRUE)))
## }

## wtd.loess.part <- function (time,theta,weights,part)
##   wtd.loess.noiter(time,theta,weights)$y[part==1]

## part_loess <- function(tab) {
##   nsubj <- max(tab$subj)
##   mocc <- max(tab$occ)+1L
##   smooths <- dplyr::group_by(tab,tab$subj) |>
##     dplyr::group_map(~wtd.loess.part(.x$time,.x$theta,.x$weights,.x$particle))
##   smooths <- matrix(unlist(smooths),nsubj,mocc,byrow=TRUE)
##   smooths
## }
