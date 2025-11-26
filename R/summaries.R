summaryq <- function(quad,
                     sumfuns=list(m=~wtd.mean(.data[[.x]],
                                              .data[[{{wname}}]]),
                                  sd=~sqrt(wtd.var(.data[[.x]],
                                                   .data[[{{wname}}]],
                                                   normwt=TRUE))),
                  thetas=quad$tnames,
                  wname=quad$wname[1]) {
  as_longform(quad) |>
    dplyr::group_by(subj,occ) |>
    dplyr::summarize(dplyr::across(thetas,sumfuns),time=first(time))
}

summaryqq <- function(quad,probs=c(.025,.5,.975),
                  thetas=quad$tnames,
                  wname=quad$wname[1]) {
  as_longform(quad) |>
    dplyr::group_by(subj,occ) |>
    dplyr::summarize(dplyr::across(thetas,
                            q=~wtd.quantiles(.data[[.x]],
                                           .data[[{{wname}}]],
                                           probs={{probs}}, normwt=TRUE)),
                     time=first(time))
}

summarylowess <- function(quad,probs=c(.025,.5,.975),
                          thetas=quad$tnames,
                          wname=quad$wname[1]) {
  as_longform(quad) |>
    dplyr::group_by(subj,occ) |>
    dplyr::summarize(time=first(time),
                     dplyr::across(thetas,
                            smooth=~wtd.lowess.noiter(.data$time,
                                               .data[[.x]],
                                               .data[[{{wname}}]])$y))

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
