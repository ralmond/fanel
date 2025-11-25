avePart <- function (restab) {
  dplyr::group_by(restab,restab$occ,restab$subj) |>
    dplyr::summarize(time=min(.data$time),tasks=min(.data$tasks),
                     Y=min(.data$Y),
                     theta_bar=wtd.mean(.data$theta,.data$weights),
                     theta_sd=sqrt(wtd.var(.data$theta,.data$weights,
                                           normwt=TRUE)))
}

wtd.loess.part <- function (time,theta,weights,part)
  wtd.loess.noiter(time,theta,weights)$y[part==1]

part_loess <- function(tab) {
  nsubj <- max(tab$subj)
  mocc <- max(tab$occ)+1L
  smooths <- dplyr::group_by(tab,tab$subj) |>
    dplyr::group_map(~wtd.loess.part(.x$time,.x$theta,.x$weights,.x$particle))
  smooths <- matrix(unlist(smooths),nsubj,mocc,byrow=TRUE)
  smooths
}
