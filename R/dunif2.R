#' @title Populationsantagande, två likformiga fördelningar
#' @description Två likformiga fördelningar ihopsatta till en, ena delen barn (under 18 år) och andra delen vuxna (18 år eller över). Ger täthetsfunktionen. Se Figur 2.
#' @param x age.
#' @param min Lägsta ålder för barn, måste vara 18 år eller under. Förvalt är 15 år.
#' @param max Högsta ålder, måste vara 18 år eller över. Förvalt är 21 år.
#' @param prevalence Prevalens, andelen som är vuxna (18 år eller över). Förvalt är 0.84
#'
#' @export
dunif2 <- function(x, min = 15, max = 21, prevalence = 0.84) {

  # Kurvhöjd för barn
  h.child <- (1 - prevalence) * stats::dunif(mean(c(min, 18)), min, 18)

  # Kurvhöjd för vuxna
  h.adult <- prevalence * stats::dunif(mean(c(18, max)), 18, max)

  res <- numeric(length(x))
  res[x < 18 & x >= min] <- h.child
  res[x >= 18 & x <= max] <- h.adult
  res

}
