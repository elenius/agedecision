#' @title Linjär nyttomodell då utfallet är mogen/vuxen
#' @description  Se ekvation 5.
#' @param x Ålder.
#' @param zero.limit Den högsta ålder som ger den lägsta nyttan 0. Förvalt är 15 år.
#' @export
utility_linear_adult <- function(x, zero.limit = 15) {
  lowest.utility <- 0
  slope <- (1 - lowest.utility) / (18 - zero.limit)
  intercept <- -1 * slope * zero.limit + lowest.utility
  res <- intercept  + slope * x
  res[x >= 18] <- 1
  res[x <= zero.limit] <- lowest.utility
  res
}

#' @title Linjär nyttomodell då utfallet är omogen/barn
#' @description  Se ekvation 5.
#' @param x Ålder.
#' @param zero.limit Den lägsta ålder som ger den lägsta nyttan. Förvalt är 21 år.
#' @param lowest.utility Den lägsta nyttan som inträffar vid zero.limit eller högre ålder. Betecknas i uppsatsen U_21O.
#' @export
utility_linear_child <- function(x, zero.limit = 21, lowest.utility = 0) {
  slope <-  (1 - lowest.utility) / (18 - zero.limit)
  intercept <- -1 * slope * zero.limit + lowest.utility
  res <- intercept + slope * x
  res[x <= 18] <- 1
  res[x >= zero.limit] <- lowest.utility
  res
}

#' @title Diskret nyttomodell då utfallet är omogen/barn $u_D^O(x)$
#' @description  Se ekvation 4.
#' @param x Ålder.
#' @param lowest.utility Den lägsta nyttan då em vuxen klassificeras som ett barn. Förvalt är 0.
#' @export
utility_discrete_child <- function(x, lowest.utility = 0) {
  res <- numeric(length(x))
  res[x >= 18] <- lowest.utility
  res[x < 18] <- 1
  res
}

#' @title Diskret nyttomodell då utfallet är mogen/vuxen
#' @description  Se ekvation 4.
#' @param x Ålder.
#' @param lowest.utility Den lägsta nyttan då ett barn klassificeras som ett barn. Förvalt är 0.
#' @export
utility_discrete_adult <- function(x) {
  res <- numeric(length(x))
  res[x >= 18] <- 1
  res
}
