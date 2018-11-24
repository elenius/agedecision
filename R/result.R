#' @title Förväntad nytta
#' @description Förväntad nytta (Expected Utility), se ekvation (16) och (17).
#' @param pM Sannolikhetsfunktion.
#' @param prevalence Prevalens, andel vuxna.
#' @param lowest.utility Nyttan för en felklassificerad vuxen.
#' @param age.min Lägsta ålder i åldersfördelningen, se ekvation (1).
#' @param age.max Högsta ålder i åldersfördelningen, se ekvation (1).
#' @param zero.limit.child Anger ålder då lägsta nytta för en felaktig klassificering av ett barn, se ekvation (3) och (4).
#' @param zero.limit.adult Anger ålder då lägsta nytta för en felklassificering av en vuxen, se ekvation (3) och (4).
#'
#' @export
eu_i <- function(
  pM,
  prevalence = 0.84,
  lowest.utility = 0,
  age.min = 15,
  age.max = 21,
  zero.limit.child = 15,
  zero.limit.adult = 21
) {
  # täthetsfunktion
  f_pi <- function(x) dunif2(x, prevalence = prevalence, min = age.min, max = age.max)

  # Diskret nyttomodell
  # Barn bedöms som barn (alltid nyttan 1)
  uBO_d <- function(x) (1 - pM(x)) * f_pi(x)
  # Vuxna bedöms som vuxna (altid nyttan 1)
  uVM_d <- function(x) pM(x) * f_pi(x)
  # Vuxna bedöms som barn (nyttan är u_VO)
  uVO_d <- function(x) (1-pM(x)) * lowest.utility * f_pi(x)

  # Förväntad nytta diskret nyttomodell
  ED_uBO_d <- integrate(uBO_d, lower = age.min, upper = 18)$value
  ED_uVM_d <- integrate(uVM_d, lower = 18, upper = age.max)$value
  ED_uVO_d <- integrate(uVO_d, lower = 18, upper = age.max)$value
  ED <- ED_uBO_d + ED_uVM_d + ED_uVO_d

  # Linjär nyttomodell (samma som diskret vid korrekta klassificeringar)
  # Barn bedöms som vuxna
  uBM_l <- function(x) {
    pM(x) *
      utility_linear_adult(x, zero.limit = zero.limit.child) *
      f_pi(x)
  }
  # Vuxna bedöms som barn
  uVO_l <- function(x) {
    (1 -pM(x)) *
      utility_linear_child(x, zero.limit = zero.limit.adult, lowest.utility = lowest.utility) *
      f_pi(x)
  }
  # Förväntad nytta linjär nyttomodell
  EL_uBM_l <- integrate(uBM_l, lower = age.min, upper = 18)$value
  EL_uVO_l <- integrate(uVO_l, lower = 18, upper = age.max)$value
  EL <- ED_uBO_d + ED_uVM_d + EL_uBM_l + EL_uVO_l

  # Förväntad nytta för respektive nyttomodeller
  c(ED = ED, EL = EL)

}

#' @title Förväntad nytta för alla alternativ/metoder
#' @description Förväntad nytta (Expected Utility) för alla metoder, se ekvation (16) och (17).
#' @param prevalence Prevalens, andel vuxna.
#' @param lowest.utility Nyttan för en felklassificerad vuxen.
#' @param age.min Lägsta ålder i åldersfördelningen, se ekvation (1).
#' @param age.max Högsta ålder i åldersfördelningen, se ekvation (1).
#' @param zero.limit.child Anger ålder då lägsta nytta för en felaktig klassificering av ett barn, se ekvation (3) och (4).
#' @param zero.limit.adult Anger ålder då lägsta nytta för en felklassificering av en vuxen, se ekvation (3) och (4).
#'
#' @export
eu_all <- function(
  prevalence = 0.84,
  lowest.utility = 0,
  age.min = 15,
  age.max = 21,
  zero.limit.child = 15,
  zero.limit.adult = 21
) {

  # Förväntad nytta givet inparametrar
  # endast sannolikhetsfunktion behövs som invärde
  eu_ix <- function(pM) {
    eu_i(
      pM,
      prevalence = prevalence,
      lowest.utility = lowest.utility,
      age.min = age.min,
      age.max = age.max,
      zero.limit.child = zero.limit.child,
      zero.limit.adult = zero.limit.adult
    )
  }
  c(
    `E_Hand-18` = eu_ix(pM = pM_hand_18),
    `E_Hand-19` = eu_ix(pM = pM_hand_19),
    `E_Hand-20` = eu_ix(pM = pM_hand_20),
    E_Tand = eu_ix(pM = pM_tand),
    E_Kna = eu_ix(pM = pM_kna),
    E_RMV = eu_ix(pM = pM_RMV),
    `E_Singla slant` = eu_ix(pM = pM_coinflip),
    E_Barn = eu_ix(pM = pM_barn),
    E_Vuxen = eu_ix(pM = pM_vuxen)
  )
}

#' @title Förväntad nytta för alla alternativ/metoder för alla kombinationer.
#' @description Förväntad nytta (Expected Utility) för alla metoder och kombinationer av prevalens och nytta för en felklassificerad vuxen, se ekvation (16) och (17).
#' @param by Minsta steg mellan 0 och 1.
#'
#' @export
eu_all_comb <- function(by = 0.1) {
  prevalence <- seq(0, 1, by = by)
  lowest.utility <- seq(0, 1, by = by)

  # method.umod <- names(eu_all(lowest.utility = 0, prevalence = 0.5))

  res <-
    lapply(prevalence, function(prev) {
      res <- lapply(lowest.utility, function(util) {
        cbind(eu_all(lowest.utility = util, prevalence = prev), prevalence = prev, uvo = util)
      })
      do.call(rbind, res)
    })
  res <- do.call(rbind, res)
  rnames <- rownames(res)
  res <- as.data.frame(res)
  res$method <- rnames
  names(res) <- c("E", names(res)[-1])
  row.names(res) <- NULL
  res$nyttomodell <- ifelse(grepl("ED$", res$method), "diskret", "linjär")
  res$method <- substr(res$method, 3, nchar(res$method) - 3)
  res

}

#' @title Specificitet och sensitivitet.
#' @description Specificitet och sensitivitet, se ekvation (15).
#' @param pM Sannolikhetsfunktion.
#' @param age.min Lägsta ålder i åldersfördelningen, se ekvation (1).
#' @param age.max Högsta ålder i åldersfördelningen, se ekvation (1).
#' @export
spec_sens <- function(
  pM,
  age.min = 15,
  age.max = 21
) {
if(age.min >= 18 | age.max <= 18) stop("age.min måste vara lägre än 18 och age.max större än 18.")

  f_pi_0 <- function(x) dunif2(x, min = age.min, max = age.max, prevalence = 0)
  f_pi_1 <- function(x) dunif2(x, min = age.min, max = age.max, prevalence = 1)

  # Andel barn som korrekt klassificeras
  f_spec <- function(x) pM(x) * f_pi_0(x)
  # Andel vuxna som korrekt klassificeras
  f_sens <- function(x) pM(x) * f_pi_1(x)

  r_spec <- 1 - integrate(f_spec, lower = age.min, upper = 18)$value
  r_sens <- integrate(f_sens, lower = 18, upper = age.max)$value

  c(specificity = r_spec, sensitivity = r_sens)

}

#' @title Specificitet och sensitivitet för alla alternativ/metoder.
#' @description Specificitet och sensitivitet för alla alternativ/metoder, se ekvation (15).
#' @param age.min Lägsta ålder i åldersfördelningen, se ekvation (1).
#' @param age.max Högsta ålder i åldersfördelningen, se ekvation (1).
#' @export
spec_sens_all <- function(
  age.min = 15,
  age.max = 21
) {

  sp_i <- function(pM) spec_sens(pM, age.min = age.min, age.max = age.max)
  res <- rbind(
    `Hand-18` = sp_i(pM = pM_hand_18),
    `Hand-19` = sp_i(pM = pM_hand_19),
    `Hand-20` = sp_i(pM = pM_hand_20),
    Tand = sp_i(pM = pM_tand),
    Kna = sp_i(pM = pM_kna),
    RMV = sp_i(pM = pM_RMV),
    `Singla slant` = sp_i(pM = pM_coinflip),
    Barn = sp_i(pM = pM_barn),
    Vuxen = sp_i(pM = pM_vuxen)
  )
  rnames <- rownames(res)
  res <- as.data.frame(res)
  res$method <- rnames
  row.names(res) <- NULL
  res[, c("method", "specificity", "sensitivity")]
}

#' @title Andel som klassificeras som vuxna.
#' @description Andel som klassificeras som vuxna mot prevalensen för alla alternativ/metoder och även specificitet, sensitivet se ekvation (15) och accuracy.
#' @param by Steglängd för prevalensen mellan 0 och 1, default är 0.1
#' @param age.min Lägsta ålder i åldersfördelningen, se ekvation (1).
#' @param age.max Högsta ålder i åldersfördelningen, se ekvation (1).
#' @export
classified_adults <- function(by = 0.1,
                       age.min = 15,
                       age.max = 21)  {
  res <- spec_sens_all(age.min = age.min, age.max = age.max)
  prev <- seq(0, 1, by = by)


  # accuracy = (1-pi) specificitet + pi sensitivitet
  # klassificerade vuxna = (1-pi)(1-specificitet) + pi sensitivitet
  res2 <- do.call("rbind",
                  lapply(prev, function(x) {
                    res$prevalence <- x
                    res$accuracy <- (1 - res$prevalence) * res$specificity +
                      res$prevalence * res$sensitivity
                    res$classified_adults <-  (1 - res$prevalence) * (1 - res$specificity) +
                      res$prevalence * res$sensitivity
                    res
                  })
  )
  res2

}
