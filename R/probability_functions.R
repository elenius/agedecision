#' @title Logistisk funktion
#' @description  Logistisk funktion som ger sannolikheten att kroppsdelen bedöms mogen givet ålder, se ekvation (7).
#' @param x Ålder.
#' @param intercept Interceptet från en logistisk regression.
#' @param slope Lutningen från en logistisk regression.
#'
#' @export
logistic_function <- function(x, intercept, slope) {
  (1 + exp(-1 * (intercept + slope * x)))^-1
}


#' @name pM
#' @family Sannolikhetsfunktioner
#' @title Sannolikhetsfunktioner från studieresultaten.
#' @description Funktioner från studieresultaten, endast ålder (x) i år anges.
#' @param x Ålder.
#' @seealso \link{logistic_function}
#' @seealso \link{RMV}
NULL


#' @rdname pM
#' @export
pM_tand <- function(x) logistic_function(x, intercept = coef.tand[[1]], slope = coef.tand[[2]])
#' @rdname pM
#' @export
pM_kna <- function(x) logistic_function(x, intercept = coef.kna[[1]], slope = coef.kna[[2]])
#' @rdname pM
#' @export
pM_hand_18 <- function(x) logistic_function(x, intercept = coef.hand.18[[1]], slope = coef.hand.18[[2]])
#' @rdname pM
#' @export
pM_hand_19 <- function(x) logistic_function(x, intercept = coef.hand.19[[1]], slope = coef.hand.19[[2]])
#' @rdname pM
#' @export
pM_hand_20 <- function(x) logistic_function(x, intercept = coef.hand.20[[1]], slope = coef.hand.20[[2]])

#' @title Centrerad ålder för en logistisk funktion
#' @description Givet regressionskoefficienterna från en logistisk regression
#' erhålls testets centrerade ålder, vilket innebär den åldern där 50% bedöms med
#' mogen respektive omogen kroppsdel.
#' @param intercept Interceptet från logistisk regression.
#' @param slope Lutningen från logistisk regression.
#'
#' @export
logistic_center <- function(intercept, slope) -1 * intercept / slope

#' @title Rättsmedicinalverkets metod
#' @description Rättsmedicinalverkets metod med två oberoende bedömare för knä respektive tand och kombinationen att minst en visar mogen.
#' @param intercept1 intercept metod 1
#' @param slope1 lutning metod 1
#' @param kappa1 kappa metod 1
#' @param intercept2 intercept metod 2
#' @param slope2 lutning metod 1
#' @param kappa2 kappa metod 1
#'
#' @export
# RMVs sätt att kombinera två logistiska funktioner
RMV <- function(x, intercept1, slope1, kappa1,
                intercept2, slope2, kappa2) {

  # p är andelen med mogen kroppsdel
  # kappa är kappakoefficient
  # se ekvation 12
  # sannolikheten att i båda bedömningarna bedömas med mogen kroppsdel
  s_p_kappa <- function(p, kappa) {
    kappa * p + (1 - kappa) * p^2
  }


  # oberoende inter-variation mellan bedömare
  # sannolikheten att bedömas som mogen, givet ålder (x)
  p1 <- logistic_function(x, intercept1, slope1)
  p2 <- logistic_function(x, intercept2, slope2)

  # Ekvation 12, sannoliketen att båda bedömarna bedömer mogen
  s_1 = s_p_kappa(p1, kappa1) # metod 1
  s_2 = s_p_kappa(p2, kappa2) # metod 2

  # Ekvation 14, kombinationen av metod 1 och 2
  s_1 + s_2 - (s_1 * s_2)
}

#' @rdname pM
#' @export
pM_RMV <- function(x) RMV(x,
                         intercept1 = coef.tand[[1]],
                         slope1 = coef.tand[[2]],
                         kappa1 = 0.743,
                         intercept2 = coef.kna[[1]],
                         slope2 = coef.kna[[2]],
                         kappa2 = 0.661)

#' @rdname pM
#' @export
pM_coinflip <- function(x) 0.5

#' @rdname pM
#' @export
pM_barn <- function(x) 0

#' @rdname pM
#' @export
pM_vuxen <- function(x) 1


