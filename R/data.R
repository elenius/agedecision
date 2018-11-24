#' Data för handled
#'
#' @name hand
#' @docType data
#' @usage data(hand)
#'
#' @format data.frame med 3 kolumner och 6 rader.
#' \describe{
#'   \item{age}{Faktisk ålder}
#'   \item{mean_ba}{Medelvärde för justerad benålder givet faktisk ålder.}
#'   \item{sd_ba}{Standardavvikelsen för justerad benålder givet faktiskt ålder.}...
#' }
#' @references Thodberg m. fl. 2017
"hand"

#' Data för visdomstand
#'
#' @name tand
#' @docType data
#'
#' @format data.frame 5 kolumner och 14 rader.
#' \describe{
#'   \item{age}{Faktisk ålder}
#'   \item{n}{Antal studiedeltagare givet ålder.}
#'   \item{moget}{Antal studiedeltagare med mogen visdomstand.}
#'   \item{omoget}{Antal studiedeltagare med omogen visdomstand.}
#' }
#' @references Simonsson m. fl (2017)
"tand"

#' Data för knäled
#'
#'
#' @name kna
#' @docType data
#'
#' @format Dataset med 5 kolumner och 14 rader.
#' \describe{
#'   \item{age}{Faktisk ålder}
#'   \item{n}{Antal studiedeltagare givet ålder.}
#'   \item{moget}{Antal studiedeltagare med mogen knäled.}
#'   \item{omoget}{Antal studiedeltagare med omogen knäled.}
#' }
#' @references Krämer m. fl. (2014)
#' @references Saint-Martin m. fl. (2015)
"kna"

#' Logistiska regressionskoefficienter
#'
#' De logistiska regressionskoefficienterna som skattats från visdomstand (tand),
#' knäled (knä) och handled (hand).
#'
#' @name logcoef
#' @docType data
#'
#' @format Dataset med 5 kolumner och 14 rader.
#' \describe{
#'   \item{intercept}{}
#'   \item{slope}{}
#'   \item{method}{Vilken metod som avses.}
#' }
"logcoef"
