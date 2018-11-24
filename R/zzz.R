
na_to_zero <- function(x) {
  x[is.na(x)] <- 0
  x
}
