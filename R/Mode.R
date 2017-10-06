#' Mode Function for finding the most frequent value in a vector
#'
#' @param x a vector
#' @return the most frequent value
#' @export
#'
Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}
