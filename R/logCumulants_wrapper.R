#' Compute log-cumulants (tilde kappa2 and kappa3)
#'
#' @param x Numeric vector of positive values (no NA)
#' @return Named numeric vector with elements 'kappa2' and 'kappa3'
#' @useDynLib k3k2, .registration = TRUE
#' @importFrom Rcpp sourceCpp
#' @export
logcumulants_wrapper <- function(x) {
  if (!is.numeric(x)) stop("x must be numeric")
  if (length(x) == 0) stop("x must not be empty")
  if (anyNA(x)) stop("x must not contain NA values")
  if (any(x <= 0)) stop("All input values must be positive for log")


  # Call the Rcpp function
  k3k2::logcumulants(x)
}
