#' Create a k3k2 object
#'
#' @param samples A numeric vector, or a list of numeric vectors
#' @return An object of class 'k3k2'
#' @export
k3k2 <- function(samples) {
  if (is.data.frame(samples) || is.matrix(samples)) {
    samples <- as.list(as.data.frame(samples))
  } else if (is.numeric(samples)) {
    samples <- list(samples)
  }

  if (!is.list(samples)) stop("samples must be a vector, or list of vectors")

  out <- lapply(seq_along(samples), function(i) {
    s <- samples[[i]]
    if (!is.numeric(s)) stop(sprintf("sample %d is not numeric", i))
    if (anyNA(s)) stop("samples must not contain NA values")
    if (any(s <= 0)) stop("All input values must be positive for log")

    vals <- logcumulants(s)
    list(data = s, kappa2 = vals[["kappa2"]], kappa3 = vals[["kappa3"]])
  })

  names(out) <- paste0("sample", seq_along(out))
  class(out) <- "k3k2"
  return(out)
}

#' Print method for k3k2 objects
#' @export
print.k3k2 <- function(x) {
  cat("k3k2 object:\n")
  for (i in seq_along(x)) {
    cat("Sample", i, ":\n")
    cat("  Data:   ", paste(x[[i]]$data, collapse = ", "), "\n")
    cat("  kappa2: ", x[[i]]$kappa2, "\n")
    cat("  kappa3: ", x[[i]]$kappa3, "\n\n")
  }
}

# Summary function will be included for final project hand-in

#summary.k3k2 <- function(object, ...) {
 # cat("Summary of k3k2 object:\n")
  #cat("Number of samples:", length(object), "\n")
  #sizes <- vapply(object, function(el) length(el$data), integer(1))
  #cat("Sample sizes:", paste(sizes, collapse = ", "), "\n")
  #kappa2 <- vapply(object, function(el) el$kappa2, numeric(1))
  #kappa3 <- vapply(object, function(el) el$kappa3, numeric(1))
  #cat("kappa2:", kappa2, "\n")
  #cat("kappa3:", kappa3, "\n")
#}
