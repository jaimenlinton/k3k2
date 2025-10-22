#' Create a k3k2Diagram object
#'
#' @param data A k3k2 object (optional)
#' @param reference Character vector of reference curves; default c("Gamma", "Gamma Inverse"). Other available references: "Normal"
#' @param design_parameters List of parameters (point size, colours...)
#' @return An object of class 'k3k2Diagram'
#' @export
k3k2Diagram <- function(data = NULL, reference = c("Gamma", "Gamma Inverse")) {

  if (!is.null(data) && !inherits(data, "k3k2"))
    stop("data must be a k3k2 object")

  structure(list(data = data,
      reference = reference),
    class = "k3k2Diagram")}

# Computing Reference curves for Gamma and Inverse Gamma
get_reference_curves <- function(
    reference = c("Gamma", "Gamma Inverse"),
    alpha = seq(0.2, 50, length.out = 10000)){

  dfs <- list()
  for (r in reference) {
     if (r == "Gamma") {
      k2 <- psigamma(alpha, 1) # trigamma
      k3 <- psigamma(alpha, 2) # polygamma order 2
      dfs[[r]] <- data.frame(alpha = alpha, kappa2 = k2, kappa3 = k3, reference = r, stringsAsFactors = FALSE)
    }
    else if (r == "Gamma Inverse") {
      k2 <- psigamma(alpha, 1)
      k3 <- -psigamma(alpha, 2)
      dfs[[r]] <- data.frame(alpha = alpha, kappa2 = k2, kappa3 = k3, reference = r, stringsAsFactors = FALSE)
    }
    else {
      stop(paste("Unknown reference:", r))
    }
  }
  do.call(rbind, dfs)
}


#' Plot method for k3k2Diagram objects
#' @export
plot_k3k2 <- function(x,
                      line_size = 1,
                      point_size = 3,
                      text_size = 12,
                      colours = NULL,
                      ...) {
  if (!inherits(x, "k3k2Diagram"))
    stop("Input must be a k3k2Diagram object")
  refdf <- get_reference_curves(reference = x$reference)
  refdf$reference <- factor(refdf$reference, levels = x$reference)

  if (is.null(colours)) {
    colours <- scales::hue_pal()(length(x$reference)) # generates distinct colors
  } else if (length(colours) < length(x$reference)) {
    stop("You must provide at least as many colours as reference curves")}

  p <- ggplot2::ggplot() +
    ggplot2::geom_path(
      data = refdf,
      ggplot2::aes(x = kappa3, y = kappa2, colour = reference, group = reference),
      linewidth = line_size
      ) +
    ggplot2::scale_color_manual(
      name = "Reference",
      values = colours,
      breaks = x$reference
    ) +
    ggplot2::labs(
      x = expression(tilde(kappa)[3]),
      y = expression(tilde(kappa)[2]),
      colour = "Reference"
      ) +
    ggplot2::theme_minimal(base_size = text_size)

  if (!is.null(x$data)) {
    samples_df <- do.call(rbind, lapply(seq_along(x$data), function(i) {
      d <- x$data[[i]]
      data.frame(
        sample = if (!is.null(names(x$data))) names(x$data)[i] else paste0("Sample", i),
        kappa2 = d$kappa2,
        kappa3 = d$kappa3,
        stringsAsFactors = FALSE)
    }))

    p <- p + ggplot2::geom_point(
      data = samples_df,
      ggplot2::aes(x = kappa3, y = kappa2, shape = sample),
      size = point_size)
  }

  return(p)
}



#' Null-coalescing operator (helper)
#'
#' Returns the first argument if it is not NULL, otherwise returns the second.
#' @keywords internal
`%||%` <- function(a, b) {
  if (!is.null(a)) a else b
}

#' Summary for k3k2Diagram
#' @export
summary.k3k2Diagram <- function(object, ...) {
  cat("k3k2Diagram\n")
  cat("References:", paste(object$reference, collapse = ", "), "\n")
  if (!is.null(object$data)) print.k3k2(object$data)
}

