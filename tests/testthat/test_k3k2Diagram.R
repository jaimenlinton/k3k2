library(testthat)
library(k3k2)


x <- c(1,2,3,4)
obj <- k3k2(x)
d <- k3k2Diagram(obj)


test_that("diagram plotting returns ggplot object", {
  x <- c(1, 2, 3, 4, 5)
  obj <- k3k2(x)
  diagram <- k3k2Diagram(data = obj)
  expect_s3_class(diagram, "k3k2Diagram")
  p <- plot_k3k2(diagram)
  expect_true(inherits(p, "ggplot"))
})

test_that("k3k2Diagram rejects non-k3k2 input", {
  x <- list(1, 2, 3)
  expect_error(k3k2Diagram(x), "data must be a k3k2 object")
})
