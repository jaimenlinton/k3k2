#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
NumericVector logcumulants(NumericVector x) {
  if (is_true(any(x <= 0))) {
    stop("All input values must be positive for log");
  }

  int n = x.size();
  NumericVector logx = log(x);

  double mu = mean(logx); // compute mean of log(x)
  double c2 = 0.0, c3 = 0.0; // central moments

  for (int i = 0; i < n; i++){
    double d = logx[i] - mu;
    c2 += d * d;
    c3 += d * d *d;
  }

  c2 /= n;
  c3 /= n;

  return NumericVector::create(
    _["kappa2"] = c2,
    _["kappa3"] = c3
  );
}





