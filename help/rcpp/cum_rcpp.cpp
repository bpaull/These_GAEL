#include <Rcpp.h>
using namespace Rcpp;
// [[Rcpp::export]]
NumericVector cumvar(NumericVector x) {
  int n = x.size();
  NumericVector result(n);
  
  for(int i = 0; i < n; i++) {
    NumericVector acc_x = x[Rcpp::Range(0, i)];
    result[i] = var(acc_x);
  }
  
  return(result);
}

// You can include R code blocks in C++ files processed with sourceCpp
// (useful for testing and development). The R code will be automatically 
// run after the compilation.
//

/*** R

*/
