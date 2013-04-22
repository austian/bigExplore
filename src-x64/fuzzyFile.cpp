// [[Rcpp::depends(bigmemory)]]
#include <Rcpp.h>
#include <bigmemory/MatrixAccessor.hpp>
#include <math.h>

// [[Rcpp::export]]
void fuzzyFileCpp(Rcpp::XPtr<BigMatrix> pBigMat, double power, double var)  {
  MatrixAccessor<double> A(*pBigMat);
  
  int rowdim = pBigMat->nrow();
  
  Rcpp::RNGScope scope;
  Rcpp::NumericVector x;
  double y;
  Rcpp::NumericVector e;
  double edub;
  
  for(int i = 0; i < rowdim; i++)  {
    x = Rcpp::runif(1);
    A[0][i] = Rcpp::as<double>(x);
    y = pow(A[0][i], power);
    e = Rcpp::rnorm(1, 0, var);
    edub = Rcpp::as<double>(e);
    A[1][i] = y + edub;   
  }
}
  
  