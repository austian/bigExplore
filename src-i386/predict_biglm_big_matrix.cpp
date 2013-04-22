// [[Rcpp::depends(bigmemory, RcppArmadillo)]]
#include <RcppArmadillo.h>
#include <bigmemory/MatrixAccessor.hpp>

// [[Rcpp::export]]
void biglmPredictCpp(Rcpp::XPtr<BigMatrix> pBigMat, Rcpp::XPtr<BigMatrix> pPred, arma::colvec coeff)  {
  MatrixAccessor<double> A(*pBigMat);
  MatrixAccessor<double> P(*pPred);
  
  int rowdim = pBigMat->nrow();
  int coldim = pBigMat->ncol();
  
  Rcpp::NumericVector row(coldim + 1);
  row[0] = 1;
  arma::rowvec armaRow(coldim + 1);
  
  for(int i = 0; i < rowdim; i++)  {
    for(int j = 0; j < coldim; j++)  {
      row[j+1] = A[j][i];
      armaRow = Rcpp::as<arma::rowvec>(row);
    }
    P[0][i] = arma::as_scalar(armaRow*coeff); 
  }
}