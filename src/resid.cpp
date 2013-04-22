// [[Rcpp::depends(bigmemory, RcppArmadillo)]]
#include <RcppArmadillo.h>
#include <bigmemory/MatrixAccessor.hpp>

// [[Rcpp::interfaces(r, cpp)]]

// [[Rcpp::export]]
void biglmResidCpp(Rcpp::XPtr<BigMatrix> pBigMat, Rcpp::XPtr<BigMatrix> pRes, Rcpp::XPtr<BigMatrix> pResid, arma::colvec coeff)  {
  MatrixAccessor<double> A(*pBigMat);
  MatrixAccessor<double> P(*pRes);
  MatrixAccessor<double> R(*pResid);
  
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
    R[0][i] = P[0][i] - arma::as_scalar(armaRow*coeff); 
  }
}