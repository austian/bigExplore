// [[Rcpp::depends(bigmemory, RcppArmadillo)]]
#include <RcppArmadillo.h>
#include <bigmemory/MatrixAccessor.hpp>

// [[Rcpp::export]]
void bigLevCpp(Rcpp::XPtr<BigMatrix> pBigMat, Rcpp::XPtr<BigMatrix> pLev, arma::colvec means, arma::mat covInv)  {
  MatrixAccessor<double> A(*pBigMat);
  MatrixAccessor<double> L(*pLev);
  
  int rowdim = pBigMat->nrow();
  int coldim = pBigMat->ncol();
  
  arma::colvec row(coldim);
  arma::colvec centRow(coldim);
  double mDist;
  
  for(int i = 0; i < rowdim; i++)  {
    for(int j = 0; j < coldim; j++)  {
      row[j] = A[j][i];
      centRow[j] = row[j] - means[j];
    }
    mDist = as_scalar(centRow.t()*covInv*centRow);
    L[0][i] = (1.0 / rowdim) + (1.0 / (rowdim - 1))*mDist; 
  }
} 
