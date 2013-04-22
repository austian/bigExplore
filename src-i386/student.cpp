// [[Rcpp::depends(bigmemory, RcppArmadillo)]]
#include <RcppArmadillo.h>
#include <bigmemory/MatrixAccessor.hpp>
#include <math.h>

// [[Rcpp::export]]
void biglmStudentCpp(Rcpp::XPtr<BigMatrix> pBigMat, Rcpp::XPtr<BigMatrix> pRes, Rcpp::XPtr<BigMatrix> pStudent, arma::colvec coeff, arma::colvec means, arma::mat covInv)  {
  MatrixAccessor<double> A(*pBigMat);
  MatrixAccessor<double> P(*pRes);
  MatrixAccessor<double> S(*pStudent);
  
  int rowdim = pBigMat->nrow();
  int coldim = pBigMat->ncol();
  
  arma::rowvec row(coldim + 1);
  arma::colvec centRow(coldim);
  row[0] = 1;
  
  double resid;
  double diff = rowdim - coldim;
  double sampVar = 0;
  double sampStdev;
  
  for(int i = 0; i < rowdim; i++)  {
    for(int j = 0; j < coldim; j++)  {
      row[j+1] = A[j][i];    
    }
    resid = P[0][i] - arma::as_scalar(row*coeff);
    sampVar += (1/(diff-1))*resid*resid;
  }
  sampStdev = sqrt(sampVar);
  
  double lev;
  double mDist;
  double stan;
  
  for(int i = 0; i < rowdim; i++)  {
    for(int j = 0; j < coldim; j++)  {
      row[j+1] = A[j][i];
      centRow[j] = row[j+1] - means[j];
    }
    resid = P[0][i] - arma::as_scalar(row*coeff);
    mDist = arma::as_scalar(centRow.t()*covInv*centRow);
    lev = (1.0 / rowdim) + (1.0 / (rowdim - 1))*mDist;
    stan = (1 / (sampStdev*sqrt(1 - lev)))*resid;
    S[0][i] = stan*sqrt((diff - 1)/(diff - stan*stan));
  }     
}