// [[Rcpp::depends(bigmemory)]]
#include <Rcpp.h>
#include <bigmemory/MatrixAccessor.hpp>

// [[Rcpp::export]]
void cbindWriter(Rcpp::XPtr<BigMatrix> pA, Rcpp::XPtr<BigMatrix> pB, Rcpp::XPtr<BigMatrix> pC)  {
  MatrixAccessor<double> A(*pA);
  MatrixAccessor<double> B(*pB);
  MatrixAccessor<double> C(*pC);
  
  int rowdim = pA->nrow();
  
  for(int i = 0; i < rowdim; i++)  {
    C[0][i] = A[0][i];
    C[1][i] = B[0][i];
  }
}