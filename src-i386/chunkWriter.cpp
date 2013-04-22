//the start int refers to the R indexing of the big mat vector, not the C++ indexing

// [[Rcpp::depends(bigmemory)]]
#include <Rcpp.h>
#include <bigmemory/MatrixAccessor.hpp>

// [[Rcpp::export]]
void chunkWriter(Rcpp::XPtr<BigMatrix> pA, Rcpp::NumericVector data, int start, int chunksize)  {
  MatrixAccessor<double> A(*pA);
  
  for(int i = 0; i < chunksize; i++)  {
    A[0][i + start - 1] = data[i];
  }
}