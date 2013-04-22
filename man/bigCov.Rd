\name{bigCov}
\alias{bigCov}
\title{Covariance matrix for big.matrix objects}
\usage{
  bigCov(bigMat, chunksize = 1e+07)
}
\arguments{
  \item{bigMat}{big.matrix}

  \item{chunksize}{number of rows to use in the
  submatrices}
}
\value{
  covariance matrix
}
\description{
  Computes the covariance matrix of a big.matrix
}
\details{
  If the big.matrix has less than or equal to 10,000,000
  rows, the covariance is computed using the standard R cov
  function.  If the big.matrix has greater than 10,000,000
  rows, an updating algorithm is used to combine the
  covariance matrices of submatrices having number of rows
  equal to chunksize.
}
\examples{
#Create a matrix with 10,000,005 rows and 2 columns, regard it also as a big.matrix
require(bigmemory)
A <- matrix(rnorm(20000010), 10000005, 2)
bigA <- as.big.matrix(A)

#Compute covariance matrices using standard R and this bigCov function, and check equality
covA <- cov(A)
bigCovA <- bigCov(A)
all.equal(covA, bigCovA, check.attributes = FALSE)
}
\author{
  Alex Ustian <alex.l.ustian@gmail.com>
}

