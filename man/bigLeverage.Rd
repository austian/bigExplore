\name{bigLeverage}
\alias{bigLeverage}
\title{Leverage using big.matrix objects}
\usage{
  bigLeverage(dataExp, nameChar, new = F,
    chunksize = 1e+07, covMat = NULL, means = NULL,
    cutoff = T)
}
\arguments{
  \item{dataExp}{big.matrix containing the explanatory
  variables}

  \item{nameChar}{the name (of type character) of the
  big.matrix to record the data set, i.e. nameChar =
  "leverages"}

  \item{new}{logical flag whether to automatically create a
  file-backed big.matrix}

  \item{chunksize}{number of rows to use during covariance
  matrix computation}

  \item{covMat}{covariance matrix to use in computation, if
  NULL it is computed from the given data}

  \item{means}{column means to use in the computation, if
  NULL they are computed from the given data}

  \item{cutoff}{logical flag to compute cutoff information
  for the leverages}
}
\value{
  single column big.matrix containing the leverages
}
\description{
  Computes the leverage (in the linear model sense) of a
  big.matrix.
}
\details{
  Computes the leverage and displays cutoffs of potentially
  large data sets. The big.matrix class from the
  'bigmemory' package is used to handle the large data sets
  and the resulting large output vector of leverages.  The
  computations and writing are done in C++ via Rcpp and
  RcppArmadillo to maximize performance.

  This function will automatically create a file-backed
  big.matrix (of the leverages) with name "nameChar" by
  setting the new flag to true. If one desires to use ones
  own big.matrix (file-backed or in memory), keep the new
  flag false and set the nameChar to the name of the
  big.matrix available in the workspace.
}
\examples{
#Create big.matrix of explanatory variables in memory
require(bigmemory)
A <- matrix(rnorm(200000), 100000, 2)
bigA <- as.big.matrix(A)

#Compute leverages and output to an in-memory big.matrix
leverage <- big.matrix(nrow = 100000, ncol = 1, type = "double")
leverage <- bigLeverage(bigA, "leverage", new = FALSE)

#Compute leverages and ouput to a file-backed big.matrix (warning creates files in the current working directory on your hard drive)
leverageFb <- bigLeverage(bigA, "leverageFb", new = TRUE)
}
\author{
  Alex Ustian <alex.l.ustian@gmail.com>
}

