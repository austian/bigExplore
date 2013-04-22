\name{fuzzyCurve}
\alias{fuzzyCurve}
\title{Fuzzy curve generator}
\usage{
  fuzzyCurve(nrow, power, var, nameChar, new = F)
}
\arguments{
  \item{nrow}{the number of rows in the data set}

  \item{power}{the power of x to use for y}

  \item{var}{the variance of the normal distribution of
  epsilon}

  \item{nameChar}{the name (of type character) of the
  big.matrix to record the data set, i.e. nameChar =
  "dataset"}

  \item{new}{logical flag whether to automatically create a
  file-backed big.matrix}
}
\value{
  nrows x 2 dimensional big.matrix
}
\description{
  Creates a two column big.matrix random dataset from the
  equation y = x^n + epsilon.
}
\details{
  The first column is a uniformly random x in [0,1] and the
  second column consists of y = x^n + epsilon, where
  epsilon is a draw from a normal distribution of mean zero
  and specified variance and n is a specified power. This
  data set can be file-backed and hence can be large.  The
  underlying computations and writing are done in C++ with
  Rcpp to enhance performance.

  See the examples for a sample plotting method.

  This function will automatically create a file-backed
  big.matrix (of the resulting data set) with name
  "nameChar" by setting the new flag to true. If one
  desires to use ones own big.matrix (file-backed or in
  memory), keep the new flag false and set the nameChar to
  the name of the big.matrix available in the workspace.
}
\examples{
#In-memory big.matrix
require(bigmemory)
fuzzyParabola <- big.matrix(nrow = 100000, ncol = 2, type = "double")
fuzzyParabola <- fuzzyCurve(100000, 2, 1, "fuzzyParabola", new = FALSE)

#File-backed big.matrix (warning creates files in the current working directory on your hard drive)
require(bigmemory)
fuzzyCubic <- fuzzyCurve(100000, 3, .5, "fuzzyCubic", new = TRUE)

#Sample plot
require(bigmemory)
require(biganalytics)
fuzzyParabola <- big.matrix(nrow = 100000, ncol = 2, type = "double")
fuzzyParabola <- fuzzyCurve(100000, 2, 1, "fuzzyParabola", new = FALSE)
bins <- binit(fuzzyParabola, 1:2, breaks = 20)
filled.contour(bins$rowcenters, bins$colcenters, bins$counts, xlab="x",
ylab="y", col = rainbow(20, start = .5, end = .9))
}
\author{
  Alex Ustian <alex.l.ustian@gmail.com>
}

