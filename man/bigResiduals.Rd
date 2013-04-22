\name{bigResiduals}
\alias{bigResiduals}
\title{Residuals using biglm and big.matrix objects}
\usage{
  bigResiduals(response, dataExp, biglmObj,
    type = c("resid", "standard", "student"), nameChar,
    new = F, chunksize = 1e+07, covMat = NULL,
    means = NULL, cutoff = T)
}
\arguments{
  \item{response}{single column big.matrix containing the
  response variable}

  \item{dataExp}{big.matrix containing the explanatory
  variables}

  \item{biglmObj}{corresponding fitted biglm object}

  \item{type}{one of "resid", "standard", or "student" for
  the type of residual to be computed}

  \item{nameChar}{the name (of type character) of the
  big.matrix to record the data set, i.e. nameChar =
  "residuals"}

  \item{new}{logical flag whether to automatically create a
  file-backed big.matrix}

  \item{chunksize}{number of rows to use during covariance
  matrix computation}

  \item{covMat}{covariance matrix to use in computation, if
  NULL it is computed from the given data}

  \item{means}{column means to use in the computation, if
  NULL they are computed from the given data}

  \item{cutoff}{logical flag to compute cutoff information
  for the studentized residuals}
}
\value{
  single column big.matrix containing the desired residuals
}
\description{
  Computes residuals, standardized residuals, and
  studentized residuals of linear models fitted with
  potentially large data sets.
}
\details{
  Computes residuals, standardized residuals, and
  studentized residuals of linear models fitted with
  potentially large data sets.  Cutoffs are displayed for
  the studentized case.  The big.matrix class from the
  'bigmemory' package and biglm method from the 'biglm'
  package are used to handle the large data sets and the
  resulting large output vector of residuals.  The
  computations and writing are done in C++ via Rcpp and
  Rcpparmadillo to maximize performance.

  This function will automatically create a file-backed
  big.matrix (of the residuals) with name "nameChar" by
  setting the new flag to true. If one desires to use ones
  own big.matrix (file-backed or in memory), keep the new
  flag false and set the nameChar to the name of the
  big.matrix available in the workspace.
}
\examples{
#Create big.matrix data set of the form y = x + epsilon in memory
require(bigmemory)
fuzzyLine <- big.matrix(nrow = 100000, ncol = 2, type = "double")
fuzzyLine <- fuzzyCurve(nrow = 100000, 1, 1, "fuzzyLine", new = FALSE)

#Separate response and explantory variables
fuzzyRes <- sub.big.matrix(fuzzyLine, firstCol = 2)
fuzzyExp <- sub.big.matrix(fuzzyLine, firstCol = 1, lastCol = 1)

#Fit a linear model
require(biglm)
require(biganalytics)
fuzzyLm <- biglm.big.matrix(y ~ x, data = fuzzyLine)

#Compute studentized residuals and output to an in-memory big.matrix
studentized <- big.matrix(nrow = 100000, ncol = 1, type = "double")
studentized <- bigResiduals(fuzzyRes, fuzzyExp, fuzzyLm, type = "student", "studentized", new = FALSE)
head(studentized)

#Compute studentized residuals and output to a file-backed big.matrix (warning creates files in the current working directory on your hard drive)
studentizedFb <- bigResiduals(fuzzyRes, fuzzyExp, fuzzyLm, type = "student", "studentizedFb", new = TRUE)
head(studentizedFb)
}
\author{
  Alex Ustian <alex.l.ustian@gmail.com>
}

