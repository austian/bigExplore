\name{predict.biglm.big.matrix}
\alias{predict.biglm.big.matrix}
\title{Predict function for biglm and big.matrix objects}
\usage{
  predict.biglm.big.matrix(biglmObj, newx, nameChar,
    new = F)
}
\arguments{
  \item{biglmObj}{fitted biglm object}

  \item{newx}{big.matrix of new explanatory variable data
  with which to predict}

  \item{nameChar}{the name (of type character) of the
  big.matrix to record the data set, i.e. nameChar =
  "predictions"}

  \item{new}{logical flag whether to automatically create a
  file-backed big.matrix}
}
\value{
  single column big.matrix containing the predictions
}
\description{
  Predict on new values via a linear model, where the new
  values can be a big.matrix object.
}
\details{
  Allows one to predict using a biglm model and a
  big.matrix as the new values.  The computations and
  writing are done in C++ via Rcpp and Rcpparmadillo to
  maximize performance.

  This function will automatically create a file-backed
  big.matrix (of the predictions) with name "nameChar" by
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

#Get the explantory variables
fuzzyExp <- sub.big.matrix(fuzzyLine, firstCol = 1, lastCol = 1)

#Fit a linear model
require(biglm)
require(biganalytics)
fuzzyLm <- biglm.big.matrix(y ~ x, data = fuzzyLine)

#Compute fitted y values of our linear model and output to an in-memory big.matrix
preds <- big.matrix(nrow = 100000, ncol = 1, type = "double")
preds <- predict.biglm.big.matrix(fuzzyLm, fuzzyExp, "preds", new = FALSE)
head(preds)

#Compute fitted y values of our linear model and output to a file-backed big.matrix (warning creates files in the current working directory on your hard drive)
predsFb <- predict.biglm.big.matrix(fuzzyLm, fuzzyExp, "predsFb", new = TRUE)
head(predsFb)
}
\author{
  Alex Ustian <alex.l.ustian@gmail.com>
}

