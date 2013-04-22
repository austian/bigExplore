#' @title Leverage using big.matrix objects
#' 
#' @description Computes the leverage (in the linear model sense) of a big.matrix.
#' 
#' @details Computes the leverage and displays cutoffs of potentially large data sets.  
#' The big.matrix class from the 'bigmemory' package is used to handle the large data sets and the resulting large 
#' output vector of leverages.  The computations and writing 
#' are done in C++ via Rcpp and RcppArmadillo to maximize performance.
#' 
#' This function will automatically create a file-backed big.matrix (of the leverages) with name "nameChar" by setting the new flag to true.  
#' If one desires to use ones own big.matrix (file-backed or in memory), keep the new flag false and set the nameChar to 
#' the name of the big.matrix available in the workspace.
#' 
#' @param dataExp big.matrix containing the explanatory variables
#' @param nameChar the name (of type character) of the big.matrix to record the data set, i.e. nameChar = "leverages"
#' @param new logical flag whether to automatically create a file-backed big.matrix
#' @param chunksize number of rows to use during covariance matrix computation
#' @param covMat covariance matrix to use in computation, if NULL it is computed from the given data
#' @param means column means to use in the computation, if NULL they are computed from the given data
#' @param cutoff logical flag to compute cutoff information for the leverages
#' @return single column big.matrix containing the leverages
#' @author Alex Ustian <alex.l.ustian@@gmail.com>
#' @export
#' @examples
#' #Create big.matrix of explanatory variables in memory
#' require(bigmemory)
#' A <- matrix(rnorm(200000), 100000, 2)
#' bigA <- as.big.matrix(A)
#' 
#' #Compute leverages and output to an in-memory big.matrix
#' leverage <- big.matrix(nrow = 100000, ncol = 1, type = "double")
#' leverage <- bigLeverage(bigA, "leverage", new = FALSE)
#' 
#' #Compute leverages and ouput to a file-backed big.matrix (warning creates files in the current working directory on your hard drive)
#' leverageFb <- bigLeverage(bigA, "leverageFb", new = TRUE)

bigLeverage <- function(dataExp, nameChar, new = F, chunksize = 10000000, covMat = NULL, means = NULL, cutoff = T)  {
  require(bigmemory)
  require(biganalytics)
  
  if(new == T)  {
    cat("Creating file-backed big.matrix in the working directory.\n")
    cat("Warning, there is a potentially large file being created on the hard drive.\n")  
    bkfile <- paste(nameChar, ".bin", sep = "")
    descfile <- paste(nameChar, ".desc", sep = "")
    name <- filebacked.big.matrix(nrow = dim(dataExp)[1], ncol = 1, type = "double", backingfile = bkfile, descriptorfile = descfile)  
  }  else  {
    name <- eval(parse(text = nameChar))
  }
  
  if(is.null(covMat))  {
  cat("Getting covariance \n")
  cov <- bigCov(dataExp, chunksize)
  }  else  {
    cov <- covMat
  }
  covInv <- solve(cov)
  
  if(is.null(means))  {
  cat("Getting column means \n")
  means <- colmean(dataExp)
  }  else  {
    means <- means
  }
  
  cat("Computing and writing\n\n")
  bigLevCpp(dataExp@address, name@address, means, covInv)
  
  if(cutoff == T)  {
    cutoffNum <- 2*(dim(dataExp)[2]+1)/dim(dataExp)[1] 
    cat("Using twice the average leverage as a cutoff:", cutoffNum, "\n")
    cutoff <- mwhich(name, 1, cutoffNum, "ge")
    cat("Percent of data outside this range:\n") 
    cat(length(cutoff), "entries out of", dim(dataExp)[1], "total\n")
    cat(100*length(cutoff)/dim(dataExp)[1], "% of the data\n")
    cat("Use mwhich to identify these entries")
  }
  return(name)
}  

