#' @title Cook's distance using biglm and big.matrix objects
#' 
#' @description Computes Cook's distance of linear models fitted with potentially large data sets.
#' 
#' @details Computes Cook's distance and displays cutoffs of linear models fitted with 
#' potentially large data sets.  The big.matrix class from the 'bigmemory' package and biglm method from the 'biglm' package
#' are used to handle the large data sets and the resulting large output vector of Cook's distances.  The computations and writing 
#' are done in C++ via Rcpp and Rcpparmadillo to maximize performance.
#' 
#' This function will automatically create a file-backed big.matrix (of the residuals) with name "nameChar" by setting the new flag to true.  
#' If one desires to use ones own big.matrix (file-backed or in memory), keep the new flag false and set the nameChar to 
#' the name of the big.matrix available in the workspace.
#' 
#' @param response single column big.matrix containing the response variable
#' @param dataExp big.matrix containing the explanatory variables
#' @param biglmObj corresponding fitted biglm object
#' @param nameChar the name (of type character) of the big.matrix to record the data set, i.e. nameChar = "CooksD"
#' @param new logical flag whether to automatically create a file-backed big.matrix
#' @param chunksize number of rows to use during covariance matrix computation
#' @param covMat covariance matrix to use in computation, if NULL it is computed from the given data
#' @param means column means to use in the computation, if NULL they are computed from the given data
#' @param cutoff logical flag to compute cutoff information for the Cook's distances
#' @return single column big.matrix containing the Cook's distances
#' @author Alex Ustian <alex.l.ustian@@gmail.com>
#' @export
#' @examples
#' #Create big.matrix data set of the form y = x + epsilon in memory
#' require(bigmemory)
#' fuzzyLine <- big.matrix(nrow = 100000, ncol = 2, type = "double")
#' fuzzyLine <- fuzzyCurve(nrow = 100000, 1, 1, "fuzzyLine", new = FALSE)
#' 
#' #Separate response and explantory variables
#' fuzzyRes <- sub.big.matrix(fuzzyLine, firstCol = 2)
#' fuzzyExp <- sub.big.matrix(fuzzyLine, firstCol = 1, lastCol = 1)
#' 
#' #Fit a linear model
#' require(biglm)
#' require(biganalytics)
#' fuzzyLm <- biglm.big.matrix(y ~ x, data = fuzzyLine)
#' 
#' #Compute Cook's distances and output to an in-memory big.matrix
#' CooksD <- big.matrix(nrow = 100000, ncol = 1, type = "double")
#' CooksD <- bigCooks(fuzzyRes, fuzzyExp, fuzzyLm, "CooksD", new = FALSE)
#' head(CooksD)
#' 
#' #Compute Cook's distances and output to a file-backed big.matrix (warning creates files in the current working directory on your hard drive)
#' CooksDFb <- bigCooks(fuzzyRes, fuzzyExp, fuzzyLm, "CooksDFb", new = TRUE) 
#' head(CooksDFb)      

bigCooks <- function(response, dataExp, biglmObj, nameChar, new = F, chunksize = 10000000, covMat = NULL, means = NULL, cutoff = T)  {
  require(biglm)
  require(bigmemory)
  require(biganalytics)
  
  rowdim <- biglmObj$n
  
  if(new == T)  {
    cat("Creating file-backed big.matrix in the working directory.\n")
    cat("Warning, there is a potentially large file being created on the hard drive.\n")
    bkfile <- paste(nameChar, ".bin", sep = "")
    descfile <- paste(nameChar, ".desc", sep = "")
    name <- filebacked.big.matrix(nrow = rowdim, ncol = 1, type = "double", backingfile = bkfile, descriptorfile = descfile)  
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
  bigCooksCpp(dataExp@address, response@address, name@address, coef(biglmObj), means, covInv)
  
  if(cutoff == T)  {
    cutoffNum <- 4/(rowdim - dim(dataExp)[2] - 1)
    cutoff <- mwhich(name, 1, cutoffNum, "gt")
    cat("Using the 4/n-p-1 cutoff:", cutoffNum, "\n")
    cat("Percent of data outside this range:\n")
    cat(length(cutoff), "entries out of", rowdim, "total\n")
    cat(100*length(cutoff)/rowdim, "% of the data\n")
    cat("Use mwhich to identify these entries")
  }
  return(name)
}