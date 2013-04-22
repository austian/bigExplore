#' @title Filled contour plot of studentized residuals against leverages
#' 
#' @description Creates a filled contour plot of the studentized residuals against the leverages for linear models 
#' fitted with potentially large data sets.
#' 
#' @details On top of the contour plot horizontal lines marking studentized residuals of values -2 and 2 and vertical lines
#' marking twice the average leverage value and three times the average leverage value are drawn.  If most of the leverages 
#' are small these vertical lines may not appear.
#' 
#' The biganalytics package has a built-in function called binit used to properly discretize and tabulate the 
#' leverages and studentized residuals.  binit requires that these two single column matrices lie in a single double column 
#' matrix.  (i.e. a bigmemory cbind() is required)  
#' 
#' This function will automatically create a file-backed big.matrix (of the combined leverages and studentized residuals) 
#' with name "nameChar" by setting the new flag to true.  If one desires to use ones own big.matrix (file-backed or in 
#' memory), keep the new flag false and set the nameChar to the name of the big.matrix available in the workspace.  
#' 
#' The required cbind() function is written in C++ via Rcpp in order to maximize performance.
#' 
#' @param leverage single column big.matrix of the leverages
#' @param studentized single column big.matrix of the studentized residuals
#' @param biglmObj corresponding fitted biglm object
#' @param nameChar the name (of type character) of the big.matrix to record the data set, i.e. nameChar = "influenceMat"
#' @param new logical flag whether to automatically create a file-backed big.matrix
#' @return two column big.matrix containing the leverages in the first column and the studentized residuals in the second
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
#' #Compute studentized residuals and output to an in-memory big.matrix
#' studentized <- big.matrix(nrow = 100000, ncol = 1, type = "double")
#' studentized <- bigResiduals(fuzzyRes, fuzzyExp, fuzzyLm, type = "student", "studentized", new = FALSE)
#' 
#' #Compute leverages and output to an in-memory big.matrix
#' leverage <- big.matrix(nrow = 100000, ncol = 1, type = "double")
#' leverage <- bigLeverage(fuzzyExp, "leverage", new = FALSE)
#' 
#' #Create the influence plot (with the auxiliary matrix outputted in memory)
#' infMat <- big.matrix(nrow = 100000, ncol = 2, type = "double")
#' infMat <- bigInfluencePlot(leverage, studentized, fuzzyLm, "infMat", new = FALSE)

bigInfluencePlot <- function(leverage, studentized, biglmObj, nameChar, new = F)  {
  require(bigmemory)
  require(biganalytics)
  
  rowdim <- dim(leverage)[1]
  coldim <- length(biglmObj$names)
  
  if(new == T)  {
    cat("Creating file-backed matrix in the working directory.\n")
    cat("Warning, there is a potentially large file being created on the hard drive.\n")  
    bkfile <- paste(nameChar, ".bin", sep = "")
    descfile <- paste(nameChar, ".desc", sep = "")
    influenceMatrix <- filebacked.big.matrix(nrow = rowdim, ncol = 2, type = "double", backingfile = bkfile, descriptorfile = descfile)  
  }  else  {
      influenceMatrix <- eval(parse(text = nameChar))
  }
  
  cat("Merging leverages and studentized residuals\n")
  cbindWriter(leverage@address, studentized@address, influenceMatrix@address)
  
  cat("Binning")
  bins <- binit(influenceMatrix, 1:2, breaks = 20)
  filled.contour(bins$rowcenters, bins$colcenters,
                 bins$counts, xlab="Leverage",
                 ylab="Studentized", col = rainbow(length(pretty(range(bins$counts), 20)), start = .5, end = .9),
                 plot.axes = {axis(1); axis(2); abline(h = 2); abline(h = -2); abline(v = 2*coldim/rowdim); abline(v = 3*coldim/rowdim)})
  
  return(influenceMatrix)
}