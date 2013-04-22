#' @title Filled contour plot of studentized residuals against fitted values
#' 
#' @description Creates a filled contour plot of the studentized residuals against the fitted values for linear models 
#' fitted with potentially large data sets.
#' 
#' @details The biganalytics package has a built-in function called binit used to properly discretize and tabulate the 
#' fitted values and studentized residuals.  binit requires that these two single column matrices lie in a single double column 
#' matrix.  (i.e. a bigmemory cbind() is required)  
#' 
#' This function will automatically create a file-backed big.matrix (of the combined fitted values and studentized residuals) 
#' with name "nameChar" by setting the new flag to true.  If one desires to use ones own big.matrix (file-backed or in 
#' memory), keep the new flag false and set the nameChar to the name of the big.matrix available in the workspace.  
#' 
#' The required cbind() function is written in C++ via Rcpp in order to maximize performance.
#' 
#' @param fitted single column big.matrix of the fitted values
#' @param studentized single column big.matrix of the studentized residuals
#' @param nameChar the name (of type character) of the big.matrix to record the data set, i.e. nameChar = "residMat"
#' @param new logical flag whether to automatically create a file-backed big.matrix
#' @return two column big.matrix containing the fitted values in the first column and the studentized residuals in the second
#' @author Alex Ustian <alex.l.ustian@@gmail.com>
#' @export
#' @examples
#' #Create big.matrix data set of the form y = x^3 + epsilon in memory
#' require(bigmemory)
#' fuzzyCubic <- big.matrix(nrow = 100000, ncol = 2, type = "double")
#' fuzzyCubic <- fuzzyCurve(nrow = 100000, 3, .5, "fuzzyCubic", new = FALSE)
#' 
#' #Separate response and explantory variables
#' fuzzyRes <- sub.big.matrix(fuzzyCubic, firstCol = 2)
#' fuzzyExp <- sub.big.matrix(fuzzyCubic, firstCol = 1, lastCol = 1)
#' 
#' #Fit a linear model
#' require(biglm)
#' require(biganalytics)
#' fuzzyLm <- biglm.big.matrix(y ~ x, data = fuzzyCubic)
#' 
#' #Compute studentized residuals and output to an in-memory big.matrix
#' studentized <- big.matrix(nrow = 100000, ncol = 1, type = "double")
#' studentized <- bigResiduals(fuzzyRes, fuzzyExp, fuzzyLm, type = "student", "studentized", new = FALSE)
#' 
#' #Compute fitted values and output to an in-memory big.matrix
#' fitted <- big.matrix(nrow = 100000, ncol = 1, type = "double")
#' fitted <- predict.biglm.big.matrix(fuzzyLm, fuzzyExp, "fitted", new = FALSE)
#' 
#' #Create the residuals plot (with the auxiliary matrix outputted in memory)
#' #Note that this clearly displays the poor fit of our linear model to a cubic curve
#' residMat <- big.matrix(nrow = 100000, ncol = 2, type = "double")
#' residMat <- bigResidualsPlot(fitted, studentized, "residMat", new = FALSE)

bigResidualsPlot <- function(fitted, studentized, nameChar, new = F)  {
  
  rowdim <- dim(fitted)[1]
  
  if(new == T)  {
    cat("Creating file-backed matrix in the working directory.\n")
    cat("Warning, there is a potentially large file being created on the hard drive.\n")  
    bkfile <- paste(nameChar, ".bin", sep = "")
    descfile <- paste(nameChar, ".desc", sep = "")
    residualMatrix <- filebacked.big.matrix(nrow = rowdim, ncol = 2, type = "double", backingfile = bkfile, descriptorfile = descfile)  
  }  else  {
    residualMatrix <- eval(parse(text = nameChar))
  }
  
  cat("Merging fitted values and studentized residuals\n")
  cbindWriter(fitted@address, studentized@address, residualMatrix@address)
  
  cat("Binning")
  bins <- binit(residualMatrix, 1:2, breaks = 20)
  filled.contour(bins$rowcenters, bins$colcenters,
                 bins$counts, xlab="Fitted Values",
                 ylab="Studentized", col = rainbow(length(pretty(range(bins$counts), 20)), start = .5, end = .9), plot.axes = {axis(1); axis(2); abline(h = 0)})
  
  return(residualMatrix)
}