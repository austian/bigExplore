#' @title Outlier test for studentized residuals
#' 
#' @description t-tests for the studentized residuals to test whether a given data point is an outlier
#' 
#' @details The studentized residuals can be interpreted as measuring the discrepancy of fitting a linear model which specifically 
#' accounts for the corresponding data point being an outlier.  This discrepancy is exactly the studentized residual and it follows a 
#' t-distribution.  One can then measure the statistical significance of the residual (and hence the discrepancy). 
#' 
#' Since one is looking for entries which have statistically significant residuals, in fact one is conducting multiple tests 
#' in the search for large residuals.  In a smaller data set, a Bonferroni correction may be used to take this multiple 
#' comparison issue into account.  However with data sets having potentially hundreds of millions of rows, even the more 
#' sophisticated multiple comparison methods are too conservative in their p-value adjustments.  
#' 
#' Since we are searching for outlying points, having many false negatives is not necessarily a bad thing and still gives us an 
#' interesting and smaller data set to sift through.  In this regard, this test returns all rows with the specified 
#' significance level with no multiple comparison correction.  
#' 
#' Another approach to multiple comparison is via the false discovery rate (FDR), which attempts to control the number of false 
#' positives.  Again by having such large data sets the algorithms which compute adjusted p-values corresponding to a fixed FDR level  
#' return uselessly large p-values.  We instead use a p-value which is computed as an average from the Benjamini,Hochberg,Yekutieli 
#' FDR procedure.  This gives a smaller subset of the previously mentioned significance test.  
#' 
#' The function returns a list containing the significant rows in the first entry, and the average FDR significant rows in the second.
#' 
#' @param studentized single column big.matrix containing the studentized residuals
#' @param biglmObj corresponding fitted biglm object
#' @param sigLevel significance level for the t-tests
#' @param approxFDR FDR level to use to calculate the average to adjust the p-values
#' @return a list with first coordinate the significant residuals and second coordinate the average FDR significant residuals
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
#' #Run the outlier test
#' outliers <- bigOutlierTest(studentized, fuzzyLm)
       
bigOutlierTest <- function(studentized, biglmObj, sigLevel = .05, approxFDR = .05)  {
  require(bigmemory)
  require(biganalytics)
  
  rowdim <- dim(studentized)[1]

  df <- rowdim - length(biglmObj$names) - 3
  levelCutUp <- qt(1-sigLevel, df)
  levelCutDown <- qt(sigLevel, df)
  
  rfdr <- (approxFDR*(rowdim + 1))/(2*rowdim)
  afdr <- rfdr/(log(rowdim) + .57721)
  fdrCutUp <- qt(1 - afdr, df)
  fdrCutDown <- qt(afdr,df)
  
  sig <- c(mwhich(studentized, 1, levelCutUp, "ge"), mwhich(studentized, 1, levelCutDown, "le"))
  aFDR <- c(mwhich(studentized, 1, fdrCutUp, "ge"), mwhich(studentized, 1, fdrCutDown, "le"))
  
  cat("There are", length(sig), "or", 100*length(sig)/rowdim, "% residuals significant at the", sigLevel, "% level.\n")
  cat("There are", length(aFDR), "or", 100*length(aFDR)/rowdim, "% residuals significant at the", approxFDR, "adjusted average approximate FDR.")
  return(list(significant = sig, approxFDR = aFDR))
}