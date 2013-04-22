student <- function(response, dataExp, biglmObj, nameChar, new, chunksize, covMat, means, cutoff)  {
  require(biglm)
  require(bigmemory)
  require(biganalytics)
  
  rowdim <- biglmObj$n
  
  if(new == T)  {
    cat("Creating file-backed big.matrix in the working directory.\n")
    cat("Warning, this is a potentially large file being created on the hard drive.\n")
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
  biglmStudentCpp(dataExp@address, response@address, name@address, coef(biglmObj), means, covInv)
  
  if(cutoff == T)  {
    cat("Ideal conditions have ~5% of studentized residuals with absolute value >= 2\n")
    cutoffres2 <- mwhich(name, 1, 2, "ge")
    cutoffresneg2 <- mwhich(name, 1, -2, "le")
    cat("Percent of data outside this range:\n") 
    cat(length(cutoffres2) + length(cutoffresneg2), "entries out of", rowdim, "total\n")
    cat(100*((length(cutoffres2) + length(cutoffresneg2))/rowdim), "% of the data\n")
    cat("Use mwhich to identify these entries")
  }
  return(name)
}