standard <- function(response, dataExp, biglmObj, nameChar, new, chunksize, covMat, means)  {
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
  
  cat("Computing and writing\n")
  biglmStandardCpp(dataExp@address, response@address, name@address, coef(biglmObj), means, covInv)
  return(name)
}