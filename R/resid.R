resid <- function(response, dataExp, biglmObj, nameChar, new = T)  { 
  require(biglm)
  require(bigmemory)
  
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
  
  cat("Computing and writing\n")
  biglmResidCpp(dataExp@address, response@address, name@address, coef(biglmObj)) 
  return(name)
}