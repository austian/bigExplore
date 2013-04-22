#' @title Covariance matrix for big.matrix objects
#' 
#' @description Computes the covariance matrix of a big.matrix
#' 
#' @details If the big.matrix has less than or equal to 10,000,000 rows, the covariance is computed using the standard R cov 
#' function.  If the big.matrix has greater than 10,000,000 rows, an updating algorithm is used to combine the 
#' covariance matrices of submatrices having number of rows equal to chunksize.
#' 
#' @param bigMat big.matrix
#' @param chunksize number of rows to use in the submatrices
#' @return covariance matrix
#' @author Alex Ustian <alex.l.ustian@@gmail.com>
#' @export
#' @examples
#' #Create a matrix with 10,000,005 rows and 2 columns, regard it also as a big.matrix
#' require(bigmemory)
#' A <- matrix(rnorm(20000010), 10000005, 2)
#' bigA <- as.big.matrix(A)
#' 
#' #Compute covariance matrices using standard R and this bigCov function, and check equality
#' covA <- cov(A)
#' bigCovA <- bigCov(A)
#' all.equal(covA, bigCovA, check.attributes = FALSE)    

bigCov <- function(bigMat, chunksize = 10000000)  {
  rowdim <- dim(bigMat)[1]
  coldim <- dim(bigMat)[2]
  
  if(rowdim <= 10000000)  {
  return(cov(as.matrix(bigMat[,])))
  }
  
  nloopBlocks <- floor(rowdim/chunksize) - 1
  remainder <- rowdim %% chunksize
  
  oldCov <- cov(as.matrix(bigMat[1:chunksize,]))
  oldMeans <- .colMeans(bigMat[1:chunksize,], chunksize, coldim)
  meansMat <- matrix(nrow = coldim, ncol = coldim)
  
  if(nloopBlocks > 0)  {
    for(i in 1:(nloopBlocks))  {
      currentMeans <- .colMeans(bigMat[(1 + i*chunksize):((i + 1)*chunksize),], chunksize, coldim)
      for(k in 1:coldim)  {
        for(j in k:coldim)  {
          meansMat[k,j] <- (currentMeans[k] - oldMeans[k])*(currentMeans[j] - oldMeans[j])
          meansMat[j,k] <- meansMat[k,j]
        }
      }
      currentCov <- cov(as.matrix(bigMat[(1 + i*chunksize):((i + 1)*chunksize),]))
      updatedCov <- ((chunksize*i - 1) / (chunksize*(i + 1) - 1))*oldCov + ((chunksize - 1) / (chunksize*(i + 1) - 1))*currentCov + ((chunksize*chunksize*i)/(chunksize*(i + 1)*(chunksize*(i + 1) - 1)))*meansMat
      oldMeans <- (i*chunksize*oldMeans + chunksize*currentMeans) / ((i + 1)*chunksize)
      oldCov <- updatedCov
    }
  }
  
  if(remainder != 0)  {
    lastCov <- cov(as.matrix(bigMat[((nloopBlocks+1)*chunksize + 1):((nloopBlocks+1)*chunksize + remainder),]))
    lastMeans <- .colMeans(bigMat[((nloopBlocks+1)*chunksize + 1):((nloopBlocks+1)*chunksize + remainder),], remainder, coldim)
    for(k in 1:coldim)  {
      for(j in k:coldim)  {
        meansMat[k,j] <- (lastMeans[k] - oldMeans[k])*(lastMeans[j] - oldMeans[j])
        meansMat[j,k] <- meansMat[k,j]
      }
    }
    finalCov <- ((rowdim - remainder - 1) / (rowdim - 1))*oldCov + ((remainder - 1) / (rowdim - 1))*lastCov + (((rowdim - remainder)*(remainder)) / ((rowdim)*(rowdim - 1)))*meansMat
    return(finalCov)
  }  else return(updatedCov)
}

