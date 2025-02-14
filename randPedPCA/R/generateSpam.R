


#' Generate spam object from L inverse file
#'
#' RandPedPCA relies in the \code{spam} onject format. But matrices are commonly
#' stored in other formats.
#'
#' @param pth path to matrix market file for L inverse matrix in dgTMatrix format
#'
#' @return A \code{spam} sparse matrix
#' @export
#' @importFrom methods as
#' @importFrom Matrix readMM
#'
importLinv <- function(pth){
  # spam's built-in read.MM is slow for large matrices
  # use Matrix::readMM instead

  dgT <- readMM(pth)

  sparse2spam(dgT)
}

#' Convert generic sparse matrix to spam format
#'
#' @param sprs A sparse matrix.
#'
#' @return A \code{spam} sparse matrix
#' @export
#' @importFrom methods as
#' @importFrom spam as.spam.dgCMatrix
sparse2spam <- function(sprs){
  if(inherits(sprs, "dtCMatrix")){ # the format returned by pedigreeTools::getLInc
    dgC <- as(sprs, "dgCMatrix") # conversion to spam has to go through dgCMatrix
  } else {
    dgC <- as(sprs, "CsparseMatrix")
  }

  return(as.spam.dgCMatrix(dgC))
}


