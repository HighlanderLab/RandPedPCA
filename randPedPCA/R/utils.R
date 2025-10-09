
# Utility functions

# The oracle function that returns the value of A * G (but taking L^-1 as input)
#' @export
oraculumLiMatrixSolve <- function(Li, G, center=FALSE){
  if(center) {
    if(is.array(G)) G <- apply(G, 2, function(col) col - mean(col))
    if(is.vector(G)) G <- cbind(G) - mean(G)
  }
  #print(class(Li))
  Y <- Matrix::solve(Matrix::t(Li), G)
  Y <- Matrix::solve(Li, Y)
  if(center) {

    if(is.array(Y)) return(apply(Y, 2, function(col) col - mean(col)))
    if(is.vector(Y)) return(Y - mean(Y))
  } else {
    return(Y)
  }
}

oraculumLi <- function(Li, G, center=FALSE){
  if(center) {
    if(is.array(G)) G <- apply(G, 2, function(col) col - mean(col))
    if(is.vector(G)) G <- cbind(G) - mean(G)
  }
  #print(class(Li))
  Y <- spam::backsolve(t(Li), G)
  Y <- spam::forwardsolve(Li, Y)
  if(center) {

    if(is.array(Y)) return(apply(Y, 2, function(col) col - mean(col)))
    if(is.vector(Y)) return(Y - mean(Y))
  } else {
    return(Y)
  }
}


# Oracle function with swapped parameters as input to RSpectra::eigs
# oracFun <- function(x, args) oraculumLi(args$A, x, args$cent)
