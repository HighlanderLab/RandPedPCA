



#' Generate range matrix for SVD
#'
#' @param L a pedigree's L inverse matrix in sparse 'spam' format
#' @param rank  An \code{integer}, how many principal components to return
#' @param depth \code{integer}, number of iterations for generating the range matrix
#' @param numVectors An \code{integer > rank}, to specify the oversampling for the
#' @param cent \code{logical} whether or not to (implicitly) 'centre' the additive
#' relationship matrix, or more precisely, its underlying 'data matrix' L
#'
#' @return The range matrix for \code{randSVD}
#' @export
#'
#' @importFrom spam backsolve
#' @importFrom spam forwardsolve
#' @importFrom stats rnorm
randRangeFinder <- function(L, rank, depth, numVectors, cent=F){
  dim <- nrow(L)
  testVectors <- rnorm(n = dim * numVectors)
  testMatrix <- matrix(testVectors, nrow = dim, ncol = numVectors)
  Q <- testMatrix
  for (i in 1:depth){
    qrObject <- base::qr(Q)
    Q <- qr.Q(qrObject)
    if(cent) Q <- apply(Q, 2, function(col) col - mean(col))
    Q <- spam::backsolve(t(L),Q)
    Q <- spam::forwardsolve(L,Q)
    if(cent)  Q <- apply(Q, 2, function(col) col - mean(col))
  }
  qrObject <- qr(Q)
  Q <- qr.Q(qrObject)
  return(Q[,1:rank])
}

#' Singular value decomposition in sparse triangular matrix
#'
#' Uses randomised linear algebra, see Halko et al. (2010). Singular value
#' decomposition (SVD) decomposes a matrix \eqn{X=U\Sigma W^T}
#'
#' @param L a pedigree's L inverse matrix in sparse 'spam' format
#' @param rank  An \code{integer}, how many principal components to return
#' @param depth \code{integer}, the number of iterations for generating the range matrix
#' @param numVectors An \code{integer > rank} to specify the oversampling for the
#' @param cent \code{logical}, whether or not to (implicitly) centre the additive
#' relationship matrix
#'
#' @return A list of three: \code{u} (=U), \code{d} (=Sigma), and \code{v} (=W^T)
#' @export
#'
#' @importFrom spam backsolve
randSVD <- function(L, rank, depth, numVectors, cent=F){
  # L: lower cholesky factor of animal matrix
  # rank: number of PCs
  # depth: power iteration, higher -> more accurate approximation, ~5 is usually sufficient
  # numVectors: usually rank + 5~10, must be larger than rank
  dim <- nrow(L)
  Q <- randRangeFinder(L, rank, depth, numVectors, cent=cent)
  if(cent) Q <- apply(Q, 2, function(col) col - mean(col))
  C <- t(backsolve(t(L), Q))
  svdObject <- svd(C)
  U <- Q %*% svdObject$u
  D <- svdObject$d
  V <- svdObject$v
  return(list(u=U[,1:rank], d=D[1:rank], v=V[1:rank,]))
}

#' Compute the number of vectors to use for Hutchinson trace estimation
#'
#' Follows Skorski, M. (2021). Modern Analysis of Hutchinson’s Trace Estimator.
#' 2021 55th Annual Conference on Information Sciences and Systems (CISS), 1–5.
#' https://doi.org/10.1109/CISS50987.2021.9400306
#'
#' @param e A \code{numeric} denoting the relative error margin
#' @param d A \code{numeric}. 1-d is the probability the the relative error is bounded
#' by e.
#'
#' @return a scalar
getNumVectorsHutchinson <- function(e, d){
  2*(2+8*sqrt(2)/3*e)*log(2/d)/e^2
}


#' Trace estimation for sparse L inverse matrices
#'
#' Using Hutchinson's method
#'
#' @param L A pedigree's L inverse matrix
#' @param numVectors, an \code{integer} specifying how many random vectors to use
#'
#' If you do not have a good reason to do otherwise, use the function \code{hutchpp} instead.
#'
#' The higher \code{numVectors}, the higher the accuracy and the longer the running time.
#' Accuracy can be estimated with the function \code{getNumVectorsHutchinson}.
#'
#' @return a scalar
randTraceHutchinson <- function(L, numVectors){
  dim <- nrow(L)
  testVectors <- rnorm(n = dim * numVectors)
  testMatrix <- matrix(testVectors, nrow = dim, ncol = numVectors)
  testMatrixColNorms <- apply(testMatrix, 2, function(col) sqrt(sum(col^2)))
  normTestMatrix <- sweep(testMatrix, 2, testMatrixColNorms, FUN="/") * sqrt(dim)

  Y <- spam::backsolve(t(L), normTestMatrix)
  Y <- spam::forwardsolve(L, Y)
  Ests <- colSums(Y * normTestMatrix)
  return(mean(Ests))
}

#' Fast pedigree PCA using sparse matrices and randomised linear algebra
#'
#' @param pdg A representation of a pedigree, see Details.
#' @param method \code{string} only randSVD (the default) is implemented
#' @param rank  \code{integer} how many principal components to return
#' @param depth \code{integer} number of iterations for generating the range matrix
#' @param numVectors \code{integer > rank} to specify the oversampling for the
#' range matrix
#' @param totVar \code{scalar} (optional) the total variance, required for
#' computation of variance proportions when using an L-inverse matrix a input
#' @param center \code{logical} whether or not to (implicitly) centre the additive
#' relationship matrix
#' @param ... optional arguments passed to methods
#'
#' @details
#' The output slots are named like those of R's built in \code{prcomp} function.
#' Rotation is not returned by default as it is the transpose of the PC scores,
#' which are returned in \code{x}. \code{scale} and \code{center} are set to \code{FALSE}.
#'
#' @returns
#' A \code{list} containing:
#' \describe{
#'  \item{\code{x}}{the principal components}
#'  \item{\code{sdev}}{the variance components of each PC. Note that the total variance is
#'   not known per se and this these components cannot be used to compute the
#'   proportion of the total variance accounted for by each PC. However, if
#'   \code{nVecTraceEst} is specified, \code{rppca} will estimate the total variance and
#'   return variance proportions.}
#'  \item{\code{vProp}}{the estimated variance proportions accounted for by each PC.
#'   Only returned if \code{totVar} is set.}
#' \item{\code{scale}}{always \code{FALSE}}
#' \item{\code{center}}{\code{logical} indicating whether or not the implicit data matrix was centred}
#' \item{\code{rotation}}{the right singular values of the relationship matrix.
#'   Only returned if \code{returnRotation == TRUE}}
#' \item{\code{varProps}}{proportion of the total variance explained by each PC. Only
#'   returned if starting from a pedigree object without centring, or if \code{totVar} is supplied.
#'   }
#'   }
#' @export rppca
#' @rdname rppca
#'
#' @examples pc <- rppca(pedLInv)
#' ped <- pedigree(sire=pedMeta$fid,
#'                 dam=pedMeta$mid,
#'                 label=pedMeta$id
#'                 )
#' pc2 <- rppca(ped)
#'
rppca <- function(pdg, ...) UseMethod("rppca")



#' @rdname rppca
#' @method rppca spam
#' @export
rppca.spam <- function(pdg,
                  method="randSVD",
                  rank=10,
                  depth=3,
                  numVectors=15,
                  totVar=NULL,
                  center=F,
                  ...){
  #check L is the right kind of sparse matrix
  returnRotation=TRUE
  nn <- dim(pdg)[1]
  if(method=="randSVD"){
    rsvd = randSVD(pdg, rank=rank, depth=depth, numVectors=numVectors, cent=center)
    scores = rsvd$u %*% diag(rsvd$d^2)
    dimnames(scores) <- list(NULL, paste0("PC", 1:rank))

    stdv <- rsvd$d
    names(stdv) <- paste0("PC", 1:length(stdv))

    pc <- list(x= scores,
               sdev=stdv / sqrt(max(1, nn-1)),
               center=center,
               scale=FALSE
    )

    if(!missing(totVar)) {
      vp <- stdv^2/totVar
      names(vp) <- paste0("PC", 1:length(vp))
      pc$varProps <- vp
    }
    # return rotation only if requested
    if(returnRotation) pc$rotation <- t(pc$x)

    class(pc) <- "rppca"
    return(pc)

  } else {
    stop(paste0("Method ", method," not implemented"))
  }
}


#' @rdname rppca
#' @method rppca pedigree
#' @export
#' @importFrom pedigreeTools inbreeding getLInv
rppca.pedigree <- function(pdg,
                          method="randSVD",
                          rank=10,
                          depth=3,
                          numVectors=15,
                          totVar=NULL,
                          center=F,
                          ...){
  #check L is the right kind of sparse matrix
  returnRotation=TRUE

  # get Linv
  Lsp <- getLInv(pdg)
  L <- sparse2spam(Lsp)
  # get number of inds
  nn <- dim(L)[1]
  # total var is sum of (inbreeding coefs + 1)
  if(center==F) {
    if(!missing(totVar)){
      warning("Using specified value of ", totVar, " for the total variance
      instead of the value computed from the pedigree, which was ",
              sum(inbreeding(pdg) + 1))
  } else {
    totVar <- sum(inbreeding(pdg) + 1)
  }
  }


  # get inbreeding+1 ->total variance

  # return var components by default

  if(method=="randSVD"){
    rsvd = randSVD(L, rank=rank, depth=depth, numVectors=numVectors, cent=center)
    scores = rsvd$u %*% diag(rsvd$d^2)
    dimnames(scores) <- list(NULL, paste0("PC", 1:rank))
    stdv <- rsvd$d
    names(stdv) <- paste0("PC", 1:length(stdv))
    if(!is.null(totVar)){
      vp <- stdv^2/totVar
      names(vp) <- paste0("PC", 1:length(vp))
      pc <- list(x= scores,
                 sdev=stdv  / sqrt(max(1, nn-1)),
                 varProps=vp,
                 center=center,
                 scale=FALSE
      )
    } else {
      pc <- list(x= scores,
                 sdev=stdv  / sqrt(max(1, nn-1)),
                 center=center,
                 scale=FALSE
      )
    }


    # return rotation only if requested
    if(returnRotation) pc$rotation <- t(pc$x)

    class(pc) <- "rppca"
    return(pc)

  } else {
    stop(paste0("Method ", method," not implemented"))
  }
}


#' @method summary rppca
#' @export
summary.rppca <- function(object, ...){
  chkDots(...)
  if(is.null(object$varProps)){
    warning("Input does not contain information on variance components. Consider
running rppca on a pedigree object or supplying an estimate of the total
variance of the data.")
    importance <- rbind("Standard deviation" = object$sdev)
  } else {
    importance <- rbind("Standard deviation" = object$sdev,
                        "Proportion of Variance" = object$varProps,
                        "Cumulative proportion" = round(cumsum(object$varProps),
                                                        5)
                        )
  }

  colnames(importance) <- colnames(object$x)
  object$importance <- importance
  class(object) <- "summary.prcomp"
  object
}

