




#' Generate range matrix for SVD
#'
#' @param L a pedigree's L inverse matrix (or other expression if using a different oracle function)
#' @param rank  An \code{integer}, how many principal components to return
#' @param depth \code{integer}, number of iterations for generating the range matrix
#' @param numVectors An \code{integer > rank}, to specify the oversampling for the
#' @param cent \code{logical} whether or not to (implicitly) 'centre' the additive
#' relationship matrix, or more precisely, its underlying 'data matrix' L
#' @param oracleFun A function that computes Ax for a
#' given matrix
#'
#' @return The range matrix for \code{randSVD}
#' @export
#'
#' @importFrom spam backsolve
#' @importFrom spam forwardsolve
#' @importFrom stats rnorm
randRangeFinder <- function(L, rank, depth, numVectors, cent=FALSE, oracleFun){
  dim <- nrow(L)
  testVectors <- rnorm(n = dim * numVectors)
  testMatrix <- matrix(testVectors, nrow = dim, ncol = numVectors)
  Q <- testMatrix
  for (i in 1:depth){
    qrObject <- base::qr(Q)
    Q <- qr.Q(qrObject)
    Q <- oracleFun(L,Q, center=cent)
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
#' @param oracleFun A function that computes Ax for a
#' given matrix
#'
#' @return A list of three: \code{u} (=U), \code{d} (=Sigma), and \code{v} (=W^T)
#' @export
#'
#' @importFrom spam backsolve
randSVD <- function(L, rank, depth, numVectors, cent=FALSE, oracleFun){
  # L: lower cholesky factor of animal matrix
  # rank: number of PCs
  # depth: power iteration, higher -> more accurate approximation, ~5 is usually sufficient
  # numVectors: usually rank + 5~10, must be larger than rank
  dim <- nrow(L)
  Q <- randRangeFinder(L, rank, depth, numVectors, cent=cent, oracleFun=oracleFun)
  if(cent) Q <- apply(Q, 2, function(col) col - mean(col))
  if(class(L)=="spam"){
    C <- t(spam::backsolve(t(L), Q))
  } else {
    C <- t(Matrix::solve(t(L), Q))
  }
  svdObject <- svd(C)
  U <- Q %*% svdObject$u
  D <- svdObject$d
  V <- svdObject$v
  return(list(u=U[,1:rank], d=D[1:rank], v=V[1:rank,]))
}

#' Fast pedigree PCA using sparse matrices and randomised linear algebra
#'
#' @param X A representation of a pedigree, see Details.
#' @param method \code{string}, \code{"randSVD"} (the default) or \code{"rspec"} can be chosen, see Details
#' @param rank  \code{integer}, the number of principal components to return
#' @param depth \code{integer}, number of iterations for generating the range matrix
#' @param numVectors \code{integer > rank}, the number of random vectors to be
#' sampled when generating the range matrix, defaults to \code{ceiling(rank*1.5)}.
#' @param totVar \code{scalar}, (optional) the total variance, required for
#' computation of variance proportions when using an L-inverse matrix a input
#' @param center \code{logical}, whether or not to (implicitly) centre the additive
#' relationship matrix
#' @param ... optional arguments passed to methods
#'
#' @details
#' The output slots are named like those of R's built in \code{prcomp} function.
#' Rotation is not returned by default as it is the transpose of the PC scores,
#' which are returned in \code{x}. \code{scale} and \code{center} are set to \code{FALSE}.
#'
#' Which \code{method} performs better depends on the number of PC requested, whether
#' centring is applied, and on the structure of the pedigree. As a rule of thumb,
#' \code{"rspec"} is faster than the default when \code{rank} is 8 or greater.
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
rppca <- function(X, ...) UseMethod("rppca")



#' @rdname rppca
#' @method rppca spam
#' @export
rppca.spam <- function(X,
                       method="randSVD",
                       rank=10,
                       depth=3,
                       numVectors,
                       totVar=NULL,
                       center=FALSE,
                       oracleFun=oraculumLi,
                       ...){
  #check L is the right kind of sparse matrix
  returnRotation=TRUE
  if(missing(numVectors)) numVectors <- ceiling(rank * 1.5)
  nn <- dim(X)[1]
  if(method %in% c("randSVD", "rspec")){
    if(method=="randSVD"){
      # run randomised SVD as defined above
      rsvd = randSVD(X, rank=rank, depth=depth, numVectors=numVectors, cent=center, oracleFun=oracleFun)

      # extract/generate components of the return value
      scores = rsvd$u %*% diag(rsvd$d^2)


      stdv <- rsvd$d
    } else if(method=="rspec") {
      oracFun <- function(x, args) oracleFun(args$A, x, args$cent)
      eigdcp = eigs(oracFun, k=rank, n=nn, args=list(A=X, cent=center))
      scores = oracleFun(X, eigdcp$vectors)
      stdv <- sqrt(as.numeric(eigdcp$values))
    }
    dimnames(scores) <- list(NULL, paste0("PC", 1:rank))
    names(stdv) <- paste0("PC", 1:length(stdv))

    pc <- list(x= scores,
               sdev=stdv / sqrt(max(1, nn-1)),
               center=center,
               scale=FALSE
    )

    if(!missing(totVar)) {
      # check whether totVar as "center" attribute set
      if(!is.null(attr(totVar, "center"))){
        #is so, make sure its identical to the "center" argument supplied to rppca
        if(center != attr(totVar, "center")) {
          warning("rppca is run with center=", center, ", but the value of totVar
                  supplied has center=", attr(totVar, "center"))
        }
      }
      # compute variance proportions
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
#' @method rppca Matrix
#' @export
rppca.Matrix <- function(X,
                       method="randSVD",
                       rank=10,
                       depth=3,
                       numVectors,
                       totVar=NULL,
                       center=FALSE,
                       oracleFun=oraculumLi,
                       ...){
  #check L is the right kind of sparse matrix
  returnRotation=TRUE
  if(missing(numVectors)) numVectors <- ceiling(rank * 1.5)
  nn <- dim(X)[1]
  if(method %in% c("randSVD", "rspec")){
    if(method=="randSVD"){
      # run randomised SVD as defined above
      rsvd = randSVD(X, rank=rank, depth=depth, numVectors=numVectors, cent=center, oracleFun=oracleFun)

      # extract/generate components of the return value
      scores = rsvd$u %*% diag(rsvd$d^2)


      stdv <- rsvd$d
    } else if(method=="rspec") {
      oracFun <- function(x, args) oracleFun(args$A, x, args$cent)
      eigdcp = eigs(oracFun, k=rank, n=nn, args=list(A=X, cent=center))
      scores = oracleFun(X, eigdcp$vectors)
      stdv <- sqrt(as.numeric(eigdcp$values))
    }
    dimnames(scores) <- list(NULL, paste0("PC", 1:rank))
    names(stdv) <- paste0("PC", 1:length(stdv))

    pc <- list(x= scores,
               sdev=stdv / sqrt(max(1, nn-1)),
               center=center,
               scale=FALSE
    )

    if(!missing(totVar)) {
      # check whether totVar as "center" attribute set
      if(!is.null(attr(totVar, "center"))){
        #is so, make sure its identical to the "center" argument supplied to rppca
        if(center != attr(totVar, "center")) {
          warning("rppca is run with center=", center, ", but the value of totVar
                  supplied has center=", attr(totVar, "center"))
        }
      }
      # compute variance proportions
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
#' @method rppca CHMfactor
#' @export
rppca.CHMfactor <- function(X,
                         method="randSVD",
                         rank=10,
                         depth=3,
                         numVectors,
                         totVar=NULL,
                         center=FALSE,
                         oracleFun=oraculumLi,
                         ...){
  #check L is the right kind of sparse matrix
  returnRotation=TRUE
  if(missing(numVectors)) numVectors <- ceiling(rank * 1.5)
  nn <- dim(X)[1]
  if(method %in% c("randSVD", "rspec")){
    if(method=="randSVD"){
      # run randomised SVD as defined above
      rsvd = randSVD(X, rank=rank, depth=depth, numVectors=numVectors, cent=center, oracleFun=oracleFun)

      # extract/generate components of the return value
      scores = rsvd$u %*% diag(rsvd$d^2)


      stdv <- rsvd$d
    } else if(method=="rspec") {
      oracFun <- function(x, args) oracleFun(args$A, x, args$cent)
      eigdcp = eigs(oracFun, k=rank, n=nn, args=list(A=X, cent=center))
      scores = oracleFun(X, eigdcp$vectors)
      stdv <- sqrt(as.numeric(eigdcp$values))
    }
    dimnames(scores) <- list(NULL, paste0("PC", 1:rank))
    names(stdv) <- paste0("PC", 1:length(stdv))

    pc <- list(x= scores,
               sdev=stdv / sqrt(max(1, nn-1)),
               center=center,
               scale=FALSE
    )

    if(!missing(totVar)) {
      # check whether totVar as "center" attribute set
      if(!is.null(attr(totVar, "center"))){
        #is so, make sure its identical to the "center" argument supplied to rppca
        if(center != attr(totVar, "center")) {
          warning("rppca is run with center=", center, ", but the value of totVar
                  supplied has center=", attr(totVar, "center"))
        }
      }
      # compute variance proportions
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
#' @importFrom RSpectra eigs
rppca.pedigree <- function(X,
                           method="randSVD",
                           rank=10,
                           depth=3,
                           numVectors,
                           totVar=NULL,
                           center=FALSE,
                           oracleFun=oraculumLi,
                           ...){
  #TODO: add check that L is the right kind of sparse matrix
  returnRotation=TRUE
  if(missing(numVectors)) numVectors <- ceiling(rank * 1.5)

  # get Linv
  LIsp <- getLInv(X)
  LI <- sparse2spam(LIsp)

  # get number of individuals
  nn <- dim(LI)[1]

  # totVar of non-centred A
  tvnc <- sum(inbreeding(X) + 1)

  # total var is sum of (inbreeding coefs + 1) if not centred
  if(center==FALSE) {
    if(!missing(totVar)){
      warning("Using user-specified value of ", totVar, " for the total variance
      instead of the value computed from the pedigree, which was ",
              tvnc)
    } else { # if totVar was not specified
      totVar <- tvnc
    }
  }




  if(method %in% c("randSVD", "rspec")){

    if(method == "randSVD"){
      rsvd = randSVD(LI, rank=rank, depth=depth, numVectors=numVectors, cent=center, oracleFun=oracleFun)
      scores = rsvd$u %*% diag(rsvd$d^2)
      stdv <- rsvd$d

    } else { # rspec
      oracFun <- function(x, args) oracleFun(args$A, x, args$cent)
      eigdcp = eigs(oracFun, k=rank, n=nn, args=list(A=LI, cent=center))
      scores = oracleFun(LI, eigdcp$vectors)
      stdv <- sqrt(as.numeric(eigdcp$values))

    }
    dimnames(scores) <- list(NULL, paste0("PC", 1:rank))
    names(stdv) <- paste0("PC", 1:length(stdv))

    # the following is computed for both modes, randSVD and rspec

    if(is.null(totVar)){ # if there is no value of totVar (can only happen with center==TRUE), then estimate totVar
      #n <- dim(LI)[1]
      onesVec <- rep(1, nn)
      totVar <- tvnc - as.vector(1/nn * t(onesVec) %*% oracleFun(LI, t(t(onesVec))))
      attr(totVar, "center") <- TRUE
    }

    #
    if(!is.null(attr(totVar, "center"))){
      if(center != attr(totVar, "center")) {
        warning("rppca is run with center=", center, ", but the value of totVar
                  supplied has center=", attr(totVar, "center"))
      }
    }

    vp <- stdv^2/totVar
    names(vp) <- paste0("PC", 1:length(vp))

    pc <- list(x= scores,
               sdev=stdv  / sqrt(max(1, nn-1)),
               varProps=vp,
               center=center,
               scale=FALSE
    )


    # return rotation only if requested
    if(returnRotation) pc$rotation <- t(pc$x)

    class(pc) <- "rppca"
    return(pc)




  } else { # if method is no in c("randSVD", "rspec")
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

