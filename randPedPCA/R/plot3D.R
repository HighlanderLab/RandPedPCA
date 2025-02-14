



#' 3D plot using rgl
#'
#' A simple wrapper around rgl's pot3d function.
#'
#' @param x, an rppca object
#'
#' @param dims, vector of length 3 - indices of the PCs to plot
#' @param xlab (optional) x axis label
#' @param ylab (optional) yaxis label
#' @param zlab (optional) xz axis label
#' @param ... additional arguments passed to rgl::plot3d
#'
#' @details
#' Note, different to \code{plot.rppca}, which is relatively slow, \code{plot3D} does
#' not down-sample the principal components and it ignores the \code{ds} slot of an
#' \code{rppca} object if present.
#'
#'
#' @export
#' @examples
#' pc <- rppca(pedLInv)
#' plot3D(pc)
#'
#' ped <- pedigree(sire=pedMeta$fid, dam=pedMeta$mid, label=pedMeta$id)
#' pc2 <- rppca(ped)
#' plot3D(pc2)
plot3D <- function(x, dims=c(1,2,3), xlab=NULL, ylab=NULL, zlab=NULL, ...) {
  if (!requireNamespace("rgl", quietly = TRUE)) {
    stop(
      "Package \"rgl\" must be installed to use this function.",
      call. = FALSE
    )
  } else {
    # Summary throws a warning if there are no variance proportions
    # We'll suppress this as we handle both cases, with and without variance components.
    ss <- suppressWarnings(summary(x)$importance) # get variance components

    if(dim(ss)[1] == 3){
      rgl::plot3d(
        x$x[,dims],
        xlab=ifelse(is.null(xlab), paste0("PC", dims[1], " (", signif(100*ss[2,dims[1]], 3), "%)"), xlab),
        ylab=ifelse(is.null(ylab), paste0("PC", dims[2], " (", signif(100*ss[2,dims[2]], 3), "%)"), ylab),
        zlab=ifelse(is.null(zlab), paste0("PC", dims[3], " (", signif(100*ss[2,dims[3]], 3), "%)"), zlab),
        ...
      )
    } else {
      rgl::plot3d(
        x$x[,dims],
        xlab=ifelse(is.null(xlab), paste0("PC", dims[1]), xlab),
        ylab=ifelse(is.null(ylab), paste0("PC", dims[2]), ylab),
        zlab=ifelse(is.null(zlab), paste0("PC", dims[3]), zlab),
        ...
      )
    }
  }
}



# internal
makeProjList <- function(A, offsets, ff=0.1){
  x <- A[,1:3]
  if(missing(offsets)) {
    rr <- apply(A, 2, range)
    dd <- apply(rr, 2, diff)

    offsets <- c(max(rr[,1]) + dd[1] * ff,
                 min(rr[,2]) - dd[2] * ff,
                 min(rr[,3]) - dd[3] * ff)
  }
  xyProj = x[,1:3]
  xyProj[,3] <- offsets[3]
  xzProj = x[,1:3]
  xzProj[,2] <- offsets[2]
  yzProj = x[,1:3]
  yzProj[,1] <- offsets[1]
  list(xyProj,
       xzProj,
       yzProj)
}



#' 3D plot of PC scores with projections on coordinate planes
#'
#' @param pc An \code{rppca} object
#' @param dims \code{integer} \code{vector}, which PCs to plot, defauts to 1:3
#' @param plotProj \code{logical}, whether to plot the projections
#' @param grid \code{logical}, wheter to plot grids
#' @param col the dot colours, integer or string, scalar or vector
#' @param ff \code{numeric}, offset for projection (proportion of the orthogonal axis's range)
#' @param theta,phi polar coordinates in degrees. \code{theta} rotates round the
#' vertical axis. \code{phi} rotates round the horizontal axis.
#'
#' @return nothing
#' @export
#'
#' @examples
#' ped <- pedigree(pedMeta$fid,
#' pedMeta$mid,
#' pedMeta$id
#' )
#' pc <- rppca(ped)
#' plot3DWithProj(pc, col=as.numeric(factor(pedMeta$population)))
#' @export
plot3DWithProj <- function(pc,
                           dims=c(1,2,3),
                           plotProj=T,
                           grid=T,
                           col=1,
                           ff=0.5,
                           theta=-45,
                           phi=25){

  plot3D(pc, dims=dims, col=col, size=10)
  if(plotProj) {
    sh <- makeProjList(pc$x[,dims], ff=ff)
    rgl::points3d(sh[[1]], col=col, size=3)
    rgl::points3d(sh[[2]], col=col, size=3)
    rgl::points3d(sh[[3]], col=col, size=3)
  }
  if(grid){
    rgl::grid3d("x+")
    rgl::grid3d("y-")
    rgl::grid3d("z-")

  }
  rgl::view3d(theta=theta, phi=phi)
}
