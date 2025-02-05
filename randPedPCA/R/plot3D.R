



#' 3D plot using rgl
#'
#' A simple wrapper around rgl's pot3d function.
#'
#' @param x, an rppca object
#'
#' @param dims, vector of length 3 - indices of the PCs to plot
#' @param ... additional arguments passed to rgl::plot3d
#'
#' @details
#' Note, different to `plot.rppca`, which is relatively slow, `plot3D` does
#' not down-sample the principal components and it ignores the `ds` slot of an
#' `rppca` object if present.
#'
#'
#' @export
plot3D <- function(x, dims=c(1,2,3), ...) {
  if (!requireNamespace("rgl", quietly = TRUE)) {
    stop(
      "Package \"rgl\" must be installed to use this function.",
      call. = FALSE
    )
  } else {
    ss <- summary(x)$importance
    if(dim(ss)[1] == 3){
      rgl::plot3d(
        x$x[,dims],
        xlab=paste0("PC", dims[1], " (", signif(100*ss[2,dims[1]], 3), "%)"),
        ylab=paste0("PC", dims[2], " (", signif(100*ss[2,dims[2]], 3), "%)"),
        zlab=paste0("PC", dims[3], " (", signif(100*ss[2,dims[3]], 3), "%)"),
        ...
      )
    } else {
      plot3d(
        x$x[,dims],
        ...
      )
    }
  }
}
