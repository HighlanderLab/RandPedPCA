


#' @method plot rppca
#' @export
plot.rppca <- function(x, dims=c(1,2), to=10000, col=NULL, ...){
  ss <- summary(x)$importance # get variance components

  if(is.null(x$ds)) x <- dspc(x, to) # add index for downsampling (if not present)

  if(!is.null(col)) {
    if(length(col)>1){
      # warn if col has different length to number of individuals
      if(length(col) != nrow(x$x)) warning(paste0("The length of col was ", length(col), " but there were ", nrow(x$x), " individuals."))

      message("Downsampling colours.")
      cols <- col[x$ds] # downsample colours if any
    } else {
      cols <- col
    }
  }
  if(dim(ss)[1] == 3){ # add var proportions if any
    plot(
      x$x[x$ds,dims],
      xlab=paste0("PC", dims[1], " (", signif(100*ss[2,dims[1]], 3), "%)"),
      ylab=paste0("PC", dims[2], " (", signif(100*ss[2,dims[2]], 3), "%)"),
      col = cols,
      ...
    )
  } else {
    plot(
      x$x[x$ds,dims],
      col = cols,
      ...
    )
  }
}
