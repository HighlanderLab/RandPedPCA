


#' Add downsampling index to rppca object
#'
#' This index is used by plot.rppca to downsample the col (colour) values. It
#' is stored in the rppca object's ds slot.
#'
#' @param pc an object of class rppca
#' @param to The down-sampling parameter. A numeric > 0 or a vector or NA. Interpreted
#' as a proportion or integer or a index vector, see details.
#'
#' @details
#' The parameter \code{to} is used to specify and possibly which individuals are sampled.
#' If NA, all individuals are retained. If \code{to} is of length one and is between 0 and 1,
#' then it is interpreted as a proportion. If it is greater than 1, it is taken to be
#' the number of individuals to be sampled (possibly rounded by \code{sample.int}). If
#' \code{to} is a logical or an integer vector, it is used for logical or integer indexing, respectively.
#' The integer indices of the sample individuals are written to the \code{ds} slot.
#' If \code{ds} exists, it is overwritten with a warning.
#'
#' @return An (invisible) object of class \code{rppca} with a slot \code{ds} added.
#'
#' @export
dspc <- function(pc, to=10000){
  stopifnot(inherits(pc, "rppca"))

  nr <- nrow(pc$x)


  if(length(to)==1){ # If 'to' is a scalar
    if(is.na(to)){ # "sample" all
      pc$ds <- 1:nr
      return(invisible(pc))
    }

    stopifnot(to > 0)
    if(to <1) n <- ceiling(nr * to) else n <- to
    if( n < nr){
      n <- min(c(n, nr))
      message(paste0("Downsampling to ", n, " individuals."))
      ind <- sample.int(nr, n)
    } else {
      ind <- 1:nr
    }
    if(!is.null(pc$ds)) warning("The existing downsampling slot was overwritten.")
    pc$ds <- ind
    return(invisible(pc))
  } else { # If 'to' is a vector
    pc$ds <- (1:nr)[to]
    message(paste0("Downsampling to ", length(pc$ds), " individuals."))
    return(invisible(pc))
  }

  stop("Should never reach this point.")
}
