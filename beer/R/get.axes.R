#' @title get.axes
#'
#' @description Get the major axes from a list of covar matrices
#'
#' @param beer the \code{"beer"} object
#' @param axis which major axis to get (default is \code{1}: the major axis)
#' @param level the level of the confidence interval for the ellipse (default is \code{0.95}).
#' @param dimensions optional, a numeric vector of the dimensions to analyse (by default, all are used).
#' @param centre optional, the centre of the axes (see details)
#' 
#' @details
#' \code{centre} can be one of the following:
#' \itemize{
#'      \item \code{"intercept"} to use the estimated intercept is used (i.e. \code{MCMCglmm$Sol});
#'      \item a list of \code{numeric} coordinates to be applied to each level.
#'      \item a \code{function} to estimate the central tendency of the estimated intercepts (e.g. \code{mean})
#' }
#' 
#' 
#' @examples
#'
#' @seealso
#' 
#' @author Thomas Guillerme
#' @export
get.axes <- function(beer, axis = 1, level = 0.95, dimensions, centre = "intercept") {
    ## Consider all dimensions by default
    if(missing(dimensions)) {
        dimensions <- 1:dim(beer[[1]][[1]]$VCV)[1]
    }

    ## Recentreing the levels
    if(is(centre, "list")) {
        recentred_beer <- mapply(recentre.levels, beer, centre, MoreArgs = list(dimensions = dimensions), SIMPLIFY = FALSE)
    } else {
        recentred_beer <- lapply(beer, recentre.levels, centre, dimensions)
    }

    ## Get all the axes per level
    return(lapply(recentred_beer, lapply, get.one.axis, axis, level, dimensions))
}

## Internal: get the coordinates of one axes
get.one.axis <- function(data, axis = 1, level = 0.95, dimensions) {

    # The magic: https://stackoverflow.com/questions/40300217/obtain-vertices-of-the-ellipse-on-an-ellipse-covariance-plot-created-by-care/40316331#40316331

    ## Select the right dimensions
    data$VCV <- data$VCV[dimensions, dimensions]

    ## Get the data dimensionality
    dims <- dim(data$VCV)[1]

    ## Create the unit hypersphere (a hypersphere of radius 1) for the scaling
    unit_hypersphere1 <- unit_hypersphere2 <- matrix(0, ncol = dims, nrow = dims)
    ## The "front" (e.g. "top", "right") units
    diag(unit_hypersphere1) <- 1
    ## The "back" (e.g. "bottom", "left") units
    diag(unit_hypersphere2) <- -1
    unit_hypersphere <- rbind(unit_hypersphere1, unit_hypersphere2)
    ## Scale the hypersphere (where level is the confidence interval)
    unit_hypersphere <- unit_hypersphere * sqrt(qchisq(level, 2))

    ## Do the eigen decomposition (symmetric - faster)
    eigen_decomp <- eigen(data$VCV, symmetric = TRUE)

    ## Re-scaling the unit hypersphere
    scaled_edges <- unit_hypersphere * rep(sqrt(eigen_decomp$values), each = dims*2)
    ## Rotating the edges coordinates
    edges <- tcrossprod(scaled_edges, eigen_decomp$vectors)

    ## Move the matrix around
    edges <- edges + rep(data$Sol[dimensions], each = dims*2)

    ## Get the edges coordinates
    return(edges[c(axis, axis+dims), ])
}