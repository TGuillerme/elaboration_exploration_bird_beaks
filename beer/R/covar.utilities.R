#' @name get.covar
#' @aliases axes.covar ellipse.covar
#' @title Utilities for a dispRity object with covariance matrices
#'
#' @description Different utility functions to extract aspects of a \code{MCMCglmm} object.
#'
#' @usage get.covar(data, n)
#' @usage axis.covar(data, n, level = 0.95, axis = 1)
#'
#' @param data a \code{dispRity} object with a \code{covar} element.
#' @param sample optional, one or more specific posterior sample IDs (is ignored if n is used).
#' @param n optional, a random number of covariance matrices to sample (if left empty, all are used).
#' @param level which confidence interval level to use (default is \code{0.95}).
#' @param axis which major axis to calculate (default is \code{1}, the first one).

#' 
#' @examples
#'
#' @seealso
#' 
#' @author Thomas Guillerme
#' @export
get.covar <- function(data, sample, n) {

    ## Some sanitizing on data, sample, n

    ## Just return everything!
    if(missing(sample) && missing(n)) {
        return(sample.n(data$MCMCglmm$covars, n = length(data$MCMCglmm$covars[[1]])))
    }

    ## Just return the n random samples
    if(!missing(n)) {
        if(!missing(sample)) {
            warning("sample argument is ignored since n = ", n, " random samples are asked for.")
        }
        return(sample.n(data$MCMCglmm$covars, n = n))
    }

    ## Return specific samples
    if(!missing(sample)) {
        return(sample.n(data$MCMCglmm$covars, selected_n = sample))
    }
}

axis.covar <- function(data, sample, n, dimensions, level = 0.95, axis = 1) {
    
    ## Get the covar matrices
    selected_covars <- get.covar(data, sample, n)
    
    ## Sanitizing on level and axis

    ## Selecting the dimensions (all by default)
    if(missing(dimensions)) {
        dimensions <- data$call$dimensions
    }
    ## Select all the axis
    return(lapply(selected_covars, lapply, get.one.axis, axis = 1, level = level, dimensions = dimensions))
}

