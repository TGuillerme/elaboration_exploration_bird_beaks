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
#' @param sample optional, one or more specific posterior sample IDs (is ignored if n is used) or a function to summarise all axes.
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
        if(is(sample, "function") || is(sample, "standardGeneric")) {
            ## Summarise the results
            return(lapply(lapply(sample.n(data$MCMCglmm$covars, n = length(data$MCMCglmm$covars[[1]])), summarise.fun, fun = sample), list))
        } else {
            ## Return specific samples
            return(sample.n(data$MCMCglmm$covars, selected_n = sample))
        }
    }
}

axis.covar <- function(data, sample, n, dimensions, level = 0.95, axis = 1) {
    
    ## Sanitizing on level and axis

    ## Selecting the dimensions (all by default)
    if(missing(dimensions)) {
        dimensions <- data$call$dimensions
    }

    ## Handle sample
    if(!missing(sample) && (is(sample, "function") || is(sample, "standardGeneric"))) {
        return(lapply(
                ## Get the axes from the selected covars
                    lapply(
                    ## Get the covar matrices
                    get.covar(data, n), lapply, get.one.axis, axis = axis, level = level, dimensions = dimensions),
                ## Get the mean of all the axes
                function(one_group, fun) list(apply(simplify2array(one_group), 1:2, fun)), fun = sample))
    }

    ## Get the covar matrices
    selected_covars <- get.covar(data, sample, n)

    ## Select all the axis
    return(lapply(selected_covars, lapply, get.one.axis, axis = axis, level = level, dimensions = dimensions))
}

