#' @title Covar projection analyses wrapper
#'
#' @description Wrapper function for a covar projection analyses
#'
#' @param data a \code{dispRity} object containing a covar component
#' @param type either \code{"within"} for the projection of the group onto itself or \code{"between"} for the projections of the groups onto the \code{base} (see details)
#' @param sample optional, one or more specific posterior sample IDs (is ignored if n is used) or a function to summarise all axes.
#' @param n optional, a random number of covariance matrices to sample (if left empty, all are used).
#' @param base optional, if \code{type = "between"}, a \code{character} string for the name of the subset in data to use as a baseline (e.g. the phylogeny) or if \code{type = "within"} the major axis to use as a base (see details).
#' @param major.axis which major axis to use (default is \code{1})
#' @param level the confidence interval to estimate the major axis (default is \code{0.95})
#' @param measure which parameters to measure from the projection. Can be any of the following \code{"c(position, distance, degree)"} (default) for respectively the distance on the projection, the distance from the projection and the angle of the projection.
#' @param verbose Whether to be verbose (\code{TRUE}) or not (\code{FALSE}, default).
#' @param dispRity.out Whether to make the output into a list of dispRity objects (\code{TRUE}) or not (\code{FALSE}, default).
#' 
#' @details
#' Effectively, the wrapper runs either of the following function (simplified here):
#' \itemize{
#'      \item if \code{type = "between"}: \code{dispRity(data, metric = projections.covar, between.groups = TRUE)} for the pairwise projections between each subset in \code{data}.
#'      \item if \code{type = "within"}: \code{dispRity(data, metric = projections, point1 = axis[1,], point2 = axis[1,])} for the projections between each subset in \code{data} onto an axis. 
#' }
#' If \code{base} is specified:
#' \itemize{
#'      \item \code{type = "between"} will run pairs between each subset and \code{base} (instead of the full pairwise analyses).
#'      \item \code{type = "within"} will run the projection of each subset onto the major axis from \code{base} rather than its own.
#' }
#' 
#' @returnssss
#' A list 
#' 
#' @examples
#'
#' @seealso \code{\link[dispRity]{projections}}  \code{\link[dispRity]{axis.covar}}  \code{\link[dispRity]{dispRity}}
#' 
#' @author Thomas Guillerme
#' @export

covar.projections.wrapper <- function(data, type, sample, n, base, average, major.axis = 1, level = 0.95, measure = c("position", "distance", "degree"), verbose = FALSE, dispRity.out = FALSE) {

    ## Check class data (dispRity)
 
    ## Check method type (c("between", "within"))

    ## base (optional)

    ## average (optional)

    ## Check method measure = c("position", "distance", "degree")

    ## 1 - get major axis
    if(verbose) message("calculating the major axis:...")
    major_axes <- axis.covar(data, sample = sample, n = n, axis = major.axis, level = level)
    # major_axes <- axis.covar(data)
    if(verbose) message("Done.")

    ## 2 - get the data
    if(!missing(n) || !missing(sample)) {
        data$MCMCglmm$covars <- get.covar(data, n = n, sample = sample)
    }

    ## A - Type between:
    if(type == "between") {
        wrap.dispRity.between <- function(measure, data, list_of_pairs, verbose) {

            if(verbose) {
                message("PLACEHOLDER:...")

                results <- dispRity.covar(data, metric = projections.covar, between.groups = list_of_pairs, measure = measure)$disparity

                message("Done.")
                return(results)
            } 

            ## Use normal dispRity here
            # dispRity.covar(data, metric = projections.covar, between.groups = list_of_pairs, measure = measure, dimensions = data$call$dimensions, point1 = axes[1, ], point2 = axes[2, ])$disparity
            return(dispRity.covar(data, metric = projections.covar, between.groups = list_of_pairs, measure = measure)$disparity)
        }

        ## 2 - get the groups
        if(missing(base)) {
            list_of_pairs <- unlist(apply(combn(1:n.subsets(data), 2), 2, list), recursive = FALSE)
        } else {
            base_id <- which(names(size.subsets(data)) == base)
            list_of_pairs <- lapply(as.list((1:n.subsets(data))[-base_id]), function(x,y) c(x, y), y = base_id)
        }

        ## Get all results
        results <- lapply(as.list(measure), wrap.dispRity.between, data, list_of_pairs, verbose)

        ## Remove the elements part
        results <- lapply(results, lapply, function(x) {x$elements <- NULL; return(matrix(unlist(x), nrow = 1))})
        names(results) <- measure
    }

    ## B - Type within
    if(type == "within") {
        ## Wrapper for within function
        wrap.dispRity.within <- function(measure, data, axes, verbose) {
            ## Calculate the measure
            if(verbose) message(".")
            results <- lapply(axes, function(axes, data) dispRity::dispRity(data, dimensions = data$call$dimensions, metric = dispRity::projections, point1 = axes[1, ], point2 = axes[2, ])$disparity, data = data)
            ## Combine all the results together
            results <- lapply(1:length(results[[1]]), function(u) do.call(cbind, lapply(results, `[[`, u)))
            return(lapply(results, function(res) do.call(cbind, res)))
        }
        
        ## FAST VERSION
        wrap.dispRity.within2 <- function(data, measure, axes, verbose) {
            ## Calculate the measure
            if(verbose) message(".")

            ## Generating the function for fast disparity
            make.fun <- function(measure, axes) {
                return(function(matrix) projections.fast(matrix, point1 = axes[1,], point2 = axes[2,], measure = measure))
            }
            lapply.axes <- function(axis, measure, group, space) {
                metric <- make.fun(measure, axis)
                return(dispRity.fast(group, space, metric))
            }
            lapply.group <- function(group, axes, measure, space) {
                return(lapply(axes, lapply.axes, measure, group, space))
            }
            clean.results <- function(results, measure) {
                output <- lapply(1:length(results[[1]]), function(u) do.call(cbind, lapply(results, `[[`, u)))
                names(output) <- one_measure
                return(output)
            }

            ## Get the groups
            groups <- lapply(data$subsets, function(subset, data) return(1:nrow(data$matrix[[1]]) %in% subset$element), data = data)
            space <- data$matrix[[1]][, data$call$dimensions]

            ## Run the fast function
            if(length(groups) == 1) {
                results <- lapply(axes, lapply.axes, measure, groups[[1]], space)
                results_matrices <- clean.results(results, measure)
            } else {
                results <- lapply(groups, lapply.group, axes, measure, space)
                results_matrices <- lapply(results, clean.results, measure)
            }
            return(results_matrices)
        }





        ## If base
        if(!missing(base)) {
            ## Select the non-base subsets
            base_id <- which(names(size.subsets(data)) == base)
            non_base_id <-  which(names(size.subsets(data)) != base)

            ## Run the measurements for all the groups on the base
            if(verbose) message("PLACEHOLDER:")
            results <- lapply(measure, wrap.dispRity.within, data = get.subsets(data, non_base_id), axes = major_axes[[base_id]], verbose)


            ## FAST VERSION: Something like that?
            # results <- wrap.dispRity.within2(data = get.subsets(data, non_base_id), measure = measure, axes = major_axes[[base_id]], verbose = verbose)
            


            if(verbose) message("Done.")
        
        } else {
        
            ## Run the measurements for each group on itself
            if(verbose) message("PLACEHOLDER:")
            non_base_id <- 1:n.subsets(data)
            results <- list()
            for(one_subset in 1:n.subsets(data)) {
                if(verbose) message("...")
                results[[one_subset]] <- lapply(measure, wrap.dispRity.within, data = get.subsets(data, one_subset), axes = major_axes[[one_subset]], verbose = FALSE)
                ## FAST VERSION: Something like that?
                # results <- wrap.dispRity.within2(data = get.subsets(data, one_subset), measure = measure, axes = axes = major_axes[[one_subset]], verbose = verbose)

            }
            if(verbose) message("Done.")

            ## Standardise the results
            reorder.results <- function(one_measure, results) {return(lapply(results, function(x, one_measure) return(x[[one_measure]][[1]]),    one_measure = 1))}
            results <- lapply(as.list(1:length(measure)), reorder.results, results = results)
        }

        ## Standardising the output
        names(results) <- measure
        results <- lapply(results, function(x, names) {names(x) <- names; return(x)}, names = names(size.subsets(data))[non_base_id])
    }

    if(dispRity.out) {
        ## Select the data subset for correct disparity display
        if(type == "between") {
            sub_data <- get.subsets(data, subsets = unique(unlist(list_of_pairs)))
        } else {
            sub_data <- get.subsets(data, subsets = non_base_id)
        }
        ## Make into a dispRity object
        output <- list()
        for(one_measure in 1:length(measure)) {
            output[[one_measure]] <- dispRitize(results[[one_measure]], sub_data,
                                               name = measure[[one_measure]],
                                               fun = ifelse(type == "between", projections.covar, dispRity::projections),
                                               type = type)
        }
        names(output) <- measure
        return(output)
    } else {
        ## Raw results
        return(results)
    }
}