#' @title sauron.plot
#'
#' @description plot a MCMCglmm.disparity object
#'
#' @param data an MCMCglmm dispRity object
#' @param n only selecting a random subset of the posteriors (if left missing, all the posteriors are used)
#' @param major.axes can be NULL = no axes; "all" = all axes; function = a function for summarising the axes;
#' @param ellipses can be NULL = no ellipses; "all" = all ellipses; function = a function for summarising the ellipses;
#' @param level the confidence interval level of the axes and ellipses (default is \code{0.95});
#' @param dimensions which dimensions (default is 1:2), could be 3D.
#' @param centres optional, a way to determine ellipses or axes centres. Can be either a function (default is \code{colMeans}), a vector or a list of coordinates vectors or "intercept". See details.
#' @param points logical, whether to plot the points (\code{TRUE}; default) or not (\code{FALSE}).
#' @param transparent.scale if multiple axes/ellipses are plotted, a scaling factor for the transparency. If left empty, the transparency is set to \code{1/n} or \code{0.1} (whichever is higher).
#' @param legend whether to add the automatic legend (\code{TRUE}) or not (\code{FALSE}; default).
#' @param legend.pos the coordinates of where to add the legend (if \code{legend = TRUE}). Can a numeric vector or a character string like \code{"topleft"} (default), \code{"bottomright"}, etc..
#' @param ... graphic parameters
#' 
#' @details
#' The argument \code{centres} allows to determine how to calculate the centre of each ellipses or axes. The argument can be either:
#' \itemize{
#'      \item A function to calculate the centre from a group like the default \code{colMeans} function that calculates the centroid coordinates of each group;
#'      \item A numeric value to be replicated as the coordinates for the centre of each group (e.g. \code{centres = 0} sets all the centres at the coordinates \code{c(0,0,0,...)}); or a vector of numeric values to be directly used as the coordinates for each group (e.g. \code{centres = c(1,2,3)} sets all the centres at the coordinates \code{c(1,2,3)}); or a list of numeric values or numeric vectors to be used as the coordinates for the centres of each group;
#'      \item code{"intercept"} for using the estimated posterior intercept for each sample.
#' }
#' @examples
#'
#' @seealso
#' 
#' @author Thomas Guillerme
#' @export

sauron.plot <- function(data, n, axes = NULL, ellipses = NULL, level = 0.95, dimensions = c(1,2), centres = colMeans, points = TRUE, transparent.scale = 0, legend = FALSE, legend.pos = "topleft", ...) {

    ## Some sanitizing to happen in dispRity on data

    ## Collecting the plot arguments
    plot_args <- list(x = NULL, ...)

    ## Selecting n
    #TODO: export as utility for MCMCglmm.dispRity
    sample.n <- function(covar, n) {
        ## Get the posterior size
        n_post <- length(covar[[1]])
        if(n == n_post) {
            return(covar)
        } else {
            ## Sample n
            selected_n <- sample(1:n_post, n, replace = n > n_post)
            ## Return n for each group
            return(lapply(covar, function(group, n) group[n], n = selected_n))
        }
    }
    if(missing(n)) {
        n <- length(data$MCMCglmm$covars[[1]])
    }
    covars <- sample.n(data$MCMCglmm$covars, n)
    ## Selecting the centres
    centre_class <- class(centres)[1]
    centre_class <- ifelse(centre_class == "standardGeneric", "function", centre_class)
    centre_class <- ifelse(centre_class == "integer", "numeric", centre_class)
    ## Adjusting the centres to match the dimensions
    adjust.centre <- function(centres, dim) {
        if(length(centres) < length(dim)) {
            return(rep(centres, length(dim))[dim])
        } else {
            return(centres[dim])
        }
    }

    if(centre_class != "character") {
        ## Get the centres
        centres <- switch(centre_class,
            "function" = lapply(unlist(data$subsets, recursive = FALSE) ,function(group, data, fun) fun(data[c(group), ]), data = data$matrix[[1]], fun = centres),
            "numeric"  = replicate(length(data$subsets), adjust.centre(centres, data$call$dimensions), simplify = FALSE),
            "list"     = lapply(centres, adjust.centre, dim = data$call$dimension))
        names(centres) <- names(data$subsets)
    } else {
        ## Handled by ellipses and axes
        centres <- "intercept"
    }

    ## Measuring the axes
    if(!is.null(axes)) {
        ## The axes
        ##TODO: improve that section!
        all_axes <- get.axes(covars, axis = 1, level = level, dimensions = data$call$dimensions, centre = centres)
        
        ## Summarising the axes (optional)
        if(is(axes, "standardGeneric") || is(axes, "function")) {
            ## Summarising the axes using the provided function
            all_axes <- lapply(all_axes, function(one_group, fun) list(apply(simplify2array(one_group), 1:2, fun)), fun = axes)
        }
    }

    ## Calculating the ellipses
    if(!is.null(ellipses)) {
        ## Get the ellipses
        all_ellipses <- lapply(covars, level.ellipses, dimensions, npoints = 50, centres)
        ## Summarising the ellipses (optional)
        if(is(ellipses, "standardGeneric") || is(ellipses, "function")) {
            ## Summarising the axes using the provided function
            all_ellipses <- lapply(all_ellipses, function(one_group, fun) list(apply(simplify2array(one_group), 1:2, fun)), fun = ellipses)
        }
    }

    ## Setting the plot parameters
    if(is.null(plot_args$col)) {
        plot_args$col <- "black"
    }
    if(length(plot_args$col) < length(data$subsets)) {
        plot_args$col <- rep(plot_args$col, length(data$subsets))
    }
    if(is.null(plot_args$pch)) {
        plot_args$pch <- 19
    }
    if(length(plot_args$pch) < length(data$subsets)) {
        plot_args$pch <- rep(plot_args$pch, length(data$subsets))
    }
    if(is.null(plot_args$cex)) {
        plot_args$cex <- 1
    }
    if(length(plot_args$cex) < length(data$subsets)) {
        plot_args$cex <- rep(plot_args$cex, length(data$subsets))
    }
    ## Get the plot limits
    if(is.null(plot_args$xlim)) {
        plot_args$xlim <- max(c(range(data$matrix[[1]]), range(unlist(all_axes))))
        plot_args$xlim <- c(-plot_args$xlim, plot_args$xlim)
    }
    if(is.null(plot_args$ylim)) {
        plot_args$ylim <- max(c(range(data$matrix[[1]]), range(unlist(all_axes))))
        plot_args$ylim <- c(-plot_args$ylim, plot_args$ylim)
    }

    ## Setting the labels
    percentage <- apply(data$matrix[[1]], 2, var)
    percentage <- paste0(round(percentage/sum(percentage)*100, 2), "%")
    if(is.null(plot_args$xlab)) {
        plot_args$xlab <- paste0(colnames(data$matrix[[1]])[dimensions[1]], " (", percentage[dimensions[1]], ")")
    }
    if(is.null(plot_args$ylab)) {
        plot_args$ylab <- paste0(colnames(data$matrix[[1]])[dimensions[2]], " (", percentage[dimensions[2]], ")")
    }

    ## Plotting the background
    do.call(plot, plot_args)

    ## Adding the points
    for(one_group in 1:length(data$subsets)) {
        ## Setting the points arguments
        points_args <- plot_args
        points_args$x <- data$matrix[[1]][c(data$subsets[[one_group]]$elements), dimensions[1]]
        points_args$y <- data$matrix[[1]][c(data$subsets[[one_group]]$elements), dimensions[2]]
        points_args$col <- plot_args$col[one_group]
        points_args$cex <- plot_args$cex[one_group]
        points_args$pch <- plot_args$pch[one_group]
        do.call(points, points_args)
    }

    ## Adding the ellipses
    if(!is.null(ellipses)) {
        ## Add the ellipses
        line_args <- plot_args
        ## Looping through the groups
        for(one_group in 1:length(data$subsets)) {
            line_args$col <- plot_args$col[one_group]
            #TODO: Add transparency
            lapply(all_ellipses[[one_group]], function(data, line_args) {line_args$x <- data ; do.call(lines, line_args)}, line_args)
        }
    }

    ## Adding the axes
    if(!is.null(axes)) {
        ## Plot the axes
        line_args <- plot_args
        for(one_group in 1:length(data$subsets)) {
            line_args$col <- plot_args$col[one_group]
            #TODO: Add transparency
            lapply(all_axes[[one_group]], function(data, line_args) {line_args$x <- data ; do.call(lines, line_args)}, line_args)
        }
    }

    return(invisible())
}

## Internal: making one ellipse
make.ellipse <- function(one_sample, dimensions, npoints){
    return(ellipse::ellipse(x       = one_sample$VCV[dimensions, dimensions],
                            centre  = one_sample$Sol[dimensions],
                            npoints = npoints))
}

## Internal: making a list of ellipses for the level
level.ellipses <- function(level_sample, dimensions, npoints, centre) {

    ## Recentreing the levels
    level_sample <- recentre.levels(level_sample, centre, dimensions)

    ## Get the ellipses for the level
    return(lapply(level_sample, make.ellipse, dimensions, npoints))
}
