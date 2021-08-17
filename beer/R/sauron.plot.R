#' @title sauron.plot
#'
#' @description plot a MCMCglmm.disparity object
#'
#' @param data an MCMCglmm dispRity object
#' @param n only selecting a random subset of the posteriors (if left missing, all the posteriors are used)
#' @param points logical, whether to plot the points (\code{TRUE}; default) or not (\code{FALSE}).
#' @param major.axes can be \code{NULL} or \code{FALSE} for no axes, \code{"all"} or \code{TRUE} for displaying the \code{n} posteriors or a \code{function} for summarising the major axes (e.g. \code{major.axes = mean}.
#' @param ellipses can be \code{NULL} or \code{FALSE} for no ellipses, \code{"all"} or \code{TRUE} for displaying the \code{n} posteriors or a \code{function} for summarising the ellipses (e.g. \code{ellipses = mean}.
#' @param level the confidence interval level of the major axes and ellipses (default is \code{0.95});
#' @param dimensions which dimensions (default is 1:2), could be 3D.
#' @param centres optional, a way to determine ellipses or major axes centres. Can be either a function (default is \code{colMeans}), a vector or a list of coordinates vectors or "intercept". See details.
#' @param transparent.scale if multiple major axes/ellipses are plotted, a scaling factor for the transparency. If left empty, the transparency is set to \code{1/n} or \code{0.1} (whichever is higher).
#' @param legend whether to add the automatic legend (\code{TRUE}) or not (\code{FALSE}; default).
#' @param legend.pos the coordinates of where to add the legend (if \code{legend = TRUE}). Can a numeric vector or a character string like \code{"topleft"} (default), \code{"bottomright"}, etc..
#' @param ... graphic parameters
#' 
#' @details
#' The argument \code{centres} allows to determine how to calculate the centre of each ellipses or major axes. The argument can be either:
#' \itemize{
#'      \item A function to calculate the centre from a group like the default \code{colMeans} function that calculates the centroid coordinates of each group;
#'      \item A numeric value to be replicated as the coordinates for the centre of each group (e.g. \code{centres = 0} sets all the centres at the coordinates \code{c(0,0,0,...)}); or a vector of numeric values to be directly used as the coordinates for each group (e.g. \code{centres = c(1,2,3)} sets all the centres at the coordinates \code{c(1,2,3)}); or a list of numeric values or numeric vectors to be used as the coordinates for the centres of each group;
#'      \item code{"intercept"} for using the estimated posterior intercept for each sample.
#' }
#' 
#' \emph{NOTE} that if the input contains more dimensions than the visualised dimensions (by default \code{dimensions = c(1,2)}) the ellipses and major axes are projected from an n-dimensional space onto a 2D space which might make them look "off".
#' \emph{NOTE} also that the ellipses and major axes are measured independently, when summarising both parameters (e.g. by using \code{ellipses = mean} and \code{major.axes = mean}), the displayed summarised major axes is not calculated from the summarised ellipse but from the coordinates of all major axes (and therefore might not match the coordinates of the ellipse).
#' 
#' @examples
#'
#' @seealso
#' 
#' @author Thomas Guillerme
#' @export

sauron.plot <- function(data, n, points = TRUE, major.axes = NULL, ellipses = NULL, level = 0.95, dimensions = c(1,2), centres = colMeans, transparent.scale, legend = FALSE, legend.pos = "topleft", ...) {

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

        ## recentre covar matrices
        covars <- mapply(recentre, covars, centres, MoreArgs = list(dimensions = dimensions), SIMPLIFY = FALSE)
    } else {
        ## Handled by ellipses and axes
        centres <- "intercept"
    }

    ## Translating the axes argument
    if(is(major.axes, "logical")) {
        if(major.axes) {
            major.axes <- "all"
        } else {
            major.axes <- NULL
        }
    }

    ## Measuring the axes
    if(!is.null(major.axes)) {
        ## The axes
        all_axes <- lapply(covars, lapply, get.one.axis, axis = 1, level = level, dimensions = dimensions)
        ## Summarising the axes (optional)
        if(is(major.axes, "standardGeneric") || is(major.axes, "function")) {
            ## Summarising the axes using the provided function
            all_axes <- lapply(all_axes, function(one_group, fun) list(apply(simplify2array(one_group), 1:2, fun)), fun = major.axes)
        }
    }

    ## Translating the ellipses argument
    if(is(ellipses, "logical")) {
        if(ellipses) {
            ellipses <- "all"
        } else {
            ellipses <- NULL
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

    ## Adjust the color
    if(missing(transparent.scale)) {
        trans_axes     <- ifelse(!is.null(major.axes), length(all_axes[[1]]), 1)
        trans_ellipses <- ifelse(!is.null(ellipses), length(all_ellipses[[1]]), 1)
        trans_axes <- ifelse(3/trans_axes < 0.1, 0.1, 3/trans_axes) 
        trans_ellipses <- ifelse(3/trans_ellipses < 0.1, 0.1, 3/trans_ellipses)
    } else {
        trans_axes <- trans_ellipses <- transparent.scale
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
    if(is.null(plot_args$lty)) {
        plot_args$lty <- 1
    }
    if(length(plot_args$lty) < length(data$subsets)) {
        plot_args$lty <- rep(plot_args$lty, length(data$subsets))
    }
    ## Get the plot limits
    if(is.null(plot_args$xlim)) {
        ## Default plot size
        plot_args$xlim <- range(data$matrix[[1]])
        ## Adding axes
        if(!is.null(major.axes)) {
            plot_args$xlim <- range(c(plot_args$xlim, unlist(all_axes)))
        }
        ## Adding ellipses (and preserving isometry)
        if(!is.null(ellipses)) {
            plot_args$xlim <- max(c(plot_args$xlim, range(unlist(all_ellipses))))
            plot_args$xlim <- c(-plot_args$xlim, plot_args$xlim)
        }
    }
    if(is.null(plot_args$ylim)) {
        ## Default plot size
        plot_args$ylim <- range(data$matrix[[1]])
        ## Adding axes
        if(!is.null(major.axes)) {
            plot_args$ylim <- range(c(plot_args$ylim, unlist(all_axes)))
        }
        ## Adding ellipses (and preserving isometry)
        if(!is.null(ellipses)) {
            plot_args$ylim <- max(c(plot_args$ylim, range(unlist(all_ellipses))))
            plot_args$ylim <- c(-plot_args$ylim, plot_args$ylim)
        }
    }

    ## Setting the x/y labels
    percentage <- apply(data$matrix[[1]], 2, var)
    percentage <- paste0(round(percentage/sum(percentage)*100, 2), "%")
    if(!is.null(colnames(data$matrix[[1]]))) {
        column_names <- colnames(data$matrix[[1]])
    } else {
        column_names <- paste0("Dim.", 1:ncol(data$matrix[[1]]))
    }
    if(is.null(plot_args$xlab)) {
        plot_args$xlab <- paste0(column_names[dimensions[1]], " (", percentage[dimensions[1]], ")")
    }
    if(is.null(plot_args$ylab)) {
        plot_args$ylab <- paste0(column_names[dimensions[2]], " (", percentage[dimensions[2]], ")")
    }

    ## Plotting the background
    do.call(plot, plot_args)

    ## Adding the points
    if(points) {

        ## Select the groups worth plotting (i.e. ignore the global ones)
        if(length(data$subsets) > 1) {
            ## Select the groups to plot
            plot_groups <- which(size.subsets(data) != nrow(data$matrix[[1]]))
            ## Update the pch in plot_args
            plot_args$pch[-plot_groups] <- NA
        } else {
            plot_groups <- 1:length(data$subsets)
        }
        
        for(one_group in plot_groups) {
            ## Setting the points arguments
            points_args <- plot_args
            points_args$x <- data$matrix[[1]][c(data$subsets[[one_group]]$elements), dimensions[1]]
            points_args$y <- data$matrix[[1]][c(data$subsets[[one_group]]$elements), dimensions[2]]
            points_args$col <- plot_args$col[one_group]
            points_args$cex <- plot_args$cex[one_group]
            points_args$pch <- plot_args$pch[one_group]
            do.call(graphics::points, points_args)
        }
    }

    ## Adding the ellipses
    if(!is.null(ellipses)) {
        ## Add the ellipses
        line_args <- plot_args
        ## Looping through the groups
        for(one_group in 1:length(data$subsets)) {
            line_args$col <- adjustcolor(plot_args$col[one_group], alpha.f = trans_ellipses)
            #TODO: Add transparency
            lapply(all_ellipses[[one_group]], function(data, line_args) {line_args$x <- data ; do.call(lines, line_args)}, line_args)
        }
    }

    ## Adding the axes
    if(!is.null(major.axes)) {
        ## Plot the axes
        line_args <- plot_args
        for(one_group in 1:length(data$subsets)) {
            line_args$col <- adjustcolor(plot_args$col[one_group], alpha.f = trans_axes)
            #TODO: Add transparency
            lapply(all_axes[[one_group]], function(data, line_args) {line_args$x <- data ; do.call(lines, line_args)}, line_args)
        }
    }

    if(legend){
        legend(legend.pos, legend = names(data$subsets), col = plot_args$col, lty = plot_args$lty, pch = plot_args$pch)
    }

    return(invisible())
}

## Internal: recentring the covar matrices (changing their Sol)
recentre <- function(one_group, one_centre, dimensions) {
    recentre.Sol <- function(covar, centre, dim) {
        covar$Sol[dim] <- centre[dim]
        return(covar)
    }
    return(lapply(one_group, recentre.Sol, centre = one_centre, dim = dimensions))
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
