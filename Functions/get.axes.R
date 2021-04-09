#' @title Get ellipses axes
#'
#' @description Get the coordinates of any axes from an nD ellipse
#'
#' @param covar A covariance matrix describing the ellipse
#' @param axis Which axis to get the coordinates from (default is 1 for the major axis).
#' @param centre The centre of the ellipse (by default this is 0).
#' @param level The confidence interval (default it 0.95).
#' 
#' @examples
#'
#' @seealso
#' 
#' @author Thomas Guillerme
#' @export
# 
#
# DEBUG
# data <- model_phylo1_clade3
# n <- 1
# covar_matrices <- get.covar(data, n = n, simplify = FALSE)
# covar <- covar_matrices[1,][[1]]$VCV
get.one.axes <- function(covar, axis = 1, centre = 0, level = 0.95, dimensions) {

    # The magic: https://stackoverflow.com/questions/40300217/obtain-vertices-of-the-ellipse-on-an-ellipse-covariance-plot-created-by-care/40316331#40316331

    ## Select the right dimensions
    covar <- covar[dimensions, dimensions]

    ## Get the data dimensionality
    dims <- dim(covar)[1]

    ## Get the right size for the centre
    if((diff <- dims) - length(centre) != 0) {
        centre <- c(centre, rep(centre[1], abs(diff)-1))
    }
    
    ## Create the unit hypersphere (a hypersphere of radius 1) for the scaling
    unit_hypersphere1 <- unit_hypersphere2 <- matrix(0, ncol = dims, nrow = dims)
    ## The "front" (e.g. "top", "right") units
    diag(unit_hypersphere1) <- 1
    ## The "back" (e.g. "bottom", "left") units
    diag(unit_hypersphere2) <- -1
    unit_hypersphere <- rbind(unit_hypersphere1, unit_hypersphere2)
    ## Scale the hypersphere (where level is the confidence interval)
    unit_hypersphere <- unit_hypersphere * sqrt(qchisq(level, 2))

    ## Do the eigen decomposition (symmetric - faster)
    eigen_decomp <- eigen(covar, symmetric = TRUE)

    ## Re-scaling the unit hypersphere
    scaled_edges <- unit_hypersphere * rep(sqrt(eigen_decomp$values), each = dims*2)
    ## Rotating the edges coordinates
    edges <- tcrossprod(scaled_edges, eigen_decomp$vectors)

    ## Move the matrix around
    edges <- edges + rep(centre, each = dims*2)

    ## Get the longest axes with dist
    distances <- apply(edges, 1, dist)[1:dims]
    axis <- which(distances == sort(distances, decreasing = TRUE)[axis])

    # stop("DEBUG")
    # ## Weird bug with asp?
    # plot(ellipse(covar), type = "l", asp = 1)
    # points(edges)
    # lines(edges[c(1,3),], lty = 2)
    # lines(edges[c(2,4),], lty = 2)


    ## Get the edges coordinates
    return(edges[c(axis, axis+dims), ])
}




#TODO: document!
#' @param data see plot.ellipses
#' @param centre see plot.ellipses
get.axes <- function(data, centre, n = 1, dimensions) {
    if(is(data, "MCMCglmm")) {
        ## Get the covariance matrices
        covar_matrices <- get.covar(data, n = n, simplify = FALSE)
    } else {
        ## TODO: handle different input formats
        if(is(data, "list")) {
            ## Convert the list into a matrix
            stop("DEBUG: allow lists as input.")
        } else {
            if(is(data, "matrix")) {
                covar_matrices <- data
            } else {
                stop("data must be a MCMCglmm object, a list or a matrix of lists.")
            }
        }
        n <- dim(covar_matrices)[2]
    }

    ## Consider all dimensions by default
    if(missing(dimensions)) {
        dimensions <- 1:dim(covar_matrices[[1]][[1]])[1]
    }

    ## Get all the axes with different methods
    if(centre == "zero") {
        axes <- apply(covar_matrices, 1, lapply, function(x) get.one.axes(x$VCV, centre = 0, dimensions = dimensions))
    } 
    if(centre == "none") {
        axes <- apply(covar_matrices, 1, lapply, function(x) get.one.axes(x$VCV, centre = x$Sol[dimensions], dimensions = dimensions))
    }
    if(centre == "level") {
        ## Get the centre for each level
        level_centre <- t(apply(covar_matrices, 1, function(X) colMeans(do.call(rbind, lapply(X, `[[`, "Sol")))))
        ## Loop placeholder
        axes <- as.list(numeric(4))
        names(axes) <- rownames(covar_matrices)
        for(level in 1:nrow(covar_matrices)) {
           axes[[level]] <- lapply(covar_matrices[level,], function(x) get.one.axes(x$VCV, centre = level_centre[level,][dimensions], dimensions = dimensions))
       }
    }
    return(axes)
}

#TODO: generalise this function to plot.ellipses, plot.axes, plot.everything!
plot.all.axes <- function(axes, add = TRUE, dimensions = c(1,2), col, use.transparent = TRUE, ...) {

    ## Get the plot arguments
    plot_args <- list(x = NULL, y = NULL, ...)
    if(is.null(plot_args$xlab)) {
        plot_args$xlab  <- paste("Dim.", dimensions[1])
    }
    if(is.null(plot_args$ylab)) {
        plot_args$ylab  <- paste("Dim.", dimensions[2])
    }
    if(is.null(plot_args$lwd)) {
        plot_args$lwd <- 1
    }
    if(is.null(plot_args$lty)) {
        plot_args$lty <- 1
    }
    if(is.null(plot_args$xlim)) {
        plot_args$xlim <- range(unlist(lapply(axes, lapply, function(x, dimensions) {return(x[, dimensions])}, dimensions = dimensions)))
    }
    if(is.null(plot_args$ylim)) {
        plot_args$ylim <- range(unlist(lapply(axes, lapply, function(x, dimensions) {return(x[, dimensions])}, dimensions = dimensions)))
    }
    if(is.null(plot_args$main)) {
        plot_args$main <- "Major axes matrices"
    }

    ## Handle colours
    if(missing(col)) {
        all_cols <- rainbow(length(axes))
    } else {
        if(missing_col <- length(axes) - length(col) > 0) {
            all_cols <- rep(col, missing_col)
        } else {
            all_cols <- col
        }
    }
    if(use.transparent) {
        adjust <- 1/length(axes[[1]]) + 1/length(axes)
    } else {
        adjust <- 1
    }

    ## Empty plot
    if(!add) {
        empty_plot <- plot_args
        do.call(plot, empty_plot)
    }

    ## Plot the major axes
    line_args <- plot_args
    line_args$y <- NULL
    plot.one.line <- function(coords, args, dimensions) {
        args$x <- coords[, dimensions]
        do.call(lines, args)
    }

    ## Plot all the lines
    for(level in 1:length(axes)) {
        line_args$col <- adjustcolor(all_cols[level], alpha.f = adjust)
        lapply(axes[[level]], plot.one.line, args = line_args)
    }
    return(invisible())
}