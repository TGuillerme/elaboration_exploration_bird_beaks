#' @title Plot ellipses
#'
#' @description Plot n ellipses out of an MCMCglmm object 
#'
#' @param data the MCMCglmm data
#' @param n the number of ellipses (default is 1)
#' @param dimensions in which dimensions to plot the ellipses
#' @param levels which level to get the ellipses from
#' @param points the number of points to draw from the ellipse (default is 50)
#' @param centre
#' 
#' @examples
#' \dontrun{
#' ## Some MCMCglmm object
#' data <- ...
#' 
#' ## Get the coordinates of one ellipse (defining the major and minor axes)
#' ellipse_coords <- get.ellipse(data)
#' major_axis <- 
#' minor_axis
#' 
#' ## Get 100 random ellipses from the data (of 50 points each)
#' ellipses_coords <- get.ellipse(data, n = 100, points = 50)
#' 
#' ## Plot the first ellipse
#' plot(ellipses_coords[[1]], type = "l")
#' ## And the subsequent ones
#' do.call(lines, ellipses_coords[-1])
#' }
#'
#' @seealso
#' 
#' @author Thomas Guillerme, Gavin Thomas
#' @export

plot.ellipses <- function(data, n, dimensions = c(1,2), npoints = 50, col, use.transparent = TRUE, ...) {

    ## Get the covariance matrices
    covar_matrices <- get.covar(data, n = n, simplify = FALSE)

    ## Get all the ellipses
    all_ellipses <- apply(covar_matrices,1, get.all.ellipses, dimensions, npoints)

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
        plot_args$xlim <- range(unlist(all_ellipses))
    }
    if(is.null(plot_args$ylim)) {
        plot_args$ylim <- range(unlist(all_ellipses))
    }
    if(is.null(plot_args$main)) {
        plot_args$main <- "Correlation matrices"
    }

    ## Handle colours
    if(missing(col)) {
        all_cols <- rainbow(length(all_ellipses))
    } else {
        if(missing_col <- length(all_ellipses) - length(col) > 0) {
            all_cols <- rep(col, missing_col)
        } else {
            all_cols <- col
        }
    }
    if(use.transparent) {
        adjust <- 1/n + 1/length(all_ellipses)
    } else {
        adjust <- 1
    }

    ## Empty plot
    empty_plot <- plot_args
    do.call(plot, empty_plot)

    draw.one.ellipse <- function(data, col, args, adjust) {
        ## Set up the plotting arguments
        line_args <- args
        line_args$y <- NULL
        line_args$col <- adjustcolor(col, alpha.f = adjust)
        ## Set up the data
        line_args$x <- data
        ## Plot it
        do.call(lines, line_args)
    }

    ## Plot the different ellipses
    for(one_level in 1:length(all_ellipses)) {
        lapply(all_ellipses[[one_level]], draw.one.ellipse, col = all_cols[[one_level]], args = plot_args, adjust = adjust)
    }
    return(invisible())
}

## Internal
get.one.ellipse <- function(one_sample, dimensions, npoints){
    return(ellipse::ellipse(
        x = one_sample$VCV[dimensions, dimensions],
        centre = one_sample$Sol[dimensions],
        npoints = npoints))
}

get.all.ellipses <- function(one_row, dimensions, npoints) {
    return(lapply(one_row, get.one.ellipse, dimensions, npoints))
}