#' @title Internal: plot.dots
#'
#' @description And ... arguments for plot functions
#'
#' @param data for x/y limits
#' @param dimensions for x/y labels
#' @param main_default for a main
#' @param dimensions.prefix for a prefix on the labels
#' 
#' @examples
#'
#' @seealso
#' 
#' @author Thomas Guillerme
#' @export

plot.dots <- function(data, dimensions, main_default, dimensions.prefix = "Dim.", ... ) {

    plot_args <- list(x = NULL, y = NULL, ...)
 
    if(is.null(plot_args$xlab) && !missing(dimensions)) {
        plot_args$xlab  <- paste(dimensions.prefix, dimensions[1])
    }
    if(is.null(plot_args$ylab) && !missing(dimensions)) {
        plot_args$ylab  <- paste(dimensions.prefix, dimensions[2])
    }
    if(is.null(plot_args$lwd)) {
        plot_args$lwd <- 1
    }
    if(is.null(plot_args$pch)) {
        plot_args$pch <- 19
    }
    if(is.null(plot_args$lty)) {
        plot_args$lty <- 1
    }
    if(is.null(plot_args$cex)) {
        plot_args$cex <- 0.5
    }
    if(is.null(plot_args$xlim) && !missing(data)) {
        plot_args$xlim <- range(unlist(data))
    }
    if(is.null(plot_args$ylim) && !missing(data)) {
        plot_args$ylim <- range(unlist(data))
    }
    if(is.null(plot_args$main) && !missing(main_default)) {
        plot_args$main <- main_default
    }
    return(plot_args)
}