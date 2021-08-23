# #' @title plot.ellipses
# #'
# #' @description Plotting axes from an ellipse
# #'
# #' @param beer the \code{"beer"} object.
# #' @param dimensions which dimensions to plot (default is \code{c(1,2)}).
# #' @param npoints the number of points per ellipses (for smoothing; default is \code{50}).
# #' @param centre optional, the centre of the ellipses (see details)
# #' @param col a colour vector.
# #' @param add logical, whether to add the lines to an existing plot (\code{TRUE}, default) or not (\code{FALSE}).
# #' @param scale.axes logical, whether to set both axes on the same scale (\code{TRUE}) or not (\code{FALSE}, default).
# #' @param transparent.scale A numerical value to fade the colours (1 = no fade, 0 = no colours).
# #' @param ... any plotting options to be passed to \code{\link{graphics}{lines}}
# #' 
# #' @details
# #' \code{centre} can be one of the following:
# #' \itemize{
# #'      \item \code{"intercept"} to use the estimated intercept is used (i.e. \code{MCMCglmm$Sol});
# #'      \item a \code{numeric} of one or 2 elements to be used as the x/y coordinates of the centre
# #'      \item a \code{function} to estimate the central tendency of the estimated intercepts (e.g. \code{mean})
# #' }
# #' 
# #' 
# #' @examples
# #'
# #' @seealso
# #' 
# #' @author Thomas Guillerme
# #' @export
# plot.ellipses <- function(beer, dimensions = c(1,2), npoints = 50, centre = "intercept", col, add = TRUE, transparent.scale = 1, ...) {
#     ## Get the ellipses
#     all_ellipses <- lapply(beer, level.ellipses, dimensions, npoints, centre)
    
#     ## Get the plot arguments
#     plot_args <- plot.dots(data = all_ellipses, dimensions = dimensions, ...)

#     ## Get the colours
#     if(missing(col)) {
#         col <- grDevices::rainbow(length(beer))
#     }
#     ## Scale the transparency
#     col <- grDevices::adjustcolor(col, alpha.f = transparent.scale)

#     ## Do the base plot
#     if(!add) {
#        do.call(plot, plot_args) 
#     }

#     ## Plot the ellipses
#     line_args <- plot_args
#     for(one_level in 1:length(beer)) {
#         line_args$col <- col[one_level]
#         lapply(all_ellipses[[one_level]], function(data, line_args) {line_args$x <- data ; do.call(lines, line_args)}, line_args)
#     }
# }

# ## Internal: making on ellipse
# make.ellipse <- function(one_sample, dimensions, npoints){
#     return(ellipse::ellipse(x       = one_sample$VCV[dimensions, dimensions],
#                             centre  = one_sample$Sol[dimensions],
#                             npoints = npoints))
# }

# ## Internal: making a list of ellipses for the level
# level.ellipses <- function(level_sample, dimensions, npoints, centre) {

#     ## Recentreing the levels
#     level_sample <- recentre.levels(level_sample, centre, dimensions)

#     ## Get the ellipses for the level
#     return(lapply(level_sample, make.ellipse, dimensions, npoints))
# }
