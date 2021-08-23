# #' @title plot.space
# #'
# #' @description Plotting axes from an ellipse
# #'
# #' @param data a trait space.
# #' @param levels the levels (categories, eg. clades)
# #' @param dimensions which dimensions to plot (default is \code{c(1,2)}).
# #' @param col a colour vector.
# #' @param add logical, whether to add the lines to an existing plot (\code{TRUE}, default) or not (\code{FALSE}).
# #' @param scale.axes logical, whether to set both axes on the same scale (\code{TRUE}) or not (\code{FALSE}, default).
# #' @param ... any plotting options to be passed to \code{\link{graphics}{points}}
# #' 
# #' @examples
# #' data(morphdat)
# #' data <- morphdat[, -c(4,5)]
# #' plot.space(data, levels = morphdat$clade)
# #'
# #' @seealso
# #' 
# #' @author Thomas Guillerme
# #' @importFrom graphics lines
# #' @export

# plot.space <- function(data, levels, dimensions = c(1,2), col, add = FALSE, scale.axes = FALSE, ...) {

#     ## Get the plotting arguments
#     if(!scale.axes) {
#         plot_args <- plot.dots(dimensions = c(1,2), ...)
#     } else {
#         plot_args <- plot.dots(data = data,dimensions = c(1,2), ...)
#     }
    
#     if(missing(col)) {
#         ## Set the default colours
#         if(missing(levels)) {
#             col <- "black"

#         } else {
#             col <- grDevices::rainbow(length(unique(levels)))
#         }
#     }

#     ## Assigning the colour (by levels or not)
#     plot_args$col <- col
#     if(!missing(levels)) {
#         if(length(col) != length(levels)) {
#             plot_args$col <- col[as.factor(levels)]
#         }
#     }

#     ## Adding the data
#     plot_args$x <- data[, dimensions[1]]
#     plot_args$y <- data[, dimensions[2]]

#     ## Plotting the results
#     if(!add) {
#         do.call(plot, plot_args)
#     } else {
#         do.call(points, plot_args)
#     }
#     return(invisible())
# }
