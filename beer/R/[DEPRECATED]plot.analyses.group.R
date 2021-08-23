# #' @title Plot analyses per group
# #'
# #' @description Plotting the results of a per group analyses
# #'
# #' @param results the results of an analyses per group (typically from \code{\link{analyses.group}}).
# #' @param what which results to plot. By default everything is plotted (e.g. \code{c("angle", "projection", "rejection")}).
# #' @param col a colour vector (for the levels).
# #' @param ... any plotting options to be passed to \code{\link{graphics}{boxplot}}

# #' 
# #' @examples
# #'
# #' @seealso
# #' 
# #' @author Thomas Guillerme
# #' @export
# plot.analyses.group <- function(results, what, col, ...) {
#     ## Get the graphical options
#     box_args <- plot.dots(...)

#     ## Missing colours
#     if(missing(col)) {
#         col <- grDevices::rainbow(length(results))
#     }
#     box_args$col <- col

#     ## Missing what
#     if(missing(what)) {
#         what <- colnames(results[[1]])
#     }

#     ## Transform the results into a table for boxplotting
#     table <- do.call(cbind, unlist(lapply(as.list(what), function(one_stat, results) return(lapply(results, function(X, one_stat) return(X[, one_stat]), one_stat)), results), recursive = FALSE))

#     ## Plot the results
#     par(mfrow = c(length(what),1))
#     for(one_stat in 1:length(what)) {
#         ## Setting the box arguments
#         one_box <- box_args
#         one_box$ylab <- what[one_stat]

#         ## Setting the box data
#         selected_data <- table[, 1:length(results) + (length(results) * (one_stat - 1))]
#         one_box$x <- selected_data

#         ## Removing the labels
#         if(one_stat != length(what)) {
#             one_box$xaxt <- "n"
#         }
#         ## Removing the title
#         if(one_stat != 1) {
#             one_box$main <- NULL
#         }

#         ## Plotting the box
#         do.call(boxplot, one_box)
#     }
#     return(invisible())
# }




