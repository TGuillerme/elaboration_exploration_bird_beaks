# #' @title sauron.plot
# #'
# #' @description plot a MCMCglmm.disparity object
# #'
# #' @param data use select.posterior for cleaningness
# #' @param subsample only select a random subsample of the whole data
# #' @param axes can be NULL = no axes; "all" = all axes; function = a function for summarising the axes;
# #' @param ellipses can be NULL = no ellipses; "all" = all ellipses; function = a function for summarising the ellipses;
# #' @param dimensions which dimensions (default is 1:2), can be 3D.
# #' @param centres The centres for each ellipses/axes (from plot.axes)
# #' @param points A list of matrices of data points to be added in the background (same length as data)
# #' @param transparent.scale if multiple axes/ellipses are plotted, a scaling factor for the transparency
# #' @param ... graphic parameters
# #' 
# #' @examples
# #'
# #' @seealso
# #' 
# #' @author Thomas Guillerme
# #' @export

# my_fun <- function() {

#     return()
# }


# ## Plotting it
# plot.space(data_matrix[,-1],
#            col = colour_vector,
#            levels = data_matrix$islands,
#            xlab = "PC1",
#            ylab = "PC2",
#            xlim = c(-4, 4),
#            ylim = c(-4, 4))
# points(do.call(rbind, group_centres)[,c(1,2)],
#        pch = 13, col = c("grey", colour_vector))
# # plot.axes(axes["animal"], col = "grey",
#           add = TRUE, transparent.scale = 0.1)
# plot.axes(axes["units:islands1"], col = colour_vector[1],
#           add = TRUE, transparent.scale = 0.1)
# plot.axes(axes["units:islands2"], col = colour_vector[2],
#           add = TRUE, transparent.scale = 0.1)
# legend("topleft",
#        legend = c("phylogeny", levels(data_matrix$islands)),
#        col = c("grey", colour_vector),
#        lty = c(1,1,1,1),
#        pch = c(NA, 19, 19, 19))