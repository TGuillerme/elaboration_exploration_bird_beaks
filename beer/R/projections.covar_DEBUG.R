# #To export in dispRity.metrics
# #' @param ... the projections arguments
# #' @param axis which major axes to use
# #' @param level which confidence interval to use for calculating the major axis
# #' 
# #' 

# stop("DEBUG projections.covar")
# matrix2 <- model_phylo$MCMCglmm$covars$phylogeny[[1]]
# matrix  <- model_phylo$MCMCglmm$covars$mainland[[1]]


# projections.covar <- function(matrix, matrix2, measure = "position", scaled = TRUE, axis = 1, level = 0.95) {

#    ## Get the main axes from the VCV matrices
#    base_vector  <- get.one.axis(matrix2, axis, level, dimensions = 1:nrow(matrix2$VCV))
#    projected_vector <- get.one.axis(matrix, axis, level, dimensions = 1:nrow(matrix$VCV))

#    warning("DEBUG: visual test projections.covar")
#    lims <- range(c(base_vector, projected_vector))
#    plot(NULL, type = "l", xlim = lims*3, ylim = lims*3)

#    ## Translating into projections format
#    matrix <- projected_vector
#    point1 <- base_vector[1,]
#    point2 <- base_vector[2,]
#    arrows(x0 = matrix[1, 1],
#           y0 = matrix[1, 2],
#           x1 = matrix[2, 1],
#           y1 = matrix[2, 2],
#           col = "grey", length = 0.1)
#    arrows(x0 = point1[1],
#           y0 = point1[2],
#           x1 = point2[1],
#           y1 = point2[2],
#           col = "orange", length = 0.1)


#    ## Moving the two axes so that there origins are the same:
#    ## 1 - Get the translation vector to point1
#    translation_vector <- point1 - matrix[1,]
#    arrows(x0 = 0,
#           y0 = 0,
#           x1 = translation_vector[1],
#           y1 = translation_vector[2],
#           col = "red", length = 0.1)

#    ## 2 - Align the matrix with point1
#    matrix <- rbind(matrix[1,] + translation_vector,
#                    matrix[2,] + translation_vector)
#    arrows(x0 = matrix[1,1],
#           y0 = matrix[1,2],
#           x1 = matrix[2,1],
#           y1 = matrix[2,2],
#           col = "blue", length = 0.1)

#    ## Measure the projection
#    ## Visualisation continued:
#    source("../tests/visualise.projections.R")
#    projections.debug(matrix[2,, drop = FALSE], point1 = point1, point2 = point2, measure = "distance", scaled = TRUE, plot.base = FALSE)

#    return(projections(matrix, point1 = point1, point2 = point2, measure = measure, scaled = scaled)[2])
# }