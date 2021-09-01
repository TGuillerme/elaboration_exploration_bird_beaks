#To export in dispRity.metrics
#' @param ... the projections arguments
#' @param axis which major axes to use
#' @param level which confidence interval to use for calculating the major axis
#' 
#' 
projections.covar <- function(matrix, matrix2, measure = "position", scaled = TRUE, axis = 1, level = 0.95) {

    ## Get the main axes from the VCV matrices
    # source("covar.utilities_fun.R")
    base_vector  <- get.one.axis(matrix2, axis, level, dimensions = 1:nrow(matrix2$VCV))
    projected_vector <- get.one.axis(matrix, axis, level, dimensions = 1:nrow(matrix$VCV))

    ## Translating into projections format
    matrix <- projected_vector
    point1 <- base_vector[1,]
    point2 <- base_vector[2,]

    ## Moving the two axes so that there origins are the same:
    ## 1 - Get the translation vector to point1
    translation_vector <- point1 - matrix[1,]
 
    ## 2 - Align the matrix with point1
    matrix <- rbind(matrix[1,] + translation_vector,
                    matrix[2,] + translation_vector)

    ## Measure the projection
    return(projections(matrix, point1 = point1, point2 = point2, measure = measure, scaled = scaled)[2])
}


