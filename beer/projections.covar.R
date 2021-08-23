#' @title level 1 between group metric for dispRity (projections wrapper for covar - MCMCglmm objects)
#'
#' @description 
#'
#' @param 
#' 
#' @examples
#'
#' @seealso
#' 
#' @author Thomas Guillerme
#' @export

## Make a between groups metric
projections.covar <- function(matrix, matrix2, measure = "position", scaled = TRUE, axis = 1, level = level) {

    ## Getting the corresponding vectors
    base_vector  <- axes_from_matrix2
    group_vector <- axes_from_matrix

    ## Translating into projections format
    matrix <- group_vector
    point1 <- base_vector[1,]
    point2 <- base_vector[2,]

    ## Measuring the projection
    return(projections(group_vector, point1 = base_vector[1,], point2 = base_vector[2,], measure = measure, scaled = scaled))
}