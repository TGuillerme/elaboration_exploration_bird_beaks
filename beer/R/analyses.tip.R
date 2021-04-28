#' @title Analyses per tips
#'
#' @description Get results per tip
#'
#' @param data the data set to make the projection/rejections
#' @param axes the main axes to on which to project the data
#' 
#' 
#' @examples
#'
#' @seealso
#' 
#' @author Thomas Guillerme
#' @export
analyses.tip <- function(data, axes) {
    ## Scale the data to the same number of dimensions than in the axes
    dimensions <- dim(axes[[1]])[2]
    if(dim(data)[2] > dimensions) {
        data <- data[1:dimensions]
    }

    ## Run the analyses
    results <- lapply(axes, lapply.projections, data)
    projections <- lapply(results, `[[`, "projection")
    rejections <- lapply(results, `[[`, "rejection")
    return(list(projections = projections, rejections = rejections))
}

## Wrapping function
lapply.projections <- function(one_axis, data) {
    return(list(
        projection = dispRity::projections(as.matrix(data), point1 = one_axis[1,], point2 = one_axis[2,]),
        rejection  = dispRity::projections(as.matrix(data), point1 = one_axis[1,], point2 = one_axis[2,], measure = "distance")))
}