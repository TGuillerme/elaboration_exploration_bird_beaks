#' @title Use covar
#'
#' @description Adapts a dispRity.metric to use a covar element from a dispRity object
#'
#' @param metric The metric to adapt as a covar one.
#' 
#' @examples
#' data(BeckLee_mat50)
#' ## Using the centroids metric on a normal disparity object
#' dispRity(BeckLee_mat50, metric = centroids)
#' ## Same for the mean centroids
#' dispRity(BeckLee_mat50, metric = c(mean, centroids))
#' 
#' ## Using it on a dispRity object containing a covar part
#' dispRity(BeckLee_mat50, metric = as.covar(centroids))
#' ## Same for the mean centroids
#' dispRity(BeckLee_mat50, metric = c(mean, as.covar(centroids)))
#'
#' @seealso
#' 
#' @author Thomas Guillerme
#' @export

as.covar <- function(metric, ...) {
    if("matrix2" %in% names(formals(metric))) {
        return(function(matrix, matrix2, ..) metric(matrix = matrix$VCV, matrix2 = matrix2$VCV, ...))
    } else {
        return(function(matrix, ..) metric(matrix = matrix$VCV, ...))
    }
}