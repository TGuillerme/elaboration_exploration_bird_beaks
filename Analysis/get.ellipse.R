#' @title Get ellipse
#'
#' @description Get the coordinates on n ellipses out of an MCMCglmm object 
#'
#' @param data the MCMCglmm data
#' @param n the number of ellipses (default is 1)
#' @param levels which level to get the ellipses from
#' @param points the number of points to draw from the ellipse (default is 5)
#' @param centre
#' 
#' @examples
#' \dontrun{
#' ## Some MCMCglmm object
#' data <- ...
#' 
#' ## Get the coordinates of one ellipse (defining the major and minor axes)
#' ellipse_coords <- get.ellipse(data)
#' major_axis <- 
#' minor_axis
#' 
#' ## Get 100 random ellipses from the data (of 50 points each)
#' ellipses_coords <- get.ellipse(data, n = 100, points = 50)
#' 
#' ## Plot the first ellipse
#' plot(ellipses_coords[[1]], type = "l")
#' ## And the subsequent ones
#' do.call(lines, ellipses_coords[-1])
#' }
#'
#' @seealso
#' 
#' @author Thomas Guillerme, Gavin Thomas
#' @export

my_fun <- function() {

    return()
}