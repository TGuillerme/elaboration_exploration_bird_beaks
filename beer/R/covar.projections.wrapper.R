#' @title Covar projection analyses wrapper
#'
#' @description Wrapper function for a covar projection analyses
#'
#' @param data a \code{dispRity} object containing a covar component
#' @param type either \code{"within"} for the projection of the group onto itself or \code{"between"} for the projections of the groups onto the \code{base} (see details)
#' @param base optional, if \code{type = "between"}, a \code{character} string for the name of the subset in data to use as a baseline (e.g. the phylogeny) or if \code{type = "within"} the major axis to use as a base (see details).
#' @param average optional, if \code{type = "within"}, a the function for averaging the major axes (see details)
#' @param major.axis which major axis to use (default is \code{1})
#' @param level the confidence interval to estimate the major axis (default is \code{0.95})
#' @param measure which parameters to measure from the projection. Can be any of the following \code{"c(position, distance, degree)"} (default) for respectively the distance on the projection, the distance from the projection and the angle of the projection.
#' 
#' @details
#' Effectively, the wrapper runs either of the following function (simplified here):
#' \itemize{
#'      \item if \code{type = "between"}: \code{dispRity(data, metric = projections, between.groups = TRUE)} for the pairwise projections between each subset in \code{data}.
#'      \item if \code{type = "within"}: \code{dispRity(data, metric = projections, point1 = axis[1,], point2 = axis[1,])} for the projections between each subset in \code{data} onto an axis. 
#' }
#' If \code{base} is specified:
#' \itemize{
#'      \item \code{type = "between"} will run pairs between each subset and \code{base} (instead of the full pairwise analyses).
#'      \item \code{type = "within"} will run the projection of each subset onto the major axis from \code{base} rather than its own.
#' }
#' 
#' @returnssss
#' A list 
#' 
#' @examples
#'
#' @seealso \code{\link[dispRity]{projections}}  \code{\link[dispRity]{axis.covar}}  \code{\link[dispRity]{dispRity}}
#' 
#' @author Thomas Guillerme
#' @export

covar.projections.wrapper <- function(data, type, base, average, major.axis = 1, level = 0.95, measure = c("position", "distance", "degree")) {

    ## Check class data (dispRity)
 
    ## Check method type (c("between", "within"))

    ## base (optional)

    ## average (optional)

    ## Check method measure = c("position", "distance", "degree")

    ## 1 - get major axis
## Get the mean axes for each level
major_axes <- axis.covar(covar_matrices, sample = mean)

    ## A - Type between:

## Projecting each group's major axes on the phylogenetic major axes
phy_group_elaboration <- dispRity(covar_matrices,
               dimensions = covar_matrices$call$dimensions,
               metric  = dispRity::projections,
               measure = "position",
               point1  = major_axes$phylogeny[[1]][1,],
               point2  = major_axes$phylogeny[[1]][2,])
phy_group_exploration <- dispRity(covar_matrices,
               dimensions = covar_matrices$call$dimensions,
               metric  = dispRity::projections,
               measure = "distance",
               point1  = major_axes$phylogeny[[1]][1,],
               point2  = major_axes$phylogeny[[1]][2,])
phy_group_angle <- dispRity(covar_matrices,
               dimensions = covar_matrices$call$dimensions,
               metric  = dispRity::projections,
               measure = "degree",
               point1  = major_axes$phylogeny[[1]][1,],
               point2  = major_axes$phylogeny[[1]][2,])

    ## B - Type within
## Projecting each group on its own major axis
gulls_elaboration <- dispRity(get.subsets(covar_matrices, "gulls"),
               dimensions = covar_matrices$call$dimensions,
               metric  = dispRity::projections,
               measure = "position",
               point1  = major_axes$gulls[[1]][1,],
               point2  = major_axes$gulls[[1]][2,])
plovers_elaboration <- dispRity(get.subsets(covar_matrices, "plovers"),
               dimensions = covar_matrices$call$dimensions,
               metric  = dispRity::projections,
               measure = "position",
               point1  = major_axes$plovers[[1]][1,],
               point2  = major_axes$plovers[[1]][2,])
sandpipers_elaboration <- dispRity(get.subsets(covar_matrices, "sandpipers"),
               dimensions = covar_matrices$call$dimensions,
               metric  = dispRity::projections,
               measure = "position",
               point1  = major_axes$sandpipers[[1]][1,],
               point2  = major_axes$sandpipers[[1]][2,])
    return()
}