#' @title Analyses per tips
#'
#' @description Get results per tip
#'
#' @param data the data set to make the projection/rejections
#' @param axes the main axes to on which to project the data or a list of main axes corresponding to the groups (if group is not missing)
#' @param group optional, a list of groups to run the analyses within groups
#' 
#' 
#' @examples
#'
#' @seealso
#' 
#' @author Thomas Guillerme
#' @export
analyses.tip <- function(data, axes, group) {

    ## Global analyses
    if(missing(group)) {
        ## Scale the data to the same number of dimensions than in the axes
        dimensions <- dim(axes[[1]])[2]
        if(dim(data)[2] > dimensions) {
            data <- data[1:dimensions]
        }

        ## Run the analyses
        results <- lapply(axes, lapply.projections, data)
        projections <- do.call(cbind, lapply(results, `[[`, "projection"))
        rejections <- do.call(cbind, lapply(results, `[[`, "rejection"))
        if(!is.null(rownames(data))) {
            rownames(projections) <- rownames(rejections) <- rownames(data)
        }

    } else {
        if(length(group) !=  length(axes)) {
            stop(paste0(length(group), " groups are used as a factor but ", length(axes), " axes groups are provided."))
        }
        ## Analyses per groups
        dimensions <- dim(axes[[1]][[1]])[2]
        ## Split the data
        data <- lapply(group, function(x, data) return(data[x, ]), data)

        ## Calculating the results per axes
        results <- mapply(mapply.projections, axes, data, SIMPLIFY = FALSE)

        ## Combining the results
        projections <- do.call(rbind, lapply(lapply(results, lapply, `[[`, "projection"), function(x) do.call(cbind, x)))
        rejections <- do.call(rbind, lapply(lapply(results, lapply, `[[`, "rejection"), function(x) do.call(cbind, x)))
        rownames(projections) <- rownames(rejections) <- unlist(group)
    }

    return(list(projections = projections, rejections = rejections))
}

## Wrapping function
lapply.projections <- function(one_axis, data) {
    return(list(
        projection = dispRity::projections(as.matrix(data), point1 = one_axis[1,], point2 = one_axis[2,]),
        rejection  = dispRity::projections(as.matrix(data), point1 = one_axis[1,], point2 = one_axis[2,], measure = "distance")))
}
mapply.projections <- function(axes, data) {
    return(lapply(axes, lapply.projections, data))
}