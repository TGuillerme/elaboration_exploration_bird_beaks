dispRitize <- function(one_result, data, name, fun, type) {
    ## Copying the data
    output <- data
    ## Adding the disparity results
    output$disparity <- lapply(one_result, function(X) list(elements = X, X))
    ## Adding the call
    output$call$bootstrap <- list(ncol(one_result[[1]]), "covar", NULL)
    output$call$disparity$metrics$name[[1]] <- name
    output$call$disparity$metrics$fun[[1]]  <- fun
    output$call$disparity$metrics$between.groups <- (type == "between")
    return(output)
}

## Between type
# dispRity.covar(data, metric = projections.covar, between.groups = list_of_pairs, measure = measure)$disparity



## Run projections faster!


## Within type
# dispRity(data, dimensions = data$call$dimensions, metric = projections, point1 = axes[1, ], point2 = axes[2, ])$disparity
