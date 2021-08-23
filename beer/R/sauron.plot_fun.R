## Internal: recentring the covar matrices (changing their Sol)
recentre <- function(one_group, one_centre, dimensions) {
    recentre.Sol <- function(covar, centre, dim) {
        covar$Sol[dim] <- centre[dim]
        return(covar)
    }
    return(lapply(one_group, recentre.Sol, centre = one_centre, dim = dimensions))
}

## Internal: making one ellipse
make.ellipse <- function(one_sample, dimensions, npoints){
    return(ellipse::ellipse(x       = one_sample$VCV[dimensions, dimensions],
                            centre  = one_sample$Sol[dimensions],
                            npoints = npoints))
}

## Internal: making a list of ellipses for the level
level.ellipses <- function(level_sample, dimensions, npoints, centre) {

    ## Recentreing the levels
    level_sample <- recentre.levels(level_sample, centre, dimensions)

    ## Get the ellipses for the level
    return(lapply(level_sample, make.ellipse, dimensions, npoints))
}

## Internal: changing the intercept ($Sol)
replace.intercept <- function(level_sample, value, dimensions) {
    lapply(level_sample, function(X) {X$Sol[dimensions] <- value[dimensions]; return(X)})
}


## Internal: changing the intercept wrapper
recentre.levels <- function(level_sample, centre, dimensions) {
    ## Centre the ellipse
    if(is(centre, "function")) {
        ## Get the central tendency (as a function)
        centre_values <- apply(do.call(rbind, lapply(level_sample, `[[`, "Sol")), 2, centre)
        ## Recentre the intercepts
        level_sample <- replace.intercept(level_sample, value = centre_values, dimensions)
    }
    if(is(centre, "numeric") || is(centre, "integer")) {
        if((diff <- length(level_sample[[1]]$Sol) - length(centre)) > 0) {
            centre <- c(centre, rep(centre, diff))
        }
        ## Manually recentre the intercepts
        level_sample <- replace.intercept(level_sample, value = centre, dimensions)
    }
    return(level_sample)
}