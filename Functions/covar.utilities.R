## Get the VCV central tendency
VCV.cent.tend <- function(one_covar, fun) {
    VCVs <- lapply(one_covar, `[[`, "VCV")
    locs <- lapply(one_covar, `[[`, "loc")
    return(list(VCV = apply(array(do.call(cbind, VCVs), dim = c(dim(VCVs[[1]]), length(VCVs))), c(1,2), fun),
                loc = apply(do.call(rbind, locs), 2, fun)))
}

## Selecting n elements from a covar object
sample.n <- function(covar, n, selected_n, dimensions = NULL) {
    ## Get the posterior size
    n_post <- length(covar[[1]])
    if(!missing(n) && n == n_post) {
        if(is.null(dimensions)) {
            ## Short cut
            return(covar)    
        } else {
            return(lapply(covar, lapply, redimension, dimensions = dimensions))
        }
    } else {
        ## Sample n
        if(missing(selected_n)) {
            selected_n <- sample.int(n_post, n, replace = n > n_post)
        }
        ## Return n for each group
        if(is.null(dimensions)) {
            return(lapply(covar, function(group, n) group[n], n = selected_n))
        } else {
            return(lapply(lapply(covar, function(group, n) group[n], n = selected_n), lapply, redimension, dimensions = dimensions))
        }
    }
}

## Internal: get the coordinates of one axes
get.one.axis <- function(data, axis = 1, level = 0.95, dimensions) {

    # The magic: https://stackoverflow.com/questions/40300217/obtain-vertices-of-the-ellipse-on-an-ellipse-covariance-plot-created-by-care/40316331#40316331

    ## VCVing the matrix
    if(!is(data, "list")) {
        data <- list(VCV = data)
    } else {
        if(is.null(data$VCV)) {
            data$VCV <- data
        }
    }
    ## adding a loc
    if(is.null(data$loc)) {
        data$loc <- rep(0, nrow(data$VCV))
    }

    ## Select the right dimensions
    data$VCV <- data$VCV[dimensions, dimensions, drop = FALSE]

    ## Get the data dimensionality
    dims <- length(diag(data$VCV))

    ## Create the unit hypersphere (a hypersphere of radius 1) for the scaling
    unit_hypersphere1 <- unit_hypersphere2 <- matrix(0, ncol = dims, nrow = dims)
    ## The "front" (e.g. "top", "right") units
    diag(unit_hypersphere1) <- 1
    ## The "back" (e.g. "bottom", "left") units
    diag(unit_hypersphere2) <- -1
    unit_hypersphere <- rbind(unit_hypersphere1, unit_hypersphere2)
    ## Scale the hypersphere (where level is the confidence interval)
    unit_hypersphere <- unit_hypersphere * sqrt(qchisq(level, dims))

    ## Do the eigen decomposition (symmetric - faster)
    eigen_decomp <- eigen(data$VCV, symmetric = TRUE)

    ## Re-scaling the unit hypersphere
    scaled_edges <- unit_hypersphere * rep(sqrt(abs(eigen_decomp$values)), each = dims*2)
    ## Rotating the edges coordinates
    edges <- tcrossprod(scaled_edges, eigen_decomp$vectors)

    ## Move the matrix around
    edges <- edges + rep(data$loc[dimensions, drop = FALSE], each = dims*2)

    ## Get the edges coordinates
    return(edges[c(axis, axis+dims), , drop = FALSE])
}