#' @name covar.ellipse.test
#' @description Testing the ellipse difference between one (x) and a reference one (y)
#'
#' @param x, y a named list of VCV (list(VCV = ..., ...)). y is used as the reference level
#' @param major.axis which major axis to use
#' @param p.value.method which method for calculating the p value (either "Bootstrap" for calculating it as in The Bootstrap, "Gavin" for using gavin's method or "Bayesian" for calculating it as a probability)
#' @param measure whether to measure the angles in "degrees" or in "orthogonality" (0 = parallel, 1 = orthogonal). 
#' @param verbose whether to be berbose or not.
covar.ellipse.test <- function(x, y, p.value.method = "Bayesian", measure = "orthogonality", major.axis = 1, verbose = FALSE) {
    ## Extracting the VCV matrices
    tested_VCV    <- lapply(x, `[[`, "VCV")
    reference_VCV <- lapply(y, `[[`, "VCV")

    if(verbose) cat(".")

    ## Calculate the angles for all samples (randomly sorted)
    options(warn = -1) # Remove warnings (acos(eigval^0.5) : NaNs produced)
    list_of_angles_within   <- mapply(MCMCglmm::krzanowski.test,
                                     tested_VCV[sample(length(tested_VCV))],
                                     tested_VCV[sample(length(tested_VCV))],
                                     MoreArgs = list(vecsA = major.axis, vecsB = major.axis),
                                     SIMPLIFY = FALSE)
    list_of_angles_between  <- mapply(MCMCglmm::krzanowski.test,
                                     tested_VCV[sample(length(tested_VCV))],
                                     reference_VCV[sample(length(reference_VCV))],
                                     MoreArgs = list(vecsA = major.axis, vecsB = major.axis),
                                     SIMPLIFY = FALSE)
    options(warn = 0)

    ## Extract all the angles
    angles_within  <- unlist(lapply(list_of_angles_within,  `[[`, "angles"))
    angles_between <- unlist(lapply(list_of_angles_between, `[[`, "angles"))

    ## Correcting for 0 angles
    angles_within  <- ifelse(is.nan(angles_within) ,  0, angles_within)
    angles_between <- ifelse(is.nan(angles_between),  0, angles_between)

    ## Convert into right angles
    if(measure == "orthogonality") {
        ## Transform the angles degrees into right angles proportions
        orthogonise <- function(angle) {
            ## Get the modulo
            if((angle > 90 && angle < 180) || (angle > 270 && angle < 360)) {
                ortho <- 90 - angle %% 90
            } else {
                ortho <- angle %% 90     
            }

            ## Convert the results
            if(ortho == 0 && (angle %in% c(0, 180, 360))) {
                return(0)
            }
            if(ortho == 0 && angle > 0) {
                return(1)
            }

            return(ortho/90)
        }
        angles_within  <- sapply(angles_within, orthogonise)
        angles_between <- sapply(angles_between, orthogonise)
    }

    ## Get the p value
    get.p.value <- switch(p.value.method,
        "Bayesian" = function(angles_within, angles_between) {
            ## All randomised posterior prob
            # quantile(replicate(100, sum(angles_within[sample(length(angles_within))] < angles_between[sample(length(angles_within))])/length(angles_within)), prob = c(0.5, 0.025, 0.975))
            ## Just randomised intervals
            c(sum(angles_within < angles_between)/length(angles_within), quantile(replicate(100, sum(angles_within[sample(length(angles_within))] < angles_between[sample(length(angles_within))])/length(angles_within)), prob = c(0.025, 0.975)))

        },
        "Bootstrap" = function(angles_within, angles_between) {
            return((sum(angles_within >= mean(angles_between)) + 1)/(length(angles_within) + 1))
        },
        "Gavin" = function(angles_within1, angles_within2, angles_between) {
            angle_statistic <- (angles_within1 + angles_within2) - (angles_between*2)
            return( 1 - sum(angle_statistic < 0) / length(angle_statistic))
        })
        
    if(p.value.method == "Gavin") {
        ## Calculate the angles for the reference set as well
        options(warn = -1) # Remove warnings (acos(eigval^0.5) : NaNs produced)
        list_of_angles_within2   <- mapply(MCMCglmm::krzanowski.test,
                                         reference_VCV[sample(length(reference_VCV))],
                                         reference_VCV[sample(length(reference_VCV))],
                                         MoreArgs = list(vecsA = major.axis, vecsB = major.axis),
                                         SIMPLIFY = FALSE)
        options(warn = 0)

        ## Extract/correct all the angles
        angles_within2  <- unlist(lapply(list_of_angles_within2,  `[[`, "angles"))
        angles_within2  <- ifelse(is.nan(angles_within2), 0, angles_within2)

        ## Convert into right angles
        if(measure == "orthogonality") {
            angles_within2  <- sapply(angles_within2, orthogonise)
        }
        angles_within1 <- angles_within

        ## Get the p.value
        return(get.p.value(angles_within1, angles_within2, angles_between))
    } else {
        ## Get the p-value
        return(get.p.value(angles_within, angles_between))
    }
    # get.p.value <- switch(alter,
    #     "two-sided" = function(random, observed, replicates) {
    #         ## Centring the randoms and observed
    #         center_random <- abs(random - mean(random))
    #         center_observed <- abs(mean(observed) - mean(random))
    #         ## Getting the p
    #         return((sum(center_random >= center_observed) + 1)/(replicates + 1))
    #     },
    #     "greater" = function(random, observed, replicates) {
    #         # Getting the p
    #         return((sum(random >= mean(observed)) + 1)/(replicates + 1))
    #     },
    #     "lesser" = function(random, observed, replicates) {
    #         # Getting the p
    #         return((sum(random <= mean(observed)) + 1)/(replicates + 1))
    #     })
}

#' @name run.ellipse.test 
#' @description Running the covar.ellipse.test on a dispRity object
#'
#' @param data the dispRity object
#' @param base the name of the reference posterior subset 
#' @param major.axis which major axis to use
#' @param p.value.method which method for calculating the p value (either "Bootstrap" for calculating it as in The Bootstrap, "Gavin" for using gavin's method or "Bayesian" for calculating it as a probability)
#' @param measure whether to measure the angles in "degrees" or in "orthogonality" (0 = parallel, 1 = orthogonal). 
#' @param verbose whether to be berbose or not.
run.ellipse.test <- function(data, base = "phylogeny", p.value.method = "Bayesian", measure = "orthogonality", major.axis = 1, verbose = TRUE) {
    ## Verbose results?
    if(verbose) cat("Testing the ellipses differences (from the base):")

    ## Setting up the base
    if(!(base %in% names(data$covar))) {
        ## By default set the last subset as the base
        base_name <- names(data$covar)[length((data$covar))]
    } else {
        base_name <- base
    }
    base <- data$covar[[base_name]]
    ## Setting up the other sets
    posteriors <- data$covar[names(data$covar) != base_name]
    
    ## Measuring everything
    all_results <- lapply(posteriors, covar.ellipse.test, y = base, p.value.method = p.value.method, measure = measure, major.axis = major.axis, verbose = verbose)

    if(verbose) cat("Done.\n")

    ## Return the results table
    return(do.call(rbind, all_results))
}

#' @name angles.base
#' @description measuring the angle from a VCV to the x axis
#' 
#' @param matrix a VCV
## Internal function from dispRity to get the coordinates of one axes
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
angles.base <- function(matrix, ...) {
    ## Measure the in a matrix (relative to the x axis)
    projected_vector <- get.one.axis(matrix, axis = 1, level = 0.95, dimensions = 1:length(diag(matrix)))

    ## Translating into projections format
    matrix <- projected_vector
    point1 <- rep(0, length(diag(matrix)))
    point2 <- c(1, rep(0, (length(diag(matrix))-1)))

    ## Measure the projection
    return(dispRity::projections(matrix, point1 = point1, point2 = point2, measure = "degree", scale = FALSE, centre = FALSE, abs = TRUE)[1]) 
}

#' @name ellipse.stats 
#' @description Wrapping function for measuring all the ellipses stats on the dispRity objects
#'
#' @param data the dispRity object
#' @param verbose whether to be berbose or not.
ellipse.stats <- function(data, verbose = TRUE) {
    ## Get the groups list
    groups_list <- unlist(apply(matrix(c(1:(n.subsets(data)-1), rep(n.subsets(data),(n.subsets(data)-1))), ncol = 2), 1, list), recursive = FALSE)

    ## Storing for the results
    results <- list()

    if(verbose) cat("Measuring distances:")
    results$distances  <- dispRity(data, metric = group.dist, probs = c(0.5), between.groups = groups_list, verbose = verbose)

    if(verbose) cat("Measuring sd:")
    results$sd  <- unlist(lapply(lapply(get.disparity(dispRity(data, metric = as.covar(angles.base), verbose = verbose), concatenate = FALSE), c), sd))

    if(verbose) cat("Measuring alignments:")
    results$alignments <- dispRity(data, metric = as.covar(disalignment, VCV = c(FALSE, TRUE), loc = c(TRUE, FALSE)), between.groups = groups_list, verbose = verbose)

    if(verbose) cat("Measuring orthogonality:")
    results$angles     <- dispRity(data, metric = as.covar(projections.between), measure = "orthogonality", between.groups = groups_list, verbose = verbose)
    if(verbose) cat("Testing orthogonality:")
    results$tests      <- run.ellipse.test(data)

    return(results)
}

#' @name table.stats 
#' @description Summarising the statistics into a table
#'
#' @param results the results output from ellipse.stats
table.stats <- function(results) {
    ## Table results

    ## Names, comparison and size
    header <- summary(results$distances)[,c(1,2), drop = FALSE]
    head_1 <- as.data.frame(do.call(rbind, strsplit(header[,1], split = ":")))
    header <- cbind(head_1, "n" = header[, 2])
    colnames(header)[c(1,2)] <- c("Group", "Comparison")

    ## Adding the statistics
    table <- cbind(header, "distance" = summary(results$distances)[,4])
    table <- cbind(table, "ellipse sd" = results$sd[-length(results$sd)])
    table <- cbind(table, summary(results$alignments, quantiles = c(95))[, -c(1:3)])
    colnames(table)[ncol(table)-2] <- "disalignment"
    table <- cbind(table, summary(results$angles, quantiles = c(95))[, -c(1:3)])
    colnames(table)[ncol(table)-2] <- "orthogonality"
    table <- cbind(table, results$test)
    colnames(table)[ncol(table)-2] <- "Post. prob."
    rownames(table) <- NULL
    return(table)
}

#' @name scale.xy 
#' @description Scales a density curve
#'
#' @param density a density curve
#' @param x,y logical, whether to scale the x and or the y axis
scale.xy <- function(density, x = TRUE, y = TRUE) {
    if(x) {
        density$x <- density$x/max(density$x)
    }
    if(y) {
        density$y <- density$y/max(density$y)
    }
    return(density)
}

#' @name plot.density 
#' @description Wrapper for plotting a density curve
#'
#' @param species the data per species
#' @param group the data per group
#' @param scale.x,scale.y logical, whether to scale the x and or the y axis
plot.density <- function(species, group, scale.x = TRUE, scale.y = TRUE, col = c("blue", "orange"), ...) {
    den_sp    <- scale.xy(density(species), x = scale.x, y = scale.y)
    den_group <- scale.xy(density(group), x = scale.x, y = scale.y)

    plot(NULL, xlim = range(c(den_sp$x, den_group$x)), ylim = range(c(den_sp$y, den_group$y)), ...)
    lines(den_sp, col = col[1])
    lines(den_group, col = col[2])
}

#' @name area.density
#' @description area under the curve for density
#'
#' @param data calculate the area under a density curve
#' @param scale.x,scale.y logical, whether to scale the x and or the y axis
area.density <- function(data, scale.x = TRUE, scale.y = TRUE) {
    density <- scale.xy(density(data), x = scale.x, y = scale.y)
    area_under_curve <- sum(diff(density$x[order(density$x)]) * zoo::rollmean(density$y[order(density$x)], 2))
}