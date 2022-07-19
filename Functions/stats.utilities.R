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
            quantile(replicate(100, sum(angles_within[sample(length(angles_within))] < angles_between[sample(length(angles_within))])/length(angles_within)), prob = c(0.5, 0.025, 0.975))
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
    base <- data$covar[[base]]
    ## Setting up the other sets
    posteriors <- data$covar
    
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
angles.base <- function(matrix, ...) {
    ## Measure the in a matrix (relative to the x axis)
    projected_vector <- get.one.axis(matrix, axis = 1, level = 0.95, dimensions = 1:length(diag(matrix)))

    ## Translating into projections format
    matrix <- projected_vector
    point1 <- rep(0, length(diag(matrix)))
    point2 <- c(1, rep(0, (length(diag(matrix))-1)))

    ## Measure the projection
    return(projections(matrix, point1 = point1, point2 = point2, measure = "degree", scale = FALSE, centre = FALSE, abs = TRUE)[1]) 
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
    results$distances  <- unlist(lapply(lapply(get.disparity(dispRity(data, metric = as.covar(angles.base), verbose = verbose), concatenate = FALSE), c), sd))

    if(verbose) cat("Measuring alignments:")
    results$alignments <- dispRity(data, metric = as.covar(disalignment, VCV = c(FALSE, TRUE), loc = c(TRUE, FALSE)), between.groups = groups_list, verbose = verbose)

    if(verbose) cat("Measuring orthogonality:")
    results$angles     <- dispRity(data, metric = as.covar(projections.between), measure = "orthogonality", between.groups = groups_list, verbose = verbose)

    if(verbose) cat("Testing orthogonality:")
    results$tests      <- run.ellipse.test(data)

    return(results)
}