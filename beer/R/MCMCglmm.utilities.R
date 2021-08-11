#' @name MCMCglmm.utilities
#' @aliases MCMCglmm.traits MCMCglmm.levels
#' @title MCMCglmm object utility functions
#'
#' @description Different utility functions to extract aspects of a \code{MCMCglmm} object.
#'
#' @usage MCMCglmm.traits(MCMglmm, ...)
#' @usage MCMCglmm.levels(MCMglmm, ...)
#' @usage MCMCglmm.sample(MCMglmm, n, ...)
#' @usage MCMCglmm.covars(MCMglmm, sample, n, ...)
#'  
#' @param MCMCglmm A \code{MCMCglmm} object.
#' @param n        Optional, a number of random samples to extract.
#' @param sample   Optional, the specific samples to extract (is ignored if \code{n} is present).
#' @param ...      Optional arguments to be passed to the function..
#'
#' @details
#' \itemize{
#'      \item \code{MCMCglmm.levels} returns the different random and residual terms levels of a \code{MCMCglmm} object. This function uses the default option \code{convert = TRUE} to convert the names into something more readable. Toggle to \code{convert = FALSE} for the raw names.
#'      \item \code{MCMCglmm.traits} returns the column names of the different traits of a \code{MCMCglmm} formula object.
#'      \item \code{MCMCglmm.sample} returns a vector of sample IDs present in the \code{MCMCglmm} object. If \code{n} is missing, all the samples IDs are returned. Else, a random series of sample IDs are returned (with replacement if n greater than the number of available samples).
#'      \item \code{MCMCglmm.covars} returns a list of covariance matrices and intercepts from a \code{MCMCglmm} object (respectively from \code{MCMCglmm$VCV} and \code{MCMCglmm$Sol}). By default, all the covariance matrices and intercepts are returned but you can use either of the arguments \code{sample} to return specific samples (e.g. \code{MCMCglmm.covars(data, sample = c(1, 42))} for returning the first and 42nd samples) or \code{n} to return a specific number of random samples (e.g. \code{MCMCglmm.covars(data, n = 42)} for returning 42 random samples). 
#' }
#' 
#' @examples
#'
#' @seealso
#' 
#' @author Thomas Guillerme
#' @export

## Get the possible levels from a MCMCglmm
MCMCglmm.levels <- function(MCMCglmm, convert = TRUE, ...) {
    convert.term.name <- function(one_term) {
        ## Return the term is simple, keep it like that
        if(length(grep(":", one_term)) == 0) {
            return(one_term)
        } else {
            ## Split the term 
            elements <- strsplit(one_term, ":")[[1]]
            ## Remove the trait component
            if(any(grep("trait", elements))) {
                elements <- elements[-grep("trait", elements)]
            }

            if(length(elements) == 1) {
                ## That's the element name
                return(gsub(" ", "", elements))
            } else {
                ## Remove spaces and get the most nested element
                return(paste(rev(unname(sapply(elements, function(X) gsub(" ", "", gsub("[^[:alnum:] ]", "", rev(strsplit(X, "\\(")[[1]])[1]))))), collapse = ":"))
            }

        }
    }

    ## Get the random terms
    random_formula <- as.character(MCMCglmm$Random$formula[2])
    if(length(random_formula) == 0) {
        random_terms <- NULL
    } else {
        random_terms <- strsplit(random_formula, "\\+")[[1]]
    }
    
    ## Get the residuals terms
    residuals_formula <- as.character(MCMCglmm$Residual$formula[2])
    if(length(residuals_formula) == 0) {
        residual_terms <- NULL
    } else {
        residual_terms <- strsplit(residuals_formula, "\\+")[[1]]
    }

    ## No terms!
    if(is.null(random_terms) && is.null(residual_terms)) {
        return(c("residual" = "units"))
    }

    ## Convert the names to make them look fancy
    if(convert) {
        if(!is.null(random_terms)) {
            random_terms <- unname(sapply(random_terms, convert.term.name))
        }
        if(!is.null(residual_terms)) {
            residual_terms <- unname(sapply(residual_terms, convert.term.name))
        }
    }

    ## Get the terms names
    all_terms <- c(random_terms, residual_terms)
    names(all_terms) <- c(rep("random", length(random_terms)), rep("residual", length(residual_terms)))

    return(all_terms)
}

## Get the number of traits from a MCMCglmm
MCMCglmm.traits <- function(MCMCglmm, ...) {

    ## Get the variables
    variables <- as.character(MCMCglmm$Fixed$formula[2])

    ## Are the variables bunched in an expression (e.g. "c()")?
    if(any(grep("\\(", variables))) {
        ## Remove the brackets of the expression
        variables <- strsplit(strsplit(variables, "\\(")[[1]][2], "\\)")[[1]]
    }

    ## Are there multiple variables in that expression (e.g. ",")?
    if(any(grep(",", variables))) {
        ## Remove the commas and spaces
        variables <- gsub(" ", "", strsplit(variables, ",")[[1]])
    }

    return(variables)
}

## Get the samples from a MCMCglmm object
MCMCglmm.sample <- function(MCMCglmm, n, ...) {
    if(missing(n)) {
        return(1:nrow(MCMCglmm$Sol))
    } else {
        return(sample(1:nrow(MCMCglmm$Sol), n, replace = n > nrow(MCMCglmm$Sol)))
    }
}

## Get some covar matrices
MCMCglmm.covars <- function(MCMCglmm, sample, n, ...){   
    
    # warning("MCMCglmm.covars DEBUG")
    # return(invisible())

    # ## The number of traits
    # traits <- MCMCglmm.traits(MCMCglmm)
    # n_traits <- length(traits)
    # ## The number of levels
    # levels <- MCMCglmm.levels(MCMCglmm)
    # n_levels <- length(levels)

    # ## Sample n covar matrices
    # if(missing(n)) {
    #     n <- MCMCglmm.samples(MCMCglmm)
    # }
    #     covar_matrices <- unlist(replicate(n, get.one.covar(MCMCglmm, levels, traits), simplify = FALSE), recursive = FALSE)
    #     }
    # }

    # ## Rearrange the list per levels
    # results_out <- list()
    # for(one_level in 1:n_levels) {
    #     results_out[[one_level]] <- unname(covar_matrices[which(names(covar_matrices) == levels[one_level])])
    #     names(results_out)[one_level] <- levels[one_level]
    # } 

    # ## Set the class to beer
    # class(results_out) <- "beer"
    # return(results_out)
}

## Internal function to get one covariance matrix
get.one.covar <- function(one_sample, data, levels, traits) {
    ## Select a specific sample
    sample_estimates <- list(VCV = data$VCV[one_sample, ],
                             Sol = data$Sol[one_sample, ])

    ## Get one covar matrix
    make.covar <- function(level, VCV, levels, n_traits) {
        matrix(VCV[(1:n_traits^2) + (level-1) * n_traits^2], ncol = n_traits)
    }

    ## Get the estimated solutions
    make.sol <- function(level, Sol, levels, n_traits) {
        if(names(levels[level]) == "random") {
            return(rep(0, n_traits))
        } else {
            return(unname(Sol[1:n_traits + (level - (sum(names(levels) == "random") + 1)) * n_traits]))
        }
    }

    ## Sapply wrapper
    sapply.fun <- function(level, sample_estimates, levels, traits) {
        list(VCV = make.covar(level, sample_estimates$VCV, levels, length(traits)),
             Sol =   make.sol(level, sample_estimates$Sol, levels, length(traits)))
    }

    levels_covar <- sapply(1:length(levels), sapply.fun, sample_estimates, levels, traits, simplify = FALSE)
    names(levels_covar) <- levels

    ## Return the covariance matrices and origins
    return(levels_covar)
}

