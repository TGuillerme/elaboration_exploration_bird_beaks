#' @title Get covariance matrices
#'
#' @description Get a sub-sample of the covariance matrices from a MCMCglmm object and the associated MME posterior intercept (solution) sorted by levels
#'
#' @param data The MCMCglmm object
#' @param n The number of samples
#' 
#' @return 
#' Returns a \code{"beer"} object that is a list of length \code{n}.
#' Each \code{n}th element is a list of of the same length as the number of random + residual terms in \code{data} containing one variance covariance matrix (\code{get.covar(data)[[1]][[1]]$VCV}) and the intercept ((\code{get.covar(data)[[1]][[1]]$Sol})).
#' 
#' @examples
#'
#' @seealso
#' 
#' @author Thomas Guillerme, Gavin Thomas
#' @export
get.covar <- function(data, n = 1){   
    ## The number of traits
    traits <- traits.MCMCglmm(data)
    n_traits <- length(traits)
    ## The number of levels
    levels <- levels.MCMCglmm(data)
    n_levels <- length(levels)

    ## Sample n covar matrices
    covar_matrices <- replicate(n, get.one.covar(data, levels, traits), simplify = FALSE)

    ## Set the class to beer
    class(covar_matrices) <- "beer"
    return(covar_matrices)
}

## Internal function to get one covariance matrix
get.one.covar <- function(data, levels, traits) {

    ## Select a random value      
    one_point <- sample(1:nrow(data$VCV), 1)
    sample_estimates <- list(VCV = data$VCV[one_point, ],
                             Sol = data$Sol[one_point, ])

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

## Internal: get the possible levels from a MCMCglmm
levels.MCMCglmm <- function(MCMCglmm, convert = TRUE) {
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

## Internal: get the number of traits from a MCMCglmm
traits.MCMCglmm <- function(MCMCglmm) {

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