#' @title Extract parameters
#'
#' @description Extracting parameters from prior MCMCglmm chains
#'
#' @param chains The prior MCMCglmm chains
#' @param parameters The parameters to extract (see details).
#' @param buffer The buffer for the burnin (how many itteration to include past the first median estimate)
#' @param nu The degree of belief parameter (nu) for the priors.
#' 
#' @details
#' The parameters that can be extracted are:
#' \itemize{
#'      \item \code{"burnin"}: the global burnin length
#'      \item \code{"priors"}: a list of priors (from the posteriors)
#' }
#' 
#' @examples
#'
#' @seealso
#' 
#' @author Thomas Guillerme
#' @export
extract.parameters <- function(chains, parameters = c("burnin", "priors"), buffer = 0.25, nu = 0.05) {

    ## Get the required parameters
    param_out <- list()
    if("burnin" %in% parameters) {
        param_out$burnin <- max(unlist(lapply(chains, get.burnin, buffer = buffer)))
    }
    if("priors" %in% parameters) {
        ## Check for burnin
        if(!is.null(param_out$burnin)) {
            burnin <- param_out$burnin/attr(chains[[1]]$Sol, "mcpar")[3]
        } else {
            burnin <- max(unlist(lapply(chains, get.burnin, buffer = buffer)))/attr(chains[[1]]$Sol, "mcpar")[3]
        }
        ## Get the list of priors
        priors_list <- lapply(chains, get.prior, nu = nu, burnin = burnin)  
        ## Merging the priors
        param_out$priors <- merge.V(priors_list)
    }

    return(param_out)
}

## Merge the lists of priors V matrices
merge.V <- function(priors_list) {
    ## Get the number of levels
    n_ran <- length(priors_list[[1]]$G)
    n_res <- length(priors_list[[1]]$R)
    n_traits <- dim(priors_list[[1]]$R$R1$V)[1]
    nu <- priors_list[[1]]$R$R1$nu

    ## Get the template to fill
    template <- flat.prior(ntraits = n_traits, residuals = n_res, randoms = n_ran, nu = nu)
    
    ## Get the length of the list
    n_chains <- length(priors_list)

    ## Fill the list for G
    for(one_ran in 1:n_ran) {
        ## Get the list of V matrices
        V_list <- lapply(lapply(priors_list, `[[`, "G"), function(X, i) return(X[[i]]$V), i = one_ran)
        template$G[[one_ran]]$V <- apply(array(do.call(cbind, V_list), dim = c(dim(V_list[[1]]), length(V_list))), c(1,2), median)
    }

    ## Fill the list for R
    for(one_res in 1:n_res) {
        ## Get the list of V matrices
        V_list <- lapply(lapply(priors_list, `[[`, "R"), function(X, i) return(X[[i]]$V), i = one_res)
        template$R[[one_res]]$V <- apply(array(do.call(cbind, V_list), dim = c(dim(V_list[[1]]), length(V_list))), c(1,2), median)
    }
    return(template)
}

## Get the burnin value
get.burnin <- function(chain, buffer) {

    ## Find the median for each MCMC
    find.median <- function(estimate, buffer) {
        ## Get the first estimate past the median
        return(ceiling(which(estimate > median(estimate))[1] * (1+buffer)))
    }

    burnin_points <- apply(chain$Sol, 2, find.median, buffer)
    return(max(burnin_points * attr(chain$Sol, "mcpar")[3]))
}

## Get the priors from one chain
get.prior <- function(chain, nu = 0.05, burnin = FALSE) {

    ## Get the set of parameters
    traits <- MCMCglmm.traits(chain)
    n_traits <- length(traits)
    ## The number of levels
    levels <- MCMCglmm.levels(chain)
    n_levels <- length(levels)
    n_ran <- sum(names(levels) %in% "random")
    n_res <- sum(names(levels) %in% "residual")

    ## Create the prior template
    template <- flat.prior(ntraits = n_traits, residuals = n_res, randoms = n_ran, nu = nu)

    ## Get the burnin
    if(missing(burnin)) {
        burnin <- as.integer(get.burnin(chain, buffer = 0.25)/attr(chain$Sol, "mcpar")[3])
    }
    burnin <- 1:burnin

    ## Handle the G-Structure bits
    for(i in 1:n_ran) {
        ## Get the columns of interest
        cells <- ((n_ran * n_traits)*(i-1)+1):((n_ran * n_traits)+(n_ran * n_traits)*(i-1))
        ## Get the mean estimates
        template$G[[i]]$V <- matrix(apply(chain$VCV[-burnin, cells], 2, mean), n_traits, n_traits, byrow = FALSE)
    }

    ## Handle the R-Structure bits
    for(i in 1:n_res) {
        ## Get the columns of interest
        cells <- (n_ran * n_traits^2) + ((n_res * n_traits^2)*(i-1)+1):((n_res * n_traits^2)+(n_res * n_traits^2)*(i-1))
        ## Get the mean estimates
        template$R[[i]]$V <- matrix(apply(chain$VCV[-burnin, cells], 2, mean), n_traits, n_traits, byrow = FALSE)
    }

    return(template)
}


