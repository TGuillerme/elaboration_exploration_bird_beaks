#' @title Extract parameters
#'
#' @description Extracting parameters from prior MCMCglmm chains
#'
#' @param chains The prior MCMCglmm chains
#' @param parameters The parameters to extract (see details).
#' @param buffer The buffer for the burnin (how many itteration to include past the first median estimate)
#' @param credence The credence level: the degree of belief parameter (nu) for the priors.
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

extract.parameters <- function(chains, parameters = c("burnin", "priors"), buffer = 0.25, credence = 0.05) {
    return()
}

extract.one.chain <- function(chain, parameters) {

    ## Get the required parameters
    parameters <- list()
    if("burnin" %in% parameters) {
        parameters$burnin <- get.burnin(chain)
    }

    if("priors" %in% parameters) {
        # 
    }







    return(parameters)
}

get.burnin <- function(chain, buffer) {
    ## Find the median for each MCMC
    find.median <- function(estimate, buffer) {
        ## Get the first estimate past the median
        return(ceiling(which(estimate > median(estimate))[1] * (1+buffer)))
    }

    burnin_points <- apply(chain$Sol, 2, find.median, buffer)
    return(max(burnin_points * attr(chain$Sol, "mcpar")[3]))
}

get.prior <- function(chain, credence = 0.05) {

    ## Get the set of parameters
    traits <- traits.MCMCglmm(chain)
    n_traits <- length(traits)
    ## The number of levels
    levels <- levels.MCMCglmm(chain)
    n_levels <- length(levels)
    n_ran <- sum(names(levels) %in% "random")
    n_res <- sum(names(levels) %in% "residual")

    ## Create the prior template
    template <- flat.prior(ntraits = n_traits, residuals = n_res, randoms = n_ran), nu = credence)

    ## Get the burnin
    burnin <- as.integer(get.burnin(chain, buffer = 0.25)/attr(chain$Sol, "mcpar")[3])
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

    # R: R-structure (residuals)
    # G: G-structure (randoms)
    # B: fixed effects
    # B$mu=0
    # B$V=I*1e+10
}
