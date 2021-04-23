#' @title Get covariance matrices
#'
#' @description Get a sub-sample of the covariance matrices from a MCMCglmm object and the associated MME posterior (Solution) sorted by levels
#'
#' @param data The MCMCglmm object
#' @param n The number of samples
# @param levels Optional, the level(s) to get the covariance from (if missing, all levels are returned)
#' @param simplify logical, whether to output the results as a matrix of lists (simplify = FALSE) where rows are the levels and columns are the replicates or whether to return it as a list containing n levels elements each containing the replicates (simplify = TRUE).
#' 
#' @examples
#'
#' @seealso
#' 
#' @author Thomas Guillerme, Gavin Thomas
#' @export

get.covar <- function(data, n, simplify = TRUE){ #, levels) { #TG: levels not handled yet: returns everything
    
    ## The number of traits
    n_traits <- length(strsplit(paste(data$Fixed$formula)[2], ",")[[1]])
    ## The number of levels
    n_levels <- c(
      ## The number of random effects
      "random" = length(grep("animal", colnames(data$VCV)))/(n_traits^2),
      ## The number of groups (residuals)
      "residual" = length(grep("units", colnames(data$VCV)))/(n_traits^2))

    ## Get the names of the levels
    param_names <- character()
    if(n_levels["random"] != 0) {
        if(n_levels["random"] > 1) {
            param_names <- c(param_names, paste0("phylogeny", n_levels["random"]))
        } else {
            param_names <- c(param_names, "phylogeny")
        }
    }
    if(n_levels["residual"] != 0) {
       param_names <- c(param_names, unique(unlist(lapply(strsplit(colnames(data$Sol), ":"), `[[`, 2))))
    }

    ## Sample n covar matrices
    covar_matrices <- replicate(n, get.one.covar(data, n_levels, n_traits), simplify = TRUE)

    if(simplify) {

        ## Function for recombining the "rows" in the output from replicate() (into a list of lists)
        combine.replicates <- function(replicates) {
            return(list(
                ## Get all the VCV
                VCV = lapply(replicates, function(x) return(x$VCV)),
                ## Get all the Sol
                Sol = lapply(replicates, function(x) return(x$Sol))
            ))
        }

        ## Recombine all the replicates
        covar_matrices <- apply(covar_matrices, 1, unlist, recursive = FALSE)

        ## Naming the list
        if(length(param_names) == length(covar_matrices)) {
            names(covar_matrices) <- param_names
        }

        return(covar_matrices)
    } else {

        if(length(param_names) == nrow(covar_matrices)) {
            rownames(covar_matrices) <- param_names
        }
        
        return(covar_matrices)
    }
}

## Internal function to get one covariance matrix
get.one.covar <- function(data, n_levels, n_traits) {

    ## Select a random value      
    one_point <- sample(1:nrow(data$VCV), 1)
    covtmp <- data$VCV[one_point, ]
    soltmp <- data$Sol[one_point, ]

    ## Placeholders for the levels
    levels_covar <- list()

    ## Loop through each level
    for(one_level in 1:sum(n_levels)) {  
        ## Selecting the adjustment level (for selecting the elements in the vector of results according to the correct level)
        level_adjust <- (one_level-1) * n_traits^2

        ## Sample values for the requested level for all traits
        levels_covar[[one_level]] <- list(
                VCV = matrix(covtmp[(1:n_traits^2) + level_adjust], ncol = n_traits),
                Sol = get.sol(soltmp, n_levels, one_level, n_traits))
    }

    ## Return the covariance matrices
    return(levels_covar)
}

## Internal function for getting the posterior solution
#TG: not sure about that one! needs checks!
get.sol <- function(posterior_sol, n_levels, one_level, n_traits, level_adjust) {

    ## Has no random effect
    if(n_levels["random"] == 0) {
        return(posterior_sol[1:n_traits + (one_level - 1) * n_traits])
    }

    ## Has one random effect and one level
    if (n_levels["random"] == 1 && n_levels["residual"] == 1) {
        return(rep(0, n_traits))
    }

    ## Has one random effect and more that one residual
    if(n_levels["random"] == 1 && n_levels["residual"] > 1) {
        ## Is the random effect
        if(one_level == 1) {
            warning("Random effects centred on zero")
            return(rep(0, n_traits))
        }
        ## Is another residual
        return(posterior_sol[1:n_traits + (one_level - 2) * n_traits])
    }

    ## Multiple randoms and the same number of residuals
    if(n_levels["random"] > 1 && n_levels["random"] == n_levels["residual"])  {

        ## The level is one of the random ones
        if(one_level %in% 1:n_levels["random"]) {
          return(posterior_sol[1:n_traits + (one_level - 1) * n_traits])
        } else {
            ## The level is one of the residual one
            return(posterior_sol[1:n_traits + (one_level - n_levels["random"] - 1) * n_traits])
            #TG: not sure about the following bit:
            #TG: "(one_level - n_levels["random"] - 1)"
            #TG: needs checking
        }
    }

    ## No case was triggered return 0
    return(rep(0, n_traits))
}

## Internal: get the possible levels from a MCMCglmm
levels.MCMCglmm <- function(MCMCglmm) {

}

## Internal: get the number of traits from a MCMCglmm
traits.MCMCglmm <- function(MCMCglmm) {

    ## Get the variables
    variables <- as.character(MCMCglmm$Fixed$formula[2])

    ## Are the variables bunched in an expression (e.g. "c()")?
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