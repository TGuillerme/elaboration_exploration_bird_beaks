#' @title make.mini.chains
#'
#' @description Making a list of mini.chains object (MCMCglmm primers)
#'
#' @param data a full ordinated matrix
#' @param dimensions the list of dimensions to include
#' @param trees the list of trees to run the models on
#' @param trait.family the family of the traits (default is "gaussian")
#' @param residuals the type of residuals (see details)
#' @param randoms the type of randoms (see details)
#' @param parameters the named list of parameters for the MCMCglmm (if missing, it is set to \code{list(nitt=1000, burnin = 100, thin = 100)} for a fast test)
#' @param priors A list of priors or a value of the overal nu parameter for generating flat priors for the MCMCglmm
#' @param verbose whether to make the MCMCglmm verbose (TRUE; default) or not (FALSE).
#' 
#' @details
#' The types of model for the residuals and random terms can be:
#' \itemize{
#'  \item \code{"global"} for just an overall term effect. For the random terms that would be just a phylogenetic effect (\code{~us(trait):animal}) or for the residuals just an overall category (\code{~ us(trait):unit}).
#'  \item \code{"clade"} for just as many levels of effects as there are clades in the data. This is used in the form \code{us(at.level(clade,1):trait):animal} for the random effects (i.e. a special phylogenetic random effect for clade 1) or \code{us(at.level(clade,1):trait):unit} for the residuals (i.e. a special residual effect for clade 1).
#' \item \code{c("global", "clade")} for the combination of both effects described above.
#' }
#' 
#' 
#' @examples
#'
#' @seealso
#' 
#' @author Thomas Guillerme
#' @export
make.mini.chains <- function(data, dimensions, trees, trait.family = "gaussian", residuals = "global", randoms = "global", parameters, priors = 0.02, verbose = TRUE) {

    ## Setting the fixed effect model
    ## Is there any clade effect in the model?
    if(length(grep("clade", c(residuals, randoms))) == 0) {
        clade_term <- FALSE
        fixed_model <- ~trait-1
    } else {
        clade_term <- TRUE
        fixed_model <- ~trait:clade-1    
    }

    ## Find the number of clades
    if(clade_term) {
        if(!("clade" %in% colnames(data))) {
            stop("Missing clade column in data.")
        } else {
            if(!is(data$clade, "factor")) {
                data$clade <- as.factor(data$clade)           
            }
        }
        n_clades <- length(levels(data$clade))
    }

    ## Check the dimensionality of the data
    if(!all(dim_check <- apply(morphdat[, dimensions], 2, is, "numeric"))) {
        stop(paste0("Invalid dimensions (not numeric?): ", paste(names(which(!dim_check)), collapse = ", "), "."))
    }
    
    ## Fixed effect
    ## Setting the fixed formula (initialising)
    fixed <- fixed_model
    ## Moving the model to the second component of the formula (third)
    fixed[[3]] <- fixed[[2]]
    ## Adding the trait model as the first component (first)
    fixed[[2]] <- as.formula(paste0("~cbind(", paste(colnames(data)[dimensions], collapse = ", "), ")"))[[2]]

    ## Random effect
    random <- NULL
    n_randoms <- 0
    if("clade" %in% randoms) {
        ## Adding the clade terms
        random <- clade.terms(n_clades, type = "animal")
        n_randoms <- n_randoms + n_clades

        ## Adding the global terms if needed
        if("global" %in% randoms) {
            random <- update.formula(random, ~ . + us(trait):animal)
            tmp2 <- random[[2]][[3]][[2]]
            tmp1 <- random[[2]][[3]][[3]]
            random[[2]][[3]][[2]] <- tmp1
            random[[2]][[3]][[3]] <- tmp2
            n_randoms <- n_randoms + 1
        }
    } else {
        ## Adding the clade terms
        random <- ~ us(trait):animal        
        n_randoms <- n_randoms + 1
    }

    ## Residuals effect
    rcov <- NULL
    n_residuals <- 0
    if("clade" %in% residuals) {
        ## Adding the clade terms
        rcov <- clade.terms(n_clades, type = "units")
        n_residuals <- n_residuals + n_clades

        ## Adding the global terms if needed
        if("global" %in% residuals) {
            rcov <- update.formula(rcov, ~ . + us(trait):units)
            tmp2 <- rcov[[2]][[3]][[2]]
            tmp1 <- rcov[[2]][[3]][[3]]
            rcov[[2]][[3]][[2]] <- tmp1
            rcov[[2]][[3]][[3]] <- tmp2
            n_residuals <- n_residuals + 1
        }
    } else {
        ## Adding the clade terms
        rcov <- ~ us(trait):units
        n_residuals <- n_residuals + 1
    }

    ## Priors
    if(is(priors, "numeric")) {
        priors <- flat.prior(ntraits = length(dimensions), residuals = n_residuals, randoms = n_randoms, nu = priors)
    }

    ## Parameters
    if(missing(parameters)) {
        parameters <- list()
    }
    if(is.null(parameters$nitt)) {
        parameters$nitt <- 1000
    }
    if(is.null(parameters$burnin)) {
        parameters$burnin <- 100
    }
    if(is.null(parameters$thin)) {
        parameters$thin <- 100
    }    

    ## Distributions
    if(length(trait.family) == 1) {
        family <- rep(trait.family, length(dimensions))
    } else {
        if(length(trait.family) == length(dimensions)) {
            family <- trait.family
        } else {
            stop("Incorrect family (must be either a single character string or of the same number as dimensions).")
        }
    }

    ## Set the tree
    if(is(tree, "phylo")) {
        tree <- list(tree)
        class(tree) <- "multiPhylo"
    }
    if(!(is(tree, "multiPhylo"))) {
        stop("tree must be a phylo or multiPhylo object.")
    }

    ## Setting the tree(s)
    output <- lapply(tree, function(tree, fixed, random, rvoc, family, data, priors, verbose, parameters)
       return(list(data = data,
                   tree = tree,
                   ## The MCMCglmm function
                   run  = function() MCMCglmm(fixed    = fixed,
                                              random   = random,
                                              rcov     = rcov,
                                              family   = family,
                                              pedigree = tree,
                                              data     = data,
                                              prior    = priors,
                                              verbose  = verbose,
                                              burnin   = parameters$burnin,
                                              nitt     = parameters$nitt,
                                              thin     = parameters$thin)))
                    , fixed, random, rvoc, family, data, priors, verbose, parameters)


    class(output) <- c("beer", "mini.chains")
    return(output)
}

clade.terms <- function(n_clades, type) {
    ## Term for the first clade
    form <- as.formula(paste0("~us(at.level(clade,1):trait):", type))

    ## Terms for the subsequent clades
    for(i in 2:n_clades) {
        ## Add the new level for the new clade
        add_form <- paste0(paste0("~ . + us(at.level(clade,", i, "):trait):", type))
        form <- update.formula(form, add_form)
    }

    ## Flip the terms around
    for(i in 2:n_clades) {
        eval(parse(text = paste0("tmp2 <- form", paste0(rep("[[2]]", n_clades - i + 1), collapse = ""), "[[3]][[2]]")))
        eval(parse(text = paste0("tmp1 <- form", paste0(rep("[[2]]", n_clades - i + 1), collapse = ""), "[[3]][[3]]")))
        eval(parse(text = paste0("form", paste0(rep("[[2]]", n_clades - i + 1), collapse = ""), "[[3]][[2]] <- tmp1")))
        eval(parse(text = paste0("form", paste0(rep("[[2]]", n_clades - i + 1), collapse = ""), "[[3]][[3]] <- tmp2")))
    }

    return(form)
}
