#' @title make.mini.chains
#'
#' @description Making a list of mini.chains object (MCMCglmm primers)
#'
#' @param data a full ordinated matrix with a column called \code{"animal"} and potentially one called \code{"clade"} (if needed).
#' @param dimensions the list of dimensions to include.
#' @param tree the \code{"phylo"} or \code{"multiPhylo"} object to run the models.
#' @param trait.family the family of the traits (default is \code{"gaussian"}).
#' @param residuals the type of residuals (see details).
#' @param randoms the type of randoms (see details).
#' @param parameters the named list of parameters for the MCMCglmm (if missing, it is set to \code{list(nitt=1000, burnin = 100, thin = 100)} for a fast test).
#' @param priors A list of priors or a value of the overal nu parameter for generating flat priors for the \code{\link[MCMCglmm]{MCMCglmm}}.
#' @param verbose whether to make the \code{\link[MCMCglmm]{MCMCglmm}} verbose (\code{TRUE}; default) or not (\code{FALSE}).
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
make.mini.chains <- function(data, dimensions, tree, trait.family = "gaussian", residuals = "global", randoms = "global", parameters, priors = 0.02, verbose = TRUE) {

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
        if("global" %in% randoms) {
            ## Clade terms + global
            random <- clade.terms(n_clades, type = "animal", add.global = TRUE)
            n_randoms <- n_clades + 1
        } else {
            ## Clade terms
            random <- clade.terms(n_clades, type = "animal", add.global = FALSE)
            n_randoms <- n_clades
        }
    } else {
        ## Global term
        random <- ~ us(trait):animal        
        n_randoms <- 1
    }

    ## Residuals effect
    rcov <- NULL
    n_residuals <- 0
    if("clade" %in% residuals) {
        if("global" %in% residuals) {
            ## Clade terms + global
            rcov <- clade.terms(n_clades, type = "units", add.global = TRUE)
            n_residuals <- n_clades + 1
        } else {
            ## Clade terms
            rcov <- clade.terms(n_clades, type = "units", add.global = FALSE)
            n_residuals <- n_clades
        }
    } else {
        ## Global term
        rcov <- ~ us(trait):units        
        n_residuals <- 1
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

clade.terms <- function(n_clades, type, add.global = FALSE) {
    ## Term for the first clade
    form <- as.formula(paste0("~us(at.level(clade,1):trait):", type))

    ## Terms for the subsequent clades
    for(i in 2:n_clades) {
        ## Add the new level for the new clade
        add_form <- paste0(paste0("~ . + us(at.level(clade,", i, "):trait):", type))
        form <- update.formula(form, add_form)
    }

    ## Add the global term?
    if(add.global) {
        add_global <- 1
        add_form <- paste0(paste0("~ . + us(trait):", type))
        form <- update.formula(form, add_form)
    } else {
        add_global <- 0
    }

    ## Flip the terms around
    for(i in 2:(n_clades+add_global)) {
        eval(parse(text = paste0("tmp2 <- form", paste0(rep("[[2]]", (n_clades+add_global) - i + 1), collapse = ""), "[[3]][[2]]")))
        eval(parse(text = paste0("tmp1 <- form", paste0(rep("[[2]]", (n_clades+add_global) - i + 1), collapse = ""), "[[3]][[3]]")))
        eval(parse(text = paste0("form", paste0(rep("[[2]]", (n_clades+add_global) - i + 1), collapse = ""), "[[3]][[2]] <- tmp1")))
        eval(parse(text = paste0("form", paste0(rep("[[2]]", (n_clades+add_global) - i + 1), collapse = ""), "[[3]][[3]] <- tmp2")))
    }

    return(form)
}
