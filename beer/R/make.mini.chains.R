#' @title make.mini.chains
#'
#' @description Making a list of mini.chains object (MCMCglmm primers)
#'
#' @param data a full ordinated matrix
#' @param dimensions the list of dimensions to include
#' @param trees the list of trees to run the models on
#' @param residuals the type of residuals (see details)
#' @param randoms the type of randoms (see details)
#' @param parameters the tunning parameters for the MCMCglmm
#' @param priors the priors for the MCMCglmm
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

make.mini.chains <- function(data, dimensions, trees, residuals = "global", randoms = "global", parameters, priors) {

    return("prototype")

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
    
    ## Fixed effect
    ## Setting the fixed formula (initialising)
    fixed <- fixed_model
    ## Moving the model to the second component of the formula (third)
    fixed[[3]] <- fixed[[2]]
    ## Adding the trait model as the first component (first)
    fixed[[2]] <- as.formula(paste0("~cbind(", paste(colnames(data)[dimensions], collapse = ", "), ")"))[[2]]

    ## Random effect
    random <- NULL
    if("clade" %in% randoms) {
        ## Adding the clade terms
        random <- clade.terms(n_clades, type = "animal")

        ## Adding the global terms if needed
        if("global" %in% randoms) {
            random <- update.formula(random, ~ . + us(trait):animal)
            tmp2 <- random[[2]][[3]][[2]]
            tmp1 <- random[[2]][[3]][[3]]
            random[[2]][[3]][[2]] <- tmp1
            random[[2]][[3]][[3]] <- tmp2
        }
    } else {
        ## Adding the clade terms
        random <- ~ us(trait):animal        
    }

    ## Residuals effect
    rcov <- NULL
    if("clade" %in% residuals) {
        ## Adding the clade terms
        rcov <- clade.terms(n_clades, type = "units")

        ## Adding the global terms if needed
        if("global" %in% rcov) {
            rcov <- update.formula(rcov, ~ . + us(trait):units)
            tmp2 <- rcov[[2]][[3]][[2]]
            tmp1 <- rcov[[2]][[3]][[3]]
            rcov[[2]][[3]][[2]] <- tmp1
            rcov[[2]][[3]][[3]] <- tmp2
        }
    } else {
        ## Adding the clade terms
        rcov <- ~ us(trait):units        
    }


    ## Distributions
    family = rep("gaussian", length(dimensions))

    ## Preparing the output
    output <- list(data = data,
                   tree = tree,
                   ## The MCMCglmm function
                   run  = function() MCMCglmm(fixed    = fixed,
                                              random   = random,
                                              rcov     = rcov,
                                              family   = family,
                                              pedigree = tree,
                                              data     = data,
                                              prior    = priors,
                                              burnin   = parameters$burnin,
                                              nitt     = parameters$nitt,
                                              thin     = parameters$thin))
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
