#' @title MCMCglmm.dispRity
#'
#' @description Creating a dispRity object from a MCMCglmm posterior output
#'
#' @param data The data used for the MCMCglmm model
#' @param posteriors A MCMCglmm object, the posteriors of the model
#' @param group Optional, a named vector of which group to include from the posteriors (if left empty the random and residual terms are used). See details.
#' @param tree Optional, the tree(s) used in the MCMCglmm analyses.
#' @param rename.groups optional, a vector of group names for renaming them. See details.
#' 
#' @details
#' \itemize{
#'      \item For the \code{group} option, the group names must be ones found in the \code{posteriors} formula in the format \emph{<Type = Term:FactorLevel>} as returned by \code{MCMCglmm.levels(posteriors)}. For example, for returning two random effect, the phylogenetic one (\code{"animal"}) and one for a specific clade (say the 2nd clade) as well as two residual terms for a specific factor (say level 1 and 4) you can use \code{group = c(random = "animal", random = "animal:clade2", residual = "units:myfactor1", residual = "units:myfactor4")}.
#'      \item For the \code{rename.groups} option, the vector must be of class \code{"character"} and must of the same length as the number of random and residual terms in \code{posteriors} or of \code{group} argument (if used). If the \code{group} argument is left empty, the groups are extracted from the \code{posteriors} in the following order: the random terms first then the residual terms as specified in the \code{posteriors} object formulas (respectively \code{posteriors$Random$formula} and \code{posteriors$Residual$formula}).
#' 
#' 
#' }
#' 
#' @examples
#'
#' @seealso
#' 
#' @author Thomas Guillerme
#' @export

MCMCglmm.dispRity <- function(data, posteriors, group, tree, rename.groups) {

    ## Cleaning then checking the data (i.e. only removing the values)
    #dispRity_export in: data_class <- check.class(data, c("data.frame", "matrix"))
    data_class <- class(data)[[1]] #dispRity_export out:
    if(data_class == "data.frame") {
        ## Remove potential non-numeric columns
        col_classes <- sapply(1:ncol(data), function(col, dat) class(data[,col]), dat = data)
        numerics <- (col_classes %in% c("numeric", "integer"))
        ## Check for non-numerics
        if(!any(numerics)) {
            #dispRity_export in: stop.call(msg = "The data does not contain any column with numeric or integer values.", call = "")
            stop("The data does not contain any column with numeric or integer values.")#dispRity_export out: 
        }
        ## Clean the data
        cleaned_data <- as.matrix(data[,numerics])
        ## Is there any classification column?
        classifier <- col_classes[!numerics] %in% "factor"
        if(any(classifier)) {
            group_classifier <- data[,which(!numerics)[which(classifier)], drop = FALSE]
        }
    }
    
    ## Checking the posteriors
    #dispRity_export in: check.class(posteriors, "MCMCglmm")
    ## Check which dimensions where used
    dimensions <- match(MCMCglmm.traits(posteriors), colnames(cleaned_data))

    ## Extracting the residuals and randoms
    posterior_levels <- MCMCglmm.levels(posteriors)
    posterior_terms <- lapply(posterior_levels, split.term.name)

    ## Extracting the formula
    formula <- list(Fixed    = posteriors$Fixed$formula,
                    Random   = posteriors$Random$formula,
                    Residual = posteriors$Residual$formula)
    
    ## Extracting the group from the posteriors
    extracted_group <- lapply(posterior_terms, get.one.group, group_classifier, elements = rownames(cleaned_data))
    names(extracted_group) <- posterior_levels

    ## Setting the groups
    if(missing(group)) {
        subsets <- extracted_group
        selected_groups <- seq(1:length(extracted_group))
    } else {
        if(is.null(names(group))) {
            #dispRity_export in: MAKE dispRity STOP STYLE
            stop("group must be a named character vector.")#dispRity_export out:
        } else {
            if(!any(is.na(selected_groups <- match(group, posterior_levels)))) {
                ## Select only the specific groups
                subsets <- extracted_group[selected_groups]
            } else {
                #dispRity_export in: MAKE dispRity STOP STYLE
                stop("groups not found in posteriors. Check MCMCglmm(posteriors) for the group names.")#dispRity_export out:                
            }
        }
    }

    ## Getting the covar matrices per group
    covar_matrices <- MCMCglmm.covars(posteriors)[selected_groups]

    ## Renaming the groups
    if(!missing(rename.groups)) {
        if(length(rename.groups) != length(subsets)) {
            #dispRity_export in: MAKE dispRity STOP STYLE
            stop("rename.groups must the same length as group.")#dispRity_export out:
        }
        names(subsets) <- names(covar_matrices) <- rename.groups
    }

    ## Adding the tree
    if(!missing(tree)) {
        ## Create a dispRity style object with tree
        stop("do tree")
    } else {
        ## Create a dispRity style object without tree
        output <- dispRity::make.dispRity(data = cleaned_data, call = list("subsets" = "MCMCglmm", "dimensions" = dimensions), subsets = subsets)
        output$MCMCglmm <- list(formula = formula, covars = covar_matrices)
        return(output)
    }
}

## create the group list
get.one.group <- function(one_term, group_classifier, elements) {
    ## Animal term (phylogeny)
    if(is.null(one_term$factor) && is.null(one_term$level)) {
        ## The group is the full phylogeny (dispRity format)
        #return(1:length(elements))
        return(list(elements = matrix(1:length(elements), ncol = 1)))
    } else {
        ## Get the factor in group_classifier (dispRity format)
        #return(which(group_classifier[,one_term$factor] == levels(group_classifier[,one_term$factor])[one_term$level]))
        return(list(elements = matrix(which(group_classifier[,one_term$factor] == levels(group_classifier[,one_term$factor])[one_term$level]), ncol = 1)))
    }
}

## Splitting a term name
split.term.name <- function(one_term) {
    ## Initialise the factor and level
    factor <- level <- NULL
    ## Split the term
    split_term <- strsplit(one_term, ":")[[1]]
    ## Get the second part of the term
    if(length(split_term) > 1) {
        level  <- as.numeric(gsub(".*?([[:digit:]]+)", "\\1", split_term[[2]]))
        factor <- gsub("_", "", as.character(gsub(level, "", split_term[[2]])))
    }
    return(list(term   = split_term[[1]],
                factor = factor,
                level  = level))
}