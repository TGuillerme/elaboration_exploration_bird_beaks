#' @title MCMCglmm.dispRity
#'
#' @description Creating a dispRity object from a MCMCglmm posterior output
#'
#' @param data The data used for the MCMCglmm model
#' @param posteriors A MCMCglmm object, the posteriors of the model
#' @param group Optional, which group to include from the posteriors (if left empty the residuals and random terms are used)
#' @param tree Optional, the tree(s) used in the MCMCglmm analyses
#' @param rename.groups optional, a vector of group names for renaming them.
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
            recycled_group_classifier <- which(!numerics)[which(classifier)]
        }
    }
    
    ## Checking the posteriors
    #dispRity_export in: check.class(posteriors, "MCMCglmm")
    ## Extracting the residuals and randoms

    # terms

    # ## Getting the groups
    # ## Extracting the group from the posteriors
    # if(missing(group)) {
    #     ## Automatically detect the groups
    # }

    # ## Renaming the groups
    # rename.groups

    # ## Adding the tree


    return(NULL)
}

## Internal: get the possible levels from a MCMCglmm
MCMCglmm.levels <- function(MCMCglmm, convert = TRUE) {
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
MCMCglmm.traits <- function(MCMCglmm) {

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