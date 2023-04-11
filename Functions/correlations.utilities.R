#' @name get.correlations.plot
#'
#' @description extracts the point estimate (e.g. median) of a series of results for the correlation plots
#' 
#' @param res the results in a dispRity list structure (typically from dispRity.covar.projections)
#' @param cent.tend the central tendency
#' @param exclude.phylo logical, whether to exclude the phylogeny component (level 0)
#' 
## Extract the point estimate correlation from the results structure
get.correlations.plot <- function(res, cent.tend = median, exclude.phylo = FALSE) {

    ## Remove the phylo one (if needed)
    if(exclude.phylo) {
        select_pos <- names(size.subsets(res$position))
        select_dis <- names(size.subsets(res$distance))
        select_pos <- select_pos[-which(select_pos %in% "phylogeny")]
        select_dis <- select_dis[-which(select_dis %in% "phylogeny")]
        res$position <- get.subsets(res$position, subsets = select_pos)
        res$distance <- get.subsets(res$distance, subsets = select_dis)
    }

    ## Extracting all the results
    disparity_pos <- lapply(res$position$disparity, function(X, type) return(X$elements))
    disparity_dis <- lapply(res$distance$disparity, function(X, type) return(X$elements))

    ## Measure the central tendencies
    centrals_pos <- lapply(disparity_pos, function(X, fun) apply(X, 1, fun), fun = cent.tend)
    centrals_dis <- lapply(disparity_dis, function(X, fun) apply(X, 1, fun), fun = cent.tend)
    
    ## Number of groups
    n_groups <- length(centrals_pos)

    ## Prepare the results
    prep.cor.results <- function(pos, dis, name) {
        ## Run the correlation test
        cor_test <- cor.test(pos, dis)
        ## Prepare the plot title
        plot_main <- paste0(name, ": ", unname(round(cor_test$estimate, 3)), " (p=", unname(round(cor_test$p.value, 3)), ")")
        ## Return the plotting parameters
        return(list(x = pos, y = dis, main = plot_main))
    }

    return(mapply(prep.cor.results, centrals_pos, centrals_dis, as.list(names(centrals_pos)), SIMPLIFY = FALSE))
}

#' @name get.correlations.scores
#'
#' @description calculates the distribution of correlation between elaboration and innovation
#'
#' @param res the results in a dispRity list structure (typically from dispRity.covar.projections)
#' @param cent.tend the central tendency for testing the correlation
#' @param exclude.phylo logical, whether to exclude the phylogeny component (level 0)
#' @param test which test to apply to check correlation
get.correlations.scores <- function(res, cent.tend = median, exclude.phylo = FALSE, test = cor.test) {
    ## Remove the phylo one (if needed)
    if(exclude.phylo) {
        select_pos <- names(size.subsets(res$position))
        select_dis <- names(size.subsets(res$distance))
        select_pos <- select_pos[-which(select_pos %in% "phylogeny")]
        select_dis <- select_dis[-which(select_dis %in% "phylogeny")]
        res$position <- get.subsets(res$position, subsets = select_pos)
        res$distance <- get.subsets(res$distance, subsets = select_dis)
    }

    ## Extracting all the results
    disparity_pos <- lapply(res$position$disparity, function(X, type) return(X$elements))
    disparity_dis <- lapply(res$distance$disparity, function(X, type) return(X$elements))
    
    ## Measuring the correlations
    get.cor <- function(elaboration, innovation, cent.tend, test) {
        correlations <- mapply(cor,
                               unlist(apply(elaboration, 2, list), recursive = FALSE),
                               unlist(apply(innovation, 2, list), recursive = FALSE))
        p_value <- test(apply(elaboration, 1, cent.tend), apply(innovation, 1, cent.tend))$p.value
        return(list(correlations = correlations, p.value = p_value))
    }

    ## Get all correlations
    return(mapply(get.cor, disparity_pos, disparity_dis, SIMPLIFY = FALSE,
                  MoreArgs = list(test = test, cent.tend = cent.tend)))
}