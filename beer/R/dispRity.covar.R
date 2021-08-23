#' @title Temporary wrapper for dispRity
#'
#' @description TO BE PROPERLY INCLUDED IN dispRity
#'
#' @param data the data with covar
#' @param metric the metric
#' @param between.groups (for group analyses or tip analyses)
#' 
#' @examples
#'
#' @seealso
#' 
#' @author Thomas Guillerme
#' @export

dispRity.covar <- function(data, metric, between.groups = FALSE, ...) {
   if(is(between.groups, "logical") && !between.groups) {
      ## Do the per tip analyses
      disparity <- lapply(lapply(data$MCMCglmm$covars, lapply, metric, ...), unlist)
   } else {
      ## Do the per group analyses
      if(is(between.groups, "logical") && between.groups) {
         list_of_pairs <- unlist(apply(combn(1:n.subsets(data), 2), 2, list), recursive = FALSE)
      } else {
         list_of_pairs <- between.groups
         between.groups <- TRUE
      }
      ## Get the list of pairs names
      names(list_of_pairs) <- lapply(list_of_pairs, function(one_pair, data) return(paste0(names(data$subsets)[one_pair[1]], ":", names(data$subsets)[one_pair[2]])), data)

      ## Make the list of pairs (super rough - done properly in dispRity)
      covar_pairs <- lapply(list_of_pairs, function(one_pair, data) return(list(data$MCMCglmm$covars[[one_pair[1]]], data$MCMCglmm$covars[[one_pair[2]]])), data = data)

      ## Mapply all that
      mapply.one.pair <- function(one_pair, metric, ...) {
         values <- unlist(mapply(metric, one_pair[[1]], one_pair[[2]], MoreArgs = list(...), SIMPLIFY = FALSE))
         return(list("elements" = mean(values), matrix(values, nrow = 1)))
      }
      ## Calculate disparity
      disparity <- lapply(covar_pairs, mapply.one.pair, metric, ...)
      #disparity <- lapply(covar_pairs, mapply.one.pair, metric)
   }

   ## Updating the object   
   data$disparity <- disparity
   data$call$subsets <- "custom" #TO BE covars
   data$call$bootstrap <- list(length(data$MCMCglmm$covars[[1]]), "covars", FALSE) # TO be handled normally
   data$call$disparity <- list(metrics = list(name = "metric.name", between.groups = between.groups)) # TO be handled normally

   return(data)
}
