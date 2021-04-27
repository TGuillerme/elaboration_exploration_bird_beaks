#' @title Analyses per group
#'
#' @description Wrapper for \code{\link{vector.diff}} to run the analyses per groups
#'
#' @param axes a list of vector coordinates (typically from \code{get.axes})
#' @param base which element in \code{"beer"} to use as the base vector (by default the first element is used).
#' @param groups which element in \code{"beer"} to use analyse (if left missing, all but the base are analysed).
#' @param ... any arguments to be passed to \code{\link{vector.diff}}.
#' 
#' @examples
#'
#' @seealso
#' 
#' @author Thomas Guillerme
#' @export

analyses.group <- function(axes, base = 1, groups, ...) {

    ## Converting base into numeric
    if(is(base, "character")) {
        base <- which(names(axes) %in% base)
    }

    ## Getting the groups
    if(missing(groups)) {
        groups <- (1:length(axes))[-base]
    }

    ## Getting the corresponding vectors
    base_vectors <- axes[[base]]
    group_vectors <- axes[groups]

    ## Run all the analyses
    return(lapply(group_vectors, function(group, base_vectors, ...) t(mapply(vector.diff, group, base_vectors, MoreArgs = list(...))), base_vectors, ...))
}