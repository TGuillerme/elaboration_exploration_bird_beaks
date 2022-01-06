#' @title diagnose.mini.chains
#'
#' @description Diagnose a set of mini chains results
#'
#' @param mini.chains a mini chain MCMCglmm result
#' @param what which part from \code{\link[MCMCglmm]{summary.MCMCglmm}} to diagnose (default is \code{"eff.samp"} for the effective sample size).
#' @param plot logical, whether to plot the results (\code{TRUE}; default) or not (\code{FALSE}).
#' @param ... if \code{plot = TRUE}, any optional arguments to be past to \code{\link[graphics]{hist}}
#' 
#' @examples
#'
#' @seealso
#' 
#' @author Thomas Guillerme
#' @export

diagnose.mini.chains <- function(mini.chains, what = "eff.samp", plot = TRUE, ...) {

    ## Is it a mini chain object or straight a MCMCglmm object?
    if(is(mini.chains, "MCMCglmm")) {
        mini.chains <- list(mini.chains)
    }

    ## Summarising the chain
    summarised <- summary.MCMCglmm(mini.chains)

    ## How many bits are summarised?
    elements <- c("solution", "Gcovariances", "Rcovariances") # TODO: make that automatic
    diagnoses <- lapply(as.list(elements), function(one_element, summarised, what) return(summarised[[one_element]][, what]))
    names(diagnoses) <- elements


    ## Plot the diagnosis
    if(plot) {

        op <- par(mfrow = c(length(diagnoses),1))

        ## Do one plot
        one.plot <- function(data, main, what) {
            graphics::hist(data, main = main, ...)
            if(what == "eff.samp") {
                graphics::abline(v = 200)
            }
        }

        ## Do all the plots
        silent <- mapply(one.plot, diagnoses, elements, MoreArgs = list(what = what), SIMPLIFY = FALSE)

        par(op)
    }

    return(diagnoses)
}