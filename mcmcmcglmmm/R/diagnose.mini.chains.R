#' @title diagnose.mini.chains
#'
#' @description Diagnose a set of mini chains results
#'
#' @param mini.chains a MCMCglmm result
#' 
#' @examples
#'
#' @seealso
#' 
#' @author Thomas Guillerme
#' @export

diagnose.mini.chains <- function(mini.chains) {

    ## Summarising the chain
    summarised <- summary.MCMCglmm(mini.chains)

    ## Interesting bits?
    ESS_sol  <- summarised$solutions[, "eff.samp"]
    ESS_VCVG <- summarised$Gcovariances[, "eff.samp"]
    ESS_VCVR <- summarised$Rcovariances[, "eff.samp"]

    ## Effective size of the Random terms
    op <- par(mfrow = c(3,1))
    hist(ESS_sol, main = "Solution")
    abline(v = 200)

    hist(ESS_VCVR, main = "Residual terms")
    abline(v = 200)
    
    hist(ESS_VCVG, main = "Random terms")
    abline(v = 200)
    par(op)

    return("prototype")
}