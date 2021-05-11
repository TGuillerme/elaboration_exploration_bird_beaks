#' @title Optimised replicates
#'
#' @description A wrapper for replicating a based on results variance
#'
#' @param function The function to execute
#' @param minimum The minimum of replicates to run (default is 20)
#' @param maximum Optional, a maximum of iterations (default is Inf).
#' @param diagnose The diagnosis function
#' @param stop.variance The percentage of variance change for stopping (default is 0.05).
#' @param increment The percentage of increment (if left empty, the value from stop.variance is used).
#' @param verbose Whether to be verbose (\code{TRUE}) or not (\code{FALSE}; default)
#' @param parallel Whether to use the \code{\link[future.apply]{future_replicate}} function for parallelisation (\code{TRUE}) or not (\code{FALSE}; default).
#' @param par.plan If \code{parallel = TRUE}, which plan to use (see \code{\link[future]{plan}}; by default, the plan is "future::multisession").
#' @param ... Any optional arguments to be passed to the function.
#' 
#' @examples
#'
#' @seealso
#' 
#' @author Thomas Guillerme
#' @export

# optim.replicate <- function(function, diagnose, minimum = 20, maximum = Inf, stop.variance = 0.05, increment, verbose = FALSE, parallel = FALSE, par.plan = "future::multisession", ...) {

#     ## Check increment
#     if(missing(increment)) {
#         increment <- stop.variance
#     }

#     ## Check diagnosis
#     if(missing(diagnose)) {
#         stop("Impossible to optimize the replicate loop without a diagnose function.")
#     }

#     ## Set the parallel (or not)
#     if(parallel) {
#         ## Set the plan for the future (please switch to: capitalism = NULL for a brighter future)
#         future::plan(par.plan)
#         ## Set the replicate function type
#         rep.fun <- future.apply::future_replicate
#     } else {
#         rep.fun <- base::replicate
#     }

#     ## Set the function to be verbose or not
#     eval.verbose(fun, )


#     ## Running the minimum number of iterations
#     output <- rep.fun(minimum, function, ...)
#     n_iterations <- minimum

#     ## Diagnosing the output variance
#     diagnosis <- lapply(output, diagnose)
#     diagnosis_table <- do.call(cbind, diagnosis)

#     ## Continue the runs
#     while(!all(apply(diagnosis_table, 2, var) < stop.variance) || n_iterations >= maximum) {
#         ## Add some iterations
#         add_iterations <- ceiling(minimum*increment)
#         output <- unlist(c(output, rep.fun(add_iterations, function, ...), recursive = FALSE))
#         n_iterations <- n_iterations + add_iterations

#         ## Diagnosing the output variance
#         diagnosis <- lapply(output, diagnose)
#         diagnosis_table <- do.call(cbind, diagnosis)
#     }

#     ## Return the output
#     return(output)
# }

eval.verbose <- function(fun, msg = ".") {
    new.fun <- function() {
        message(msg, appendLF = FALSE)
        return(fun())
    }
    return(new.fun)
}
