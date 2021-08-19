# #' @title Get covariance matrices
# #'
# #' @description Get a sub-sample of the covariance matrices from a MCMCglmm object and the associated MME posterior intercept (solution) sorted by levels
# #'
# #' @param MCMCglmm The MCMCglmm object
# #' @param n The number of samples
# #' 
# #' @return 
# #' Returns a \code{"beer"} object that is a list of length \code{n}.
# #' Each \code{n}th element is a list of of the same length as the number of random + residual terms in \code{data} containing one variance covariance matrix (\code{get.covar(data)[[1]][[1]]$VCV}) and the intercept ((\code{get.covar(data)[[1]][[1]]$Sol})).
# #' 
# #' @examples
# #'
# #' @seealso
# #' 
# #' @author Thomas Guillerme, Gavin Thomas
# #' @export
# get.covar <- function(MCMCglmm, n = 1){   
#     ## The number of traits
#     traits <- MCMCglmm.traits(MCMCglmm)
#     n_traits <- length(traits)
#     ## The number of levels
#     levels <- MCMCglmm.levels(MCMCglmm)
#     n_levels <- length(levels)

#     ## Sample n covar matrices
#     covar_matrices <- unlist(replicate(n, get.one.covar(MCMCglmm, levels, traits), simplify = FALSE), recursive = FALSE)

#     ## Rearrange the list per levels
#     results_out <- list()
#     for(one_level in 1:n_levels) {
#         results_out[[one_level]] <- unname(covar_matrices[which(names(covar_matrices) == levels[one_level])])
#         names(results_out)[one_level] <- levels[one_level]
#     } 

#     ## Set the class to beer
#     class(results_out) <- "beer"
#     return(results_out)
# }

# ## Internal function to get one covariance matrix
# get.one.covar <- function(data, levels, traits) {

#     ## Select a random value      
#     one_point <- sample(1:nrow(data$VCV), 1)
#     sample_estimates <- list(VCV = data$VCV[one_point, ],
#                              Sol = data$Sol[one_point, ])

#     ## Get one covar matrix
#     make.covar <- function(level, VCV, levels, n_traits) {
#         matrix(VCV[(1:n_traits^2) + (level-1) * n_traits^2], ncol = n_traits)
#     }

#     ## Get the estimated solutions
#     make.sol <- function(level, Sol, levels, n_traits) {
#         if(names(levels[level]) == "random") {
#             return(rep(0, n_traits))
#         } else {
#             return(unname(Sol[1:n_traits + (level - (sum(names(levels) == "random") + 1)) * n_traits]))
#         }
#     }

#     ## Sapply wrapper
#     sapply.fun <- function(level, sample_estimates, levels, traits) {
#         list(VCV = make.covar(level, sample_estimates$VCV, levels, length(traits)),
#              Sol =   make.sol(level, sample_estimates$Sol, levels, length(traits)))
#     }

#     levels_covar <- sapply(1:length(levels), sapply.fun, sample_estimates, levels, traits, simplify = FALSE)
#     names(levels_covar) <- levels

#     ## Return the covariance matrices and origins
#     return(levels_covar)
# }

