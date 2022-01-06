#' Running the mini chains MCMCglmm method
#' 
#' A wrapper package with functions for running the mini chains MCMCglmm method.
#' 
#' @name dispRity-package
#'
#' @docType package
#'
#' @author Thomas Guillerme <guillert@@tcd.ie>, Andrew Beckerman, Natalie Cooper, Gavin Thomas.
#'
#' @concept MCMCglmm
#'
# @import MCMCglmm
# @import dispRity

NULL





# #' @title Charadriiformes
# #' @name charadriiformes
# #'
# #' @description An example of a \code{\link[MCMCglmm]{MCMCglmm}} model.
# #'
# #' @details This dataset is based on a random subset of 359 Charadriiformes (gulls, plovers and sandpipers) from Cooney et al 2017.
# #' It contains:
# #' \itemize{
# #'   \item \code{data} A \code{"data.frame"} .
# #'   \item \code{tree} A consensus tree of 359 charadriiformes species (\code{"phylo"}).
# #'   \item \code{posteriors} The posteriors from a \code{"MCMCglmm"} model (see example below).
# #' }
# #'
# #' @format one \code{data.frame}, one \code{phylo} and one \code{MCMCglmm}.
# #'
# #' @references Cooney CR, Bright JA, Capp EJ, Chira AM, Hughes EC, Moody CJ, Nouri LO, Varley ZK, Thomas GH. Mega-evolutionary dynamics of the adaptive radiation of birds. Nature. 2017 Feb;542(7641):344-7.
# #' 
# #' @examples
# # set.seed(42)
# #' \dontrun{
# #' ## Reproducing the MCMCglmm model
# #' require(MCMCglmm)
# #' data(charadriiformes)
# #' 
# #' ## Setting up the model parameters:
# #' ## 1 - The formula (the first three PC axes)
# #' model_formula <- cbind(PC1, PC2, PC3) ~ trait:clade-1
# #' ## 2 - The residual term
# #' model_residuals <- ~us(trait):units
# #' ## 3 - The random terms
# #' ## (one per clade and one for the whole phylogeny)
# #' model_randoms <- ~ us(at.level(clade,1):trait):animal
# #'                  + us(at.level(clade,2):trait):animal
# #'                  + us(at.level(clade,3):trait):animal
# #'                  + us(trait):animal
# #' 
# #' ## Flat priors for the residuals and random terms
# #' flat_priors <- list(
# #'      ## The residuals priors
# #'      R = list(
# #'          R1 = list(V = diag(3), nu = 0.002)), 
# #'      ## The random priors (the phylogenetic terms)
# #'      G = list(
# #'          G1 = list(V = diag(3), nu = 0.002),
# #'          G2 = list(V = diag(3), nu = 0.002),
# #'          G3 = list(V = diag(3), nu = 0.002),
# #'          G4 = list(V = diag(3), nu = 0.002)))
# #' 
# #' ## Run the model for 110000 iterations
# #' ## sampled every 100 with a burnin (discard)
# #' ## of the first 10000 iterations)
# #' model <- MCMCglmm(formula  = model_formula,
# #'                   rcov     = model_residual,
# #'                   random   = model_randoms,
# #'                   family   = rep("gaussian", 3),
# #'                   prior    = flat_priors,
# #'                   nitt     = 110000,
# #'                   burnin   = 10000,
# #'                   thin     = 100,
# #'                   pedigree = charadriiformes$tree,
# #'                   data     = charadriiformes$data)
# #' }
# # charadriiformes <- list(data = charadriiformes$data, tree = charadriiformes$tree, model = model)
# # save(charadriiformes, file = "../Data/charadriiformes.rda")
# NULL