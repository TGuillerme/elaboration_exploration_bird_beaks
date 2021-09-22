#' @title Set up parametersisation scripts
#'
#' @description 
#'
#' @param 
#' 
#' @examples
#'
#' @seealso
#' 
#' @author Thomas Guillerme
#' @export


set.parametrisation <- function(data, nitt = 50000, thin = 500, replicates = 3) {
    ## Get the data name
    data_name <- match.call()$data

    ## Loop through each element
    for(i in 1:length(data)) {
        ## Set the random levels (remove empty ones)
        if(length(empties <- which(unlist(lapply(data[[i]]$levels, length)) == 0)) > 0) {
            data[[i]]$levels[[empties]] <- NULL
        }

        ## Make the param chains
        param_chain <- make.mini.chains(
            data         = data[[i]]$space,
            dimensions   = data[[i]]$dimensions,
            tree         = data[[i]]$consensus_tree,
            trait.family = "gaussian",
            randoms      = c("global", colnames(data[[i]]$space)[1:length(data[[i]]$levels) + length(data[[i]]$dimensions)]),
            residuals    = "global",
            priors       = 0.02,
            verbose      = TRUE,
            parameters   = list(nitt   = nitt,
                                thin   = thin,
                                burnin = 0))
        ## Get the chain name
        chain_name <- names(data)[i]
        if(is.null(chain_name)) {
            chain_name <- ""
        } else {
            chain_name <- paste0(chain_name, "_")
        }
        chain_name <- paste0(data_name, "_", chain_name, "param")
        ## Save the parametrisation
        save(param_chain, file = paste0(chain_name, ".minichains"))

        ## Save the running script
        system("echo \"library(mcmcmcglmmm)\" > script")
        system("echo \"## Loading parametrisation\" >> script")
        system(paste0("echo \"load('", chain_name,".minichains')\" >> script"))
        system("echo \"## Running parametrisation\" >> script")
        system("echo \"start <- Sys.time()\" >> script")
        system(paste0("echo \"", chain_name, " <- run.mini.chains(param_chain, replicates = ", replicates, ")\" >> script"))
        system("echo \"## Save the results\" >> script")
        system(paste0("echo \"save(", chain_name, ", file = '",chain_name, ".rda')\" >> script"))
        system("echo \"end <- Sys.time()\" >> script")
        system("echo \"end - start\" >> script")
        system(paste0("mv script ", paste0(chain_name, ".R")))
    }
    return(NULL)
}

#load("../Data/Processed/shapespace_allbirds_lvl_superorder_order.rda")
#set.parametrisation(shapespace_allbirds_lvl_superorder_order)