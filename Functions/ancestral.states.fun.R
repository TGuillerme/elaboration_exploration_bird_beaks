
## Function for running the ace on the whole matrix (i.e. for all the traits)
one.ace <- function(traits, tree) {
    cat(".")
    results <- ace(traits, tree)
    results$labels <- tree$node.label
    return(results)
}
run.full.ace <- function(tree, matrix) {
    cat("Estimating one matrix:")
    ace_results <- apply(matrix, 2, one.ace, tree)
    cat("Done\n")
    return(ace_results)
}

## Function for extracting the ace values of interest
#' @param ace the output from run.full.ace
#' @param what either "max" for the most likely value, or any integer for a number of random values (e.g. 1 or 10)
#' @param sampling the sampling distribution can be "uniform" (default) or "normal"0
#' 
## Extracting random values from the ace_results 95% (uniform or normal)
fun.uniform <- function(ci95, what) {
    return(runif(what, min = ci95[1], max = ci95[2]))
}
fun.normal <- function(ci95, what) {
    return(rnorm(what, mean = mean(ci95), sd = diff(ci95)/4))
}
extract.ace <- function(ace_result, what, sampling = "uniform", matrix) {
    ## Switching between both type of results
    if(what == "max") {
        ## Extracting the most likely states
        node_states <- do.call(cbind, lapply(ace_result, function(x) return(x$ace)))
        rownames(node_states) <- ace_result[[1]]$labels
        return(rbind(matrix, node_states))
    } else {
        ## Toggle the sampling function
        fun.sample <- ifelse(sampling == "uniform", fun.uniform, fun.normal)
        trait_results <- lapply(ace_result, function(trait, fun.sample, what) apply(trait$CI95, 1, fun.sample, what = what), fun.sample, what)

        if(what == 1) {
            ## Return single samples
            node_states <- do.call(cbind, trait_results)
            rownames(node_states) <- ace_result[[1]]$labels
            return(rbind(matrix, node_states))            
        } else {
            ## Return multiple samples
            node_states <- list()
            for(i in 1:what) {
                node_states[[i]] <- do.call(cbind, lapply(trait_results, function(X, i) return(X[i, ]), i))
                rownames(node_states[[i]]) <- ace_result[[1]]$labels
            }
            return(lapply(node_states, function(X, matrix) rbind(matrix, X), matrix))
        }
    }
}

# ##DEBUG
# matrix <- matrix(rnorm(100), 10, 5)
# trees <- rmtree(7, 10)
# trees <- lapply(trees, function(tree) {tree$node.label <- paste0("n", 1:Nnode(tree)); return(tree)})
# class(trees) <- "multiPhylo"
# tree <- trees[[1]]
# rownames(matrix) <- tree$tip.label

# ##DEBUG
# all_results <- lapply(trees, run.full.ace, matrix)
# ace_result <- all_results[[1]]

# ## DEBUG
# all_trees_max_lik <- lapply(all_results, extract.ace, what = "max", matrix = matrix)