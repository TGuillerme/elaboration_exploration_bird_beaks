## Internal for sauron.plot and covar.utilties
## Selecting n elements from a covar object
sample.n <- function(covar, n, selected_n) {
    ## Get the posterior size
    n_post <- length(covar[[1]])
    if(!missing(n) && n == n_post) {
        return(covar)
    } else {
        ## Sample n
        if(missing(selected_n)) {
            selected_n <- sample(1:n_post, n, replace = n > n_post)
        }
        ## Return n for each group
        return(lapply(covar, function(group, n) group[n], n = selected_n))
    }
}