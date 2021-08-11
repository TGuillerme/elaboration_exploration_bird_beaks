## Test
require(MCMCglmm)
data(model_list)
set.seed(1)
data(PlodiaPO)
model_simple<-MCMCglmm(PO~1, random=~FSfamily, data=PlodiaPO, verbose=FALSE,
nitt=1300, burnin=300, thin=1)

test_that("get.one.covar/get.covar works", {

    ## Checking the correct structure in the nested lists
    correct.structure <- function(X) {expect_equal(length(X), 2); expect_is(X$VCV, "matrix"); expect_is(X$Sol, "numeric")}

    test <- get.one.covar(model_simple, MCMCglmm.levels(model_simple), MCMCglmm.traits(model_simple))
    expect_is(test, "list")
    expect_equal(names(test), unname(MCMCglmm.levels(model_simple)))
    silent <- lapply(test, correct.structure)

    ## Everything works!
    for(i in 1:7) {
        test <- get.one.covar(model_list[[i]],
              MCMCglmm.levels(model_list[[i]]),
              MCMCglmm.traits(model_list[[i]]))
        expect_is(test, "list")
        expect_equal(names(test), unname(MCMCglmm.levels(model_list[[i]])))
        silent <- lapply(test, correct.structure)
    }

    ## replicate bit works
    test <- get.covar(model_list[[1]], n = 7)
    expect_is(test, "beer")
    expect_equal(length(test), length(MCMCglmm.levels(model_list[[1]])))
    expect_equal(length(test[[1]]), 7)
    test <- get.covar(model_list[[7]], n = 7)
    expect_is(test, "beer")
    expect_equal(length(test), length(MCMCglmm.levels(model_list[[7]])))
    expect_equal(length(test[[1]]), 7)
})