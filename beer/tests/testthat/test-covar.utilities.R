## Test
require(MCMCglmm)
data(model_list)
set.seed(1)
data(PlodiaPO)
model_simple<-MCMCglmm(PO~1, random=~FSfamily, data=PlodiaPO, verbose=FALSE,
nitt=1300, burnin=300, thin=1)

test_that("get.covar works", {
    ## Works on a dispRity object
    data_test <- MCMCglmm.dispRity(data = PlodiaPO, posteriors = model_simple)

    ## works on a MCMCglmm object (basically calls MCMCglmm.covars)
    test <- get.covar(data_test)
    expect_equal(length(test), length(MCMCglmm.levels(model_simple)))
    expect_equal(length(test[[1]]), 1000)
    ## n argument works
    test <- get.covar(data_test, n = 7)
    expect_equal(length(test), length(MCMCglmm.levels(model_simple)))
    expect_equal(length(test[[1]]), 7)
    ## sample argument works
    test <- get.covar(data_test, sample = c(42, 5))
    expect_equal(length(test), length(MCMCglmm.levels(model_simple)))
    expect_equal(length(test[[1]]), 2)
    ## Only takes the n argument
    warn <- capture_warnings(test <- get.covar(data_test, sample = 42, n = 7))
    expect_equal(warn[[1]], "sample argument is ignored since n = 7 random samples are asked for.")
    expect_equal(length(test), length(get.covar(data_test)))
    expect_equal(length(test[[1]]), 7)
})

## Test
test_that("ellipse.covar works", {
})

## Test
test_that("axis.covar works", {
})
