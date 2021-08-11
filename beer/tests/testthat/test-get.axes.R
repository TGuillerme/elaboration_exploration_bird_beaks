## Test
data(model_list)
data(morphdat)
test_that("get.one.axis works", {
    set.seed(1)
    beer <- get.covar(model_list[[7]], n = 1)
    ## Right output
    expect_is(get.one.axis(beer[[1]][[1]]), "matrix")
    expect_equal(dim(get.one.axis(beer[[1]][[1]])), c(2,3))
    ## Selects the right axes
    test1 <- get.one.axis(beer[[1]][[1]], axis = 1)
    test2 <- get.one.axis(beer[[1]][[1]], axis = 2)
    test3 <- get.one.axis(beer[[1]][[1]], axis = 3)
    expect_true(all(test1 != test2))
    expect_true(all(test1 != test3))
    expect_true(all(test2 != test3))
    ## Levels works
    test2 <- get.one.axis(beer[[1]][[1]], level = 1)
    expect_true(all(is.nan(test2)))
    test2 <- get.one.axis(beer[[1]][[1]], level = 0.5)
    expect_true(all(test1 != test2))
    ## Dimensions selection works
    expect_equal(dim(get.one.axis(beer[[1]][[1]], dimensions = c(1,2))), c(2,2))

})

test_that("get.axes works", {
    test <- get.axes(get.covar(model_list[[7]], n = 7))
    ## Right output
    expect_equal(names(test), unname(MCMCglmm.levels(model_list[[7]])))
    expect_equal(length(test[[1]]), 7)
    expect_equal(unique(unlist(lapply(test[[1]], dim))), c(2, 3))
    ## Centreing works
    centre <- list(colMeans(morphdat[, c(1:3)]),
                   colMeans(morphdat[morphdat$clade == "gulls", c(1:3)]),
                   colMeans(morphdat[morphdat$clade == "plovers", c(1:3)]),
                   colMeans(morphdat[morphdat$clade == "sandpipers", c(1:3)]))


})
