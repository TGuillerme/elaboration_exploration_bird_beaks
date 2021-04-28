## Test
data(model_list)
data(morphdat)
test_that("plot.analyses.tip works", {
    set.seed(1)
    all_axes <- get.axes(get.covar(model_list[[7]], n = 4))
    data <- morphdat
    results <- analyses.tip(data, axes = all_axes[[1]])
    ## Basic
    expect_null(plot.analyses.tip(results))
    expect_null(plot.analyses.tip(results, what = "projections", main = "title", ylim = c(0, 10), col = "blue"))
    expect_null(plot.analyses.tip(results, group = split(rownames(morphdat), f = morphdat$clade)))

    ## With a tree
})
