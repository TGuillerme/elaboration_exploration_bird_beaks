## Test
data(model_list)
test_that("plot.analyses.group works", {
    set.seed(1)
    axes <- get.axes(get.covar(model_list[[4]], n = 4))
    results <- analyses.group(axes)
    expect_null(plot.analyses.group(results))
    expect_null(plot.analyses.group(results, col = "black"))
    expect_null(plot.analyses.group(results, main = "black"))
    expect_null(plot.analyses.group(results, main = "black", what = c("angle", "rejection")))
})
