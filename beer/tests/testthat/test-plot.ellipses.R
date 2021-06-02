## Test
data(model_list)
data(morphdat)
test_that("plot.ellipses works", {
    beer <- get.covar(model_list[[7]], n = 7)
    plot.space(morphdat, levels = morphdat$clade)
    expect_null(plot.ellipses(beer, transparent.scale = 0.5))
    expect_null(plot.ellipses(beer, centre = -1.5, add = FALSE))
    expect_null(plot.ellipses(beer, centre = median, add = FALSE))
})