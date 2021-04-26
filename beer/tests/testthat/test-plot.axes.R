## Test
data(model_list)
data(morphdat)
test_that("plot.axes works", {
    beer <- get.covar(model_list[[7]], n = 7)
    plot.space(morphdat, levels = morphdat$clade)
    axes <- get.axes(beer)
    expect_is(axes, "list")
    expect_null(plot.axes(axes))
    expect_null(plot.axes(axes, use.transparent = FALSE, add = FALSE))
    expect_null(plot.axes(axes, add = FALSE))
})