## Test
data(model_list)
data(morphdat)
test_that("tip.results works", {
    set.seed(1)
    all_axes <- get.axes(get.covar(model_list[[7]], n = 4))
    data <- morphdat
    test <- analyses.tip(data, all_axes[[1]])
    expect_equal(names(test), c("projections", "rejections"))
    expect_equal(length(test[[1]]), 4)
    expect_equal(length(test[[1]][[1]]), dim(data)[1])
    expect_equal(length(test[[2]]), 4)
    expect_equal(length(test[[2]][[1]]), dim(data)[1])
})
