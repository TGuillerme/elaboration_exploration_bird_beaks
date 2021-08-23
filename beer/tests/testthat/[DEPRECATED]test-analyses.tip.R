# ## Test
# data(model_list)
# data(morphdat)
# test_that("tip.results works", {
#     set.seed(1)
#     all_axes <- get.axes(get.covar(model_list[[7]], n = 4))
#     data <- morphdat
#     test <- analyses.tip(data, axes = all_axes[[1]])
#     expect_equal(names(test), c("projections", "rejections"))
#     expect_equal(dim(test[[1]]), c(359, 4))
#     expect_equal(rownames(test[[1]]), rownames(data))
#     expect_equal(dim(test[[2]]), c(359, 4))
#     expect_equal(rownames(test[[2]]), rownames(data))
# })
