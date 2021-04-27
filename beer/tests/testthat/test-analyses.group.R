
data(model_list)
## Test
test_that("group.results works", {
    set.seed(1)
    axes <- get.axes(get.covar(model_list[[7]], n = 4))
    test <- analyses.group(axes)
    expect_equal(names(test), unname(levels.MCMCglmm(model_list[[7]])[-1]))
    test <- analyses.group(axes, base = 3)
    expect_equal(names(test), unname(levels.MCMCglmm(model_list[[7]])[-3]))
    test <- analyses.group(axes, base = "animal")
    expect_equal(names(test), unname(levels.MCMCglmm(model_list[[7]])[-4]))
    test <- analyses.group(axes, groups = c(1, 2, 3), return = c("angle", "projection"))
    expect_equal(names(test), unname(levels.MCMCglmm(model_list[[7]])[1:3]))
    expect_is(test[[1]], "matrix")
    expect_equal(dim(test[[1]]), c(2, 4))
    expect_equal(rownames(test[[1]]), c("angle", "projection"))
})
