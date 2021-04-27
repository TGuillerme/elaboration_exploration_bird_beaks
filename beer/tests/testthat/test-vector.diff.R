data(model_list)
test_that("vector.diff works", {
    set.seed(1)
    vec <- get.axes(get.covar(model_list[[7]]))
    ## Returns works
    test <- vector.diff(vec$animal[[1]], vec$units[[1]], return = c("rejection", "projection"))
    expect_equal(names(test), c("projection", "rejection"))

    ## Results works
    test <- vector.diff(vec$animal[[1]], vec$units[[1]])
    expect_equal(names(test), c("angle", "projection", "rejection"))
    expect_equal(unname(round(test["angle"], 3)), 6.106)
    expect_equal(unname(round(test["projection"], 3)), 2.249)
    expect_equal(unname(round(test["rejection"], 3)), 0.241)

    ## Scaling works
    test <- vector.diff(vec$animal[[1]], vec$units[[1]], scale = FALSE)
    expect_equal(names(test), c("angle", "projection", "rejection"))
    expect_equal(unname(round(test["angle"], 3)), 6.106)
    expect_equal(unname(round(test["projection"], 3)), 1.277)
    expect_equal(unname(round(test["rejection"], 3)), 0.455)
})
