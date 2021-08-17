## Test
test_that("MCMCglmm.dispRity and sauron.plot works", {
    ## Testing the mini chains pipeline
    load("covar_model_list.Rda")
    load("covar_char_data.Rda")
    load("covar_tree_data.Rda")

    ## Sanitizing
    # data class
    # posteriors class
    # group (optional)
    # tree (optional)
    # rename.groups (optional)

    ## Model 1: 1 group residual
    test <- MCMCglmm.dispRity(data = covar_char_data, posteriors =covar_model_list[[1]])
    expect_is(test, "dispRity")
    expect_equal(length(test), 5)
    expect_equal(names(test), c("matrix", "tree", "call", "subsets", "MCMCglmm"))
    expect_equal(dim(test$matrix[[1]]), c(359, 17))
    expect_equal(test$call$dimensions, c(1,2,3))
    expect_equal(test$call$subsets, "MCMCglmm")
    expect_null(test$tree[[1]])
    expect_equal(names(test$subsets), "units")
    expect_equal(names(test$subsets[[1]]), "elements")
    expect_equal(names(test$MCMCglmm), c("formula", "covars"))
    expect_equal(names(test$MCMCglmm$formula), c("Fixed", "Random", "Residual"))
    expect_equal(paste0(as.character(test$MCMCglmm$formula$Fixed), collapse = ""), "~cbind(PC1, PC2, PC3)trait - 1")
    expect_null(test$MCMCglmm$formula$Random)
    expect_equal(paste0(as.character(test$MCMCglmm$formula$Residual), collapse = ""), "~us(trait):units")
    expect_equal(length(test$MCMCglmm$covars), 1)
    expect_equal(length(test$MCMCglmm$covars[[1]]), 1000)
    expect_equal(names(test$MCMCglmm$covars[[1]][[1]]), c("VCV", "Sol"))

    ## Default plot
    expect_null(sauron.plot(test))
    expect_null(sauron.plot(test, n = 3, ellipses = TRUE, main = "something", level = 0.5, legend = TRUE, legend.pos = c(0.5, -0.5), pch = 12))

    ## Model 2: 3 group residual
    test <- MCMCglmm.dispRity(data = covar_char_data, posteriors =covar_model_list[[2]])
    expect_equal(length(test$MCMCglmm$covars), 3)
    expect_equal(names(test$MCMCglmm$covars), c("units:clade1", "units:clade2", "units:clade3"))
    expect_null(sauron.plot(test, col = rainbow(3), points = FALSE, ellipses = mean))

    ## Model 3: 2 group (res/rand)
    test <- MCMCglmm.dispRity(data = covar_char_data, posteriors =covar_model_list[[3]])
    expect_equal(length(test$MCMCglmm$covars), 2)
    expect_equal(names(test$MCMCglmm$covars), c("animal", "units"))
    expect_null(sauron.plot(test, col = rainbow(2), points = FALSE, major.axes = "all", n = 20, legend = TRUE))
    
    ## Model 4: 4 group (3/1)
    test <- MCMCglmm.dispRity(data = covar_char_data, posteriors =covar_model_list[[4]])
    expect_equal(length(test$MCMCglmm$covars), 4)
    expect_equal(names(test$MCMCglmm$covars), c("animal", "units:clade1", "units:clade2", "units:clade3"))
    expect_null(sauron.plot(test, col = rainbow(4), points = TRUE, major.axes = "all", n = 20, legend = TRUE, ellipses = mean, pch = 21))

    ## Model 5: 6 group (3/3)
    test <- MCMCglmm.dispRity(data = covar_char_data, posteriors =covar_model_list[[5]])
    expect_equal(length(test$MCMCglmm$covars), 6)
    expect_equal(names(test$MCMCglmm$covars), c("animal:clade1", "animal:clade2", "animal:clade3", "units:clade1", "units:clade2", "units:clade3"))
    expect_null(sauron.plot(test, col = rainbow(6), points = FALSE, major.axes = mean, legend = TRUE, ellipses = mean, pch = 21))

    ## Model 6: 6 group (3/4)
    test <- MCMCglmm.dispRity(data = covar_char_data, posteriors =covar_model_list[[6]])
    expect_equal(length(test$MCMCglmm$covars), 7)
    expect_equal(names(test$MCMCglmm$covars), c("animal:clade1", "animal:clade2", "animal:clade3", "animal", "units:clade1", "units:clade2", "units:clade3"))
    expect_null(sauron.plot(test, ellipses = mean))

    ## Model 7: 5 group (1/4)
    test <- MCMCglmm.dispRity(data = covar_char_data, posteriors =covar_model_list[[7]])
    expect_equal(length(test$MCMCglmm$covars), 5)
    expect_equal(names(test$MCMCglmm$covars), c("animal:clade1", "animal:clade2", "animal:clade3", "animal", "units"))
    
    test <- MCMCglmm.dispRity(data = covar_char_data, posteriors =covar_model_list[[7]], group = c(random = "animal", random = "animal:clade1", random = "animal:clade2", random = "animal:clade3"), rename.groups = c("phylogeny", "clade1", "clade2", "clade3"))
    expect_equal(size.subsets(test), c("phylogeny" = 359, "clade1" = 160, "clade2" = 97, "clade3" = 102))
    expect_equal(length(test$MCMCglmm$covars), 4)
    expect_equal(names(test$MCMCglmm$covars), c("phylogeny", "clade1", "clade2", "clade3"))
    expect_null(sauron.plot(test, ellipses = mean, major.axes = "all", n = 100, col = c("grey","orange", "blue", "darkgreen"), legend = TRUE, points = TRUE, cex = 0.2))

    # Try with not all subsets selected on a big model
    test <- MCMCglmm.dispRity(data = covar_char_data, posteriors = covar_model_list[[7]], group = c(random = "animal", residual = "units"))
    expect_equal(length(test$MCMCglmm$covars), 2)
    expect_equal(names(test$MCMCglmm$covars), c("animal", "units"))
    
    # Try with correct groups renaming
    test <- MCMCglmm.dispRity(data = covar_char_data, posteriors = covar_model_list[[7]], rename.groups = c("clade1", "clade2", "clade3", "phylo", "residual"))
    expect_equal(length(test$MCMCglmm$covars), 5)
    expect_equal(names(test$MCMCglmm$covars), c("clade1", "clade2", "clade3", "phylo", "residual"))
})
