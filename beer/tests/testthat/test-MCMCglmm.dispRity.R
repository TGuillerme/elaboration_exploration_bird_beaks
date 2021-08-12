## Test
test_that("MCMCglmm.dispRity works", {
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

    ## Working well
    # Output has the right content (data, subsets, trees, subset "MCMCglmm" class)
    # For dispRity: output has the dispRity class

    # Try on all the models

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

    ## Model 2: 3 group residual
    test <- MCMCglmm.dispRity(data = covar_char_data, posteriors =covar_model_list[[2]])
    expect_equal(length(test$MCMCglmm$covars), 3)
    expect_equal(names(test$MCMCglmm$covars), c("units:clade1", "units:clade2", "units:clade3"))

    ## Model 3: 2 group (res/rand)
    test <- MCMCglmm.dispRity(data = covar_char_data, posteriors =covar_model_list[[3]])
    expect_equal(length(test$MCMCglmm$covars), 2)
    expect_equal(names(test$MCMCglmm$covars), c("animal", "units"))
    
    ## Model 4: 4 group (3/1)
    test <- MCMCglmm.dispRity(data = covar_char_data, posteriors =covar_model_list[[4]])
    expect_equal(length(test$MCMCglmm$covars), 4)
    expect_equal(names(test$MCMCglmm$covars), c("animal", "units:clade1", "units:clade2", "units:clade3"))

    ## Model 5: 6 group (3/3)
    test <- MCMCglmm.dispRity(data = covar_char_data, posteriors =covar_model_list[[5]])
    expect_equal(length(test$MCMCglmm$covars), 6)
    expect_equal(names(test$MCMCglmm$covars), c("animal:clade1", "animal:clade2", "animal:clade3", "units:clade1", "units:clade2", "units:clade3"))

    ## Model 6: 6 group (3/4)
    test <- MCMCglmm.dispRity(data = covar_char_data, posteriors =covar_model_list[[6]])
    expect_equal(length(test$MCMCglmm$covars), 7)
    expect_equal(names(test$MCMCglmm$covars), c("animal:clade1", "animal:clade2", "animal:clade3", "animal", "units:clade1", "units:clade2", "units:clade3"))

    ## Model 7: 5 group (1/4)
    test <- MCMCglmm.dispRity(data = covar_char_data, posteriors =covar_model_list[[7]])
    expect_equal(length(test$MCMCglmm$covars), 5)
    expect_equal(names(test$MCMCglmm$covars), c("animal:clade1", "animal:clade2", "animal:clade3", "animal", "units"))

    # Try with not all subsets selected on a big modelw

    # Try with correct groups renaming
})
