## Test
test_that("MCMCglmm.dispRity works", {
    ## Testing the mini chains pipeline
    load("covar_model_list.Rda")
    load("covar_char_data.Rda")
    load("covar_tree_data.Rda")


    # ## Sanitizing
    # expect_error(MCMCglmm.dispRity(data = covar_char_data,
    #                               posteriors = covar_model_list[[5]],
    #                               tree = covar_tree_data))

    # ## Right output
    # expect_is(
    #     MCMCglmm.dispRity()
    #     , "class")
    # expect_equal(
    #     dim(MCMCglmm.dispRity())
    #     , dim)
})
