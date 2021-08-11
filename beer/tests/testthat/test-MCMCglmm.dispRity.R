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

    # Try with not all subsets selected on a big model

    # Try with correct groups renaming
})
