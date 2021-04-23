## Test

data(model_list)
test_that("traits.MCMCglmm works", {

    ## Test data example
    for(i in 1:7) {
        expect_equal(traits.MCMCglmm(model_list[[i]]), c("PC1", "PC2", "PC3"))
    }

    ## Simple example
    data(PlodiaPO)  
    model1<-MCMCglmm(PO~1, random=~FSfamily, data=PlodiaPO, verbose=FALSE,
    nitt=1300, burnin=300, thin=1)
    expect_equal(traits.MCMCglmm(model1), "PO")
})
