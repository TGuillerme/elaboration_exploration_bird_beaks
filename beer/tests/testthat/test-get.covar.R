## Test
require(MCMCglmm)
data(model_list)
set.seed(1)
data(PlodiaPO)
model_simple<-MCMCglmm(PO~1, random=~FSfamily, data=PlodiaPO, verbose=FALSE,
nitt=1300, burnin=300, thin=1)

test_that("traits.MCMCglmm works", {
    ## Simple example
    expect_equal(traits.MCMCglmm(model_simple), "PO")

    ## Test data example
    for(i in 1:7) {
        expect_equal(traits.MCMCglmm(model_list[[i]]), c("PC1", "PC2", "PC3"))
    }
})

test_that("levels.MCMCglmm works", {

    ## Simple example
    expect_equal(levels.MCMCglmm(model_simple), c("random" = "FSfamily", "residual" = "units"))
    expect_equal(levels.MCMCglmm(model_simple, convert = FALSE), c("random" = "FSfamily", "residual" = "units"))

    ## Test data example
    expect_equal(levels.MCMCglmm(model_list[[1]]), c("residual" = "units"))
    expect_equal(levels.MCMCglmm(model_list[[2]]), c("residual" = "units:clade1", "residual" = "units:clade2", "residual" = "units:clade3"))
    expect_equal(levels.MCMCglmm(model_list[[3]]), c("random" = "animal", "residual" = "units"))
    expect_equal(levels.MCMCglmm(model_list[[3]], convert = FALSE), c("random" = "us(trait):animal", "residual" = "us(trait):units"))
    expect_equal(levels.MCMCglmm(model_list[[4]]), c("random" = "animal", "residual" = "units:clade1", "residual" = "units:clade2", "residual" = "units:clade3"))
    expect_equal(levels.MCMCglmm(model_list[[5]]), c("random" = "animal:clade1", "random" = "animal:clade2", "random" = "animal:clade3", "residual" = "units:clade1", "residual" = "units:clade2", "residual" = "units:clade3"))
    expect_equal(levels.MCMCglmm(model_list[[6]]), c("random" = "animal:clade1", "random" = "animal:clade2", "random" = "animal:clade3", "random" = "animal", "residual" = "units:clade1", "residual" = "units:clade2", "residual" = "units:clade3"))
    expect_equal(levels.MCMCglmm(model_list[[7]]), c("random" = "animal:clade1", "random" = "animal:clade2", "random" = "animal:clade3", "random" = "animal", "residual" = "units"))
    expect_equal(levels.MCMCglmm(model_list[[7]], convert = FALSE), c("random" = "us(at.level(clade, 1):trait):animal ", "random" = " us(at.level(clade, 2):trait):animal ", "random" = " us(at.level(clade, 3):trait):animal ", "random" = " us(trait):animal", "residual" = "us(trait):units"))
})

test_that("get.one.covar/get.covar works", {

    ## Checking the correct structure in the nested lists
    correct.structure <- function(X) {expect_equal(length(X), 2); expect_is(X$VCV, "matrix"); expect_is(X$Sol, "numeric")}

    test <- get.one.covar(model_simple, levels.MCMCglmm(model_simple), traits.MCMCglmm(model_simple))
    expect_is(test, "list")
    expect_equal(names(test), unname(levels.MCMCglmm(model_simple)))
    silent <- lapply(test, correct.structure)

    ## Everything works!
    for(i in 1:7) {
        test <- get.one.covar(model_list[[i]],
              levels.MCMCglmm(model_list[[i]]),
              traits.MCMCglmm(model_list[[i]]))
        expect_is(test, "list")
        expect_equal(names(test), unname(levels.MCMCglmm(model_list[[i]])))
        silent <- lapply(test, correct.structure)
    }

    ## replicate bit works
    test <- get.covar(model_list[[1]], n = 7)
    expect_is(test, "beer")
    expect_equal(length(test), 7)

    test <- get.covar(model_list[[7]], n = 7)
    expect_is(test, "beer")
    expect_equal(length(test), 7)
})