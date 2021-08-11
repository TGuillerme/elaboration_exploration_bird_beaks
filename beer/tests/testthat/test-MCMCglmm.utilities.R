## Test
require(MCMCglmm)
data(model_list)
set.seed(1)
data(PlodiaPO)
model_simple<-MCMCglmm(PO~1, random=~FSfamily, data=PlodiaPO, verbose=FALSE,
nitt=1300, burnin=300, thin=1)

test_that("MCMCglmm.traits works", {
    ## Simple example
    expect_equal(MCMCglmm.traits(model_simple), "PO")

    ## Test data example
    for(i in 1:7) {
        expect_equal(MCMCglmm.traits(model_list[[i]]), c("PC1", "PC2", "PC3"))
    }
})

test_that("MCMCglmm.levels works", {

    ## Simple example
    expect_equal(MCMCglmm.levels(model_simple), c("random" = "FSfamily", "residual" = "units"))
    expect_equal(MCMCglmm.levels(model_simple, convert = FALSE), c("random" = "FSfamily", "residual" = "units"))

    ## Test data example
    expect_equal(MCMCglmm.levels(model_list[[1]]), c("residual" = "units"))
    expect_equal(MCMCglmm.levels(model_list[[2]]), c("residual" = "units:clade1", "residual" = "units:clade2", "residual" = "units:clade3"))
    expect_equal(MCMCglmm.levels(model_list[[3]]), c("random" = "animal", "residual" = "units"))
    expect_equal(MCMCglmm.levels(model_list[[3]], convert = FALSE), c("random" = "us(trait):animal", "residual" = "us(trait):units"))
    expect_equal(MCMCglmm.levels(model_list[[4]]), c("random" = "animal", "residual" = "units:clade1", "residual" = "units:clade2", "residual" = "units:clade3"))
    expect_equal(MCMCglmm.levels(model_list[[5]]), c("random" = "animal:clade1", "random" = "animal:clade2", "random" = "animal:clade3", "residual" = "units:clade1", "residual" = "units:clade2", "residual" = "units:clade3"))
    expect_equal(MCMCglmm.levels(model_list[[6]]), c("random" = "animal:clade1", "random" = "animal:clade2", "random" = "animal:clade3", "random" = "animal", "residual" = "units:clade1", "residual" = "units:clade2", "residual" = "units:clade3"))
    expect_equal(MCMCglmm.levels(model_list[[7]]), c("random" = "animal:clade1", "random" = "animal:clade2", "random" = "animal:clade3", "random" = "animal", "residual" = "units"))
    expect_equal(MCMCglmm.levels(model_list[[7]], convert = FALSE), c("random" = "us(at.level(clade, 1):trait):animal ", "random" = " us(at.level(clade, 2):trait):animal ", "random" = " us(at.level(clade, 3):trait):animal ", "random" = " us(trait):animal", "residual" = "us(trait):units"))
})

test_that("MCMCglmm.sample works", {
    test1 <- MCMCglmm.sample(model_simple)
    expect_equal(test1, seq(1:1000))
    set.seed(1)
    test2 <- MCMCglmm.sample(model_simple, n = 10)
    expect_equal(test2, c(836, 679, 129, 930, 509, 471, 299, 270, 978, 187))
    warn <- capture_warnings(test3 <- MCMCglmm.sample(model_simple, n = 1001))
    expect_equal(length(test3), 1001)
    expect_equal(warn[[1]], "The required number of samples 1001 is larger than the available number of samples 1000. Some samples will be used more than once.")
})

test_that("MCMCglmm.covars works", {
    
})