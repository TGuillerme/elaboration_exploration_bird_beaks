## Testing the mini chains pipeline
data(morphdat)
data(tree)

test_that("flat.prior works", {

    ## Making a default flat priors for 2 traits
    test <- flat.prior(ntraits = 2)
    expect_is(test, "list")
    expect_equal(names(test), c("R", "G"))
    expect_equal(names(test[[1]]), c("R1"))
    expect_equal(names(test[[2]]), c("G1"))
    expect_equal(dim(test$G$G1$V), c(2,2))
    expect_equal(test$G$G1$nu, 0)

    test <- flat.prior(residuals = 1, randoms = 4, ntraits = 3, nu = 0.002)
    expect_is(test, "list")
    expect_equal(names(test), c("R", "G"))
    expect_equal(names(test[[1]]), c("R1"))
    expect_equal(names(test[[2]]), c("G1", "G2", "G3", "G4"))
    expect_equal(dim(test$G$G1$V), c(3,3))
    expect_equal(test$G$G1$nu, 0.002)
})

test_that("clade.terms works", {

    ## Testing clade terms is correct
    test <- clade.terms(7, type = "bob")
    expect_is(test, "formula")
    ## Fucking nestedness!
    expect_equal(as.character(test[[2]][[2]][[2]][[2]][[2]][[2]][[2]][[3]]), "bob")
    expect_equal(test[[2]][[2]][[2]][[2]][[2]][[2]][[2]][[2]][[2]][[2]][[3]], 1)
    expect_equal(as.character(test[[2]][[2]][[2]][[2]][[2]][[2]][[3]][[3]]), "bob")
    expect_equal(test[[2]][[2]][[2]][[2]][[2]][[2]][[3]][[2]][[2]][[2]][[3]], 2)
    expect_equal(as.character(test[[2]][[2]][[2]][[2]][[2]][[3]][[3]]), "bob")
    expect_equal(test[[2]][[2]][[2]][[2]][[2]][[3]][[2]][[2]][[2]][[3]], 3)
    expect_equal(as.character(test[[2]][[2]][[2]][[2]][[3]][[3]]), "bob")
    expect_equal(test[[2]][[2]][[2]][[2]][[3]][[2]][[2]][[2]][[3]], 4)
    expect_equal(as.character(test[[2]][[2]][[2]][[3]][[3]]), "bob")
    expect_equal(test[[2]][[2]][[2]][[3]][[2]][[2]][[2]][[3]], 5)
    expect_equal(as.character(test[[2]][[2]][[3]][[3]]), "bob")
    expect_equal(test[[2]][[2]][[3]][[2]][[2]][[2]][[3]], 6)
    expect_equal(as.character(test[[2]][[3]][[3]]), "bob")
    expect_equal(test[[2]][[3]][[2]][[2]][[2]][[3]], 7)
})

test_that("make.mini.chains works", {

#     short_run <- list(nitt = 100, burnin = 10, thin = 10)
#     flat_priors <- flat.prior(residuals = 1, randoms = 4, ntraits = 3, nu = 0.002)

#     ## make.mini.chains produces the right output
#     test <- make.mini.chains(parameters = short_run, prior = flat_priors, model = , dimensions = c(1:3), data = morphdat)
#     expect_is(test, c("beer", "mini.chains"))
#     expect_equal(length(test), 3)
#     expect_equal(names(test), c("data", "tree", "run"))
#     expect_is(test$run, "function")

#     ## Testing the run
#     tust <- test$run()
#     expect_is(tust, "MCMCglmm")


# random_ok <- ~ us(at.level(clade,1):trait):animal + us(at.level(clade,2):trait):animal + us(at.level(clade,3):trait):animal


# random_no <- ~ us(at.level(clade,1):trait):animal + us(at.level(clade,2):trait):animal + us(at.level(clade,3):trait):animal
# random_test <- random_ok
# random_test <- clade.terms(3, type = "animal")
# random_test <- 




# model7 <- MCMCglmm(cbind(PC1, PC2, PC3) ~ trait:clade-1, family = rep("gaussian", ntraits),
#                        random = random_test
#                          ,
#                       rcov=
#                          ~ us(trait):units,
#                        prior= flat_priors, pedigree = tree, data = morphdat, nitt = 1000, burnin = 100, thin = 100) 

})
