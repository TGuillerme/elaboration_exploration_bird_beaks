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

    tree_list <- list(tree, tree, tree)
    class(tree_list) <- "multiPhylo"

    ## Model 3
    test <- make.mini.chains(data = morphdat, tree = tree_list, dimensions = c(1,2), verbose = FALSE)
    expect_is(test, c("beer", "mini.chains"))
    expect_equal(length(test), 3)
    expect_equal(names(test[[1]]), c("data", "tree", "run"))
    ## Run!
    tust <- test[[1]]$run()
    expect_is(tust, "MCMCglmm")
    expect_equal(paste0(as.character(tust$Fixed$formula), collapse = ""),
                "~cbind(PC1, PC2)trait - 1")
    expect_equal(paste0(as.character(tust$Random$formula), collapse = ""),
                "~us(trait):animal")
    expect_equal(paste0(as.character(tust$Residual$formula), collapse = ""),
                "~us(trait):units")

    ## Model 4
    priors_list <- flat.prior(ntraits = 3, randoms = 1, residuals = 3, nu = 0.1)
    test <- make.mini.chains(data = morphdat, tree = tree, dimensions = c(1:3), verbose = FALSE, residuals = "clade", priors = priors_list)
    expect_is(test, c("beer", "mini.chains"))
    expect_equal(length(test), 1)
    expect_equal(names(test[[1]]), c("data", "tree", "run"))
    ## Run!
    tust <- test[[1]]$run()
    expect_is(tust, "MCMCglmm")
    expect_equal(paste0(as.character(tust$Fixed$formula), collapse = ""),
                "~cbind(PC1, PC2, PC3)trait:clade - 1")
    expect_equal(paste0(as.character(tust$Random$formula), collapse = ""),
                "~us(trait):animal")
    expect_equal(paste0(as.character(tust$Residual$formula), collapse = ""),
                "~us(at.level(clade, 1):trait):units + us(at.level(clade, 2):trait):units + us(at.level(clade, 3):trait):units")
    
    ## Model 5
    test <- make.mini.chains(data = morphdat, tree = tree, dimensions = c(1:3), verbose = FALSE, residuals = "clade", randoms = "clade")
    expect_is(test, c("beer", "mini.chains"))
    expect_equal(length(test), 1)
    expect_equal(names(test[[1]]), c("data", "tree", "run"))
    ## Run!
    tust <- test[[1]]$run()
    expect_is(tust, "MCMCglmm")
    expect_equal(paste0(as.character(tust$Fixed$formula), collapse = ""),
                "~cbind(PC1, PC2, PC3)trait:clade - 1")
    expect_equal(paste0(as.character(tust$Random$formula), collapse = ""),
                "~us(at.level(clade, 1):trait):animal + us(at.level(clade, 2):trait):animal + us(at.level(clade, 3):trait):animal")
    expect_equal(paste0(as.character(tust$Residual$formula), collapse = ""),
                "~us(at.level(clade, 1):trait):units + us(at.level(clade, 2):trait):units + us(at.level(clade, 3):trait):units")

    ## Model 6
    test <- make.mini.chains(data = morphdat, tree = tree, dimensions = c(1:3), verbose = FALSE, residuals = "clade", randoms = c("global", "clade"))
    expect_is(test, c("beer", "mini.chains"))
    expect_equal(length(test), 1)
    expect_equal(names(test[[1]]), c("data", "tree", "run"))
    ## Run!
    tust <- test[[1]]$run()
    expect_is(tust, "MCMCglmm")
    expect_equal(paste0(as.character(tust$Fixed$formula), collapse = ""),
                "~cbind(PC1, PC2, PC3)trait:clade - 1")
    expect_equal(paste0(as.character(tust$Random$formula), collapse = ""),
                "~us(at.level(clade, 1):trait):animal + us(at.level(clade, 2):trait):animal + us(at.level(clade, 3):trait):animal + us(trait):animal")
    expect_equal(paste0(as.character(tust$Residual$formula), collapse = ""),
                "~us(at.level(clade, 1):trait):units + us(at.level(clade, 2):trait):units + us(at.level(clade, 3):trait):units")


    ## Model 7
    test <- make.mini.chains(data = morphdat, tree = tree, dimensions = c(1:3), verbose = FALSE, residuals = "global", randoms = c("global", "clade"))
    expect_is(test, c("beer", "mini.chains"))
    expect_equal(length(test), 1)
    expect_equal(names(test[[1]]), c("data", "tree", "run"))
    ## Run!
    tust <- test[[1]]$run()
    expect_is(tust, "MCMCglmm")
    expect_equal(paste0(as.character(tust$Fixed$formula), collapse = ""),
                "~cbind(PC1, PC2, PC3)trait:clade - 1")
    expect_equal(paste0(as.character(tust$Random$formula), collapse = ""),
                "~us(at.level(clade, 1):trait):animal + us(at.level(clade, 2):trait):animal + us(at.level(clade, 3):trait):animal + us(trait):animal")
    expect_equal(paste0(as.character(tust$Residual$formula), collapse = ""),
                "~us(trait):units")
})
