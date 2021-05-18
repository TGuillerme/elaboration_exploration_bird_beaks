## Test
test_that("verbose function works", {

    test <- function() mean(c(1,2,3))
    message <- capture_message(res <- test())
    expect_equal(res, 2)
    expect_null(message)
    test2 <- eval.verbose(test)
    message <- capture_message(res <- test2())
    expect_equal(res, 2)
    expect_equal(message[[1]], ".")
    test3 <- eval.verbose(test, msg = "what?")
    message <- capture_message(res <- test3())
    expect_equal(res, 2)
    expect_equal(message[[1]], "what?")

    set.seed(1)
    test <- function() rnorm(1)
    message <- capture_message(res <- test())
    expect_equal(round(res, 2), -0.63)
    expect_null(message)
    test2 <- eval.verbose.count(test, count = 0)
    message <- capture_message(res <- test2())
    expect_equal(message[[1]], "1.")
    message <- capture_message(res <- test2())
    expect_equal(message[[1]], "2.")
    test3 <- eval.verbose.count(test, count = 41)
    message <- capture_message(res <- test3())
    expect_equal(message[[1]], "42.")
})

test_that("optim.replicate works", {
    set.seed(1)
    fun1 <- function() return(sample(1:100, 1))
    fun2 <- function() return(c("a" = sample(1:100, 1), "b" = sample(1:100, 1)))
    diagnose.fun <- function(x) var(x)

    ## Verbose test
    messages <- capture_messages(test <- optim.replicate(fun2, diagnose.fun, minimum = 100, verbose = TRUE, stop.variance = 0.001, increment = .05))
    expect_equal(length(messages), 1518)
    expect_equal(messages[[1]], "Running the initial 100 replicates:")
    expect_equal(messages[[2]], "1.")
    expect_equal(messages[[102]], "Done.")
    expect_equal(messages[[103]], "\nRunning an additional 5 replicates:")
    expect_equal(messages[[104]], "101.")
    expect_equal(messages[[110]], "\nDiagnosis change:0.011, -0.022")
    expect_equal(messages[[111]], "\nRunning an additional 6 replicates:")
    expect_equal(messages[[1518]], "\nResults converged after 1359 iterations: additional variances (-0.001, 0) all < 0.001.")
    expect_equal(length(test$outputs), 1359)
    expect_equal(dim(test$results), c(1359, 2))
    expect_equal(dim(test$diagnoses), c(54, 2))

    set.seed(1)
    ## Verbose test
    test <- optim.replicate(fun2, diagnose.fun, minimum = 100, maximum = 200, verbose = FALSE, stop.variance = 0.001, increment = .05)
    expect_equal(length(test$outputs), 206)


    ## Test with a simple function
    set.seed(1)
    test <- optim.replicate(fun1, diagnose.fun)
    expect_equal(length(test$outputs), 21)

    ## Test with a function that outputs two terms
    set.seed(23)
    test <- optim.replicate(fun2, diagnose.fun)
    expect_equal(length(test$outputs), 23)

    ## Test with a function thaty outputs a list (summary function needed)
    set.seed(42)
    fun3 <- function() return(list("a" = sample(1:100, 1), "b" = sample(1:100, 1), "c" = sample(1:10000, 1)))
    sum.fun <- function(x) return(c(x$a, x$b))
    ## Error
    expect_error(test <- optim.replicate(fun3, diagnose.fun))
    ## Works!
    test <- optim.replicate(fun3, diagnose.fun, summarise = sum.fun)
    expect_equal(length(test$outputs), 21)
    expect_equal(dim(test$results), c(21, 2))
    expect_equal(length(test$outputs[[1]]), 3)
})

test_that("optim.replicate backup works", {
    fun1 <- function() return(sample(1:100, 1))
    fun2 <- function() return(c("a" = sample(1:100, 1), "b" = sample(1:100, 1)))
    diagnose.fun <- function(x) var(x)    
    set.seed(2)
    test <- optim.replicate(fun2, diagnose.fun, minimum = 100, verbose = FALSE, stop.variance = 0.001, increment = .05, bkp.path = "./", bkp.name = "tust")
    expect_equal(list.files(path = "./", pattern = "tust"), "tust")
    expect_equal(length(test$outputs), 7907)
    set.seed(2)
    test2 <- optim.replicate(fun2, diagnose.fun, minimum = 100, verbose = FALSE, stop.variance = 0.001, increment = .05, bkp.path = "./", bkp.name = "tust")
    expect_equal(length(test2$outputs), 7907)
    expect_true(file.remove("./tust"))
})