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
})

test_that("optim.replicate works", {


})
