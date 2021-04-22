## Test
test_that("init works", {

    ## Right output
    expect_equal(init(), "init!")
    expect_equal(init(42), "init!")
})
