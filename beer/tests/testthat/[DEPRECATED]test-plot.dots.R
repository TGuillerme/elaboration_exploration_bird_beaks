# ## Test
# test_that("plot.dots (internal) works", {

#     ## Everything default
#     plot_args <- plot.dots()
#     expect_null(plot_args$x)
#     expect_null(plot_args$y)
#     expect_equal(plot_args$lwd, 1)
#     expect_equal(plot_args$pch, 19)
#     expect_equal(plot_args$lty, 1)
#     expect_equal(plot_args$cex, 0.5)

#     ## Works with default title limits and labels
#     data_dummy <- rnorm(50)
#     plot_args <- plot.dots(data = data_dummy, main_default = "ho", dimensions = c(1,2))
#     expect_null(plot_args$x)
#     expect_null(plot_args$y)
#     expect_equal(plot_args$lwd, 1)
#     expect_equal(plot_args$pch, 19)
#     expect_equal(plot_args$lty, 1)
#     expect_equal(plot_args$cex, 0.5)
#     expect_equal(plot_args$xlim, range(data_dummy))
#     expect_equal(plot_args$ylim, range(data_dummy))
#     expect_equal(plot_args$main, "ho")
#     expect_equal(plot_args$xlab, "Dim. 1")
#     expect_equal(plot_args$ylab, "Dim. 2")

#     ## Work with all the options (and the ignored ones)
#     plot_args <- plot.dots(x = data_dummy, lwd = "hi", pch = "ho", lty = 42, cex = 21, something = "yes")
#     expect_null(plot_args$x)
#     expect_null(plot_args$y)
#     expect_equal(plot_args$lwd, "hi")
#     expect_equal(plot_args$pch, "ho")
#     expect_equal(plot_args$lty, 42)
#     expect_equal(plot_args$cex, 21)
#     expect_equal(plot_args$something, "yes")
# })
