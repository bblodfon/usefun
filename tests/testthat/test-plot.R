context("Testing plot functions")
test_that("plot objects are created and returned", {
  color.vector = rainbow(10)
  number.vector = 1:10
  title = "First 10 rainbow() colors"
  expect_true(is.matrix(make_color_bar_plot(color.vector, number.vector, title)))
  dev.off()

  mat = matrix(rnorm(60), ncol=20)
  densities = apply(mat, 1, density)
  names(densities) = c("1st", "2nd", "3rd")
  expect_true(is.list(make_multiple_density_plot(densities, legend.title = "Samples",
    x.axis.label = "", title = "3 Normal Distribution Samples")))
  dev.off()
})
