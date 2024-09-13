test_that("it works", {
  expect_error(powerset_icounts(list()))
  expect_error(powerset_icounts(list(c(1,2,3))))
  expect_error(powerset_icounts(list(a = letters)))
  expect_error(powerset_icounts(list(a = 1, a = 2)))

  res = powerset_icounts(list(a = 1:3, b = 2:4, c = 1:5))
  expect_data_frame(res, nrows = 2^3 - 1, ncols = 4, any.missing = FALSE)
  i = which.max(res$num_subsets)
  expect_equal(res$num_subsets[i], 3)
  expect_equal(res$set_combo[i], "a-b-c")

  j = which.min(res$num_subsets)
  expect_equal(res$num_subsets[j], 1)

  ii = which.max(res$count)
  expect_equal(res$count[ii], 5)
  expect_equal(res$set_combo[ii], "c")
})
