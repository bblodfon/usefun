test_that("pr.test works", {
  set.seed(1)
  labels = sample(c(0,1), 10, replace = TRUE, prob = c(0.8,0.2))
  p1 = rnorm(10)
  p2 = rnorm(10)
  expect_silent(pr.test(labels, p1, p2, boot.n = 50, boot.stratified = FALSE))
  expect_silent(pr.test(labels, p1, p2, boot.n = 50, boot.stratified = FALSE,
    alternative = "less"))
  expect_silent(pr.test(labels, p1, p2, boot.n = 50, boot.stratified = FALSE,
    alternative = "greater"))
  expect_silent(pr.test(labels, p1, p2, boot.n = 50, boot.stratified = TRUE))
})

test_that("pr.boot works", {
  set.seed(1)
  labels = sample(c(0,1), 10, replace = TRUE, prob = c(0.8,0.2))
  preds = rnorm(10)

  res = pr.boot(labels, preds, boot.n = 10, x_bins = 30)
  expect_equal(class(res)[1], "tbl_df")
  expect_equal(colnames(res), c("recall", "precision", "precision_low", "precision_high"))
  expect_silent(pr.boot(labels, preds, boot.n = 10, x_bins = 30, boot.stratified = FALSE))
})
