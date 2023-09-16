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
