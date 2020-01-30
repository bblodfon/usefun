context("Testing 'normalize_to_range'")

test_that("it returns proper results", {
  vec = 1:10
  mat = matrix(c(0,2,1), ncol = 3, nrow = 4)

  res.1 = normalize_to_range(vec)
  expected.res.1 = seq(0, 1, 0.11111111)

  res.2 = normalize_to_range(vec, range = c(1,100))
  expected.res.2 = c(1, 12, 23, 34, 45, 56, 67, 78, 89, 100)

  res.3 = normalize_to_range(vec, range = c(-1,1))
  expected.res.3 = seq(-1, 1, 0.22222222)

  res.4 = normalize_to_range(mat)
  expected.res.4 = matrix(c(0.0, 1.0, 0.5), ncol = 3, nrow = 4)

  res.5 = normalize_to_range(as.data.frame(mat))

  expect_equal(res.1, expected.res.1, tolerance = 0.0000001)
  expect_equal(res.2, expected.res.2, tolerance = 0.0000001)
  expect_equal(res.3, expected.res.3, tolerance = 0.0000001)
  expect_equal(res.4, expected.res.4, tolerance = 0.0000001)
  expect_equal(res.5, as.data.frame(expected.res.4), tolerance = 0.0000001)
})

context("Testing 'specify_decimal'")

test_that("it returns proper results", {
  expect_equal(specify_decimal(number = 0.12342, digits.to.keep = 6), "0.123420")
  expect_equal(specify_decimal(number = 56.12342, digits.to.keep = 5), "56.12342")
  expect_equal(specify_decimal(number = 0.12342, digits.to.keep = 4), "0.1234")
  expect_equal(specify_decimal(number = 0.12342, digits.to.keep = 3), "0.123")
  expect_equal(specify_decimal(number = 0.12342, digits.to.keep = 2), "0.12")
  expect_equal(specify_decimal(number = 0.12342, digits.to.keep = 1), "0.1")
  expect_equal(specify_decimal(number = 3.12342, digits.to.keep = 0), "3")
})

context("Testing 'remove_commented_and_empty_lines'")

test_that("it removes commented lines", {
  lines = c("aaa", "#comment1", "bbb", "#comment2", "ccc")

  res = remove_commented_and_empty_lines(lines)
  expected.res = c("aaa", "bbb", "ccc")

  expect_equal(res, expected.res)
})

test_that("it removes empty lines", {
  lines = c("aaa", "", "bbb", "    ", "ccc", "\t  \t")

  res = remove_commented_and_empty_lines(lines)
  expected.res = c("aaa", "bbb", "ccc")

  expect_equal(res, expected.res)
})

test_that("it removes both commented and empty lines", {
  lines = c("aaa", "#comment1", "\t", "#comment2", "ccc", "")

  res = remove_commented_and_empty_lines(lines)
  expected.res = c("aaa", "ccc")

  expect_equal(res, expected.res)
})

context("Testing 'get_parent_dir'")

test_that("it returns proper results", {
  res.1 = get_parent_dir("/home/john")
  expected.res.1 = "/home"

  res.2 = get_parent_dir("/home/john/a.txt")
  expected.res.2 = "/home/john"

  res.3 = get_parent_dir("/home")
  expected.res.3 = "/"

  res.4 = get_parent_dir("/")
  expected.res.4 = "/"

  res.5 = get_parent_dir("")
  expected.res.5 = "/"

  expect_equal(res.1, expected.res.1)
  expect_equal(res.2, expected.res.2)
  expect_equal(res.3, expected.res.3)
  expect_equal(res.4, expected.res.4)
  expect_equal(res.5, expected.res.5)
})

context("Testing 'mat_equal'")

test_that("it returns proper results", {
  a = matrix(0, nrow = 3, ncol = 3)
  a2 = matrix(0, nrow = 3, ncol = 3)
  b = matrix(0, nrow = 2, ncol = 3)
  c = matrix(0, nrow = 3, ncol = 2)
  d = 1:10
  e = matrix(1, nrow = 3, ncol = 3)

  expect_true(mat_equal(a, a2))
  expect_false(mat_equal(a, b))
  expect_false(mat_equal(a, c))
  expect_false(mat_equal(a, d))
  expect_false(mat_equal(a, e))
})

context("Testing 'is_empty'")

test_that("it returns proper results", {
  expect_true(is_empty(NULL))
  expect_true(is_empty(c()))

  expect_false(is_empty(""))
  expect_false(is_empty(NA))
  expect_false(is_empty(NaN))
  expect_false(is_empty(1:10))
})

context("Testing 'outersect'")

test_that("it returns proper results", {
  x = 1:3
  y = 2:5
  w = 1

  expect_equal(outersect(x,y), c(1,4,5))
  expect_equal(outersect(x,w), c(2,3))
  expect_equal(outersect(y,w), c(1,2,3,4,5))
  expect_length(outersect(x,x), 0) # or is_empty = TRUE
  expect_length(outersect(w,w), 0)
})

context("Testing 'is_between'")

test_that("it returns proper results", {
  low.thres = -0.23456
  high.thres = 1.26346

  res.1 = is_between(value = 0, low.thres, high.thres)
  res.2 = is_between(value = -1, low.thres, high.thres)
  res.3 = is_between(value = 1, low.thres, high.thres)
  res.4 = is_between(value = low.thres, low.thres, high.thres)
  res.5 = is_between(value = low.thres, low.thres, high.thres,
                     include.high.value = TRUE)
  res.6 = is_between(value = high.thres, low.thres, high.thres)
  res.7 = is_between(value = high.thres, low.thres, high.thres,
                     include.high.value = TRUE)

  expect_true(res.1)
  expect_false(res.2)
  expect_true(res.3)
  expect_true(res.4)
  expect_true(res.5)
  expect_false(res.6)
  expect_true(res.7)
})
