context("Testing 'pretty_print_string'")

test_that("it returns proper results", {
  res.1 = capture_output(pretty_print_string("string"))
  res.2 = capture_output(pretty_print_string("string", with.gt = FALSE))

  expect_equal(res.1, "> string")
  expect_equal(res.2, "string")
})

context("Testing 'pretty_print_bold_string'")

test_that("it returns proper results", {
  res.1 = capture_output(pretty_print_bold_string("string", html.output = FALSE))
  res.2 = capture_output(pretty_print_bold_string("string", with.gt = FALSE,
                                                  html.output = FALSE))
  res.3 = capture_output(pretty_print_bold_string("string", html.output = TRUE))
  res.4 = capture_output(pretty_print_bold_string("string", with.gt = FALSE,
                                                  html.output = TRUE))

  expect_equal(res.1, "> string")
  expect_equal(res.2, "string")
  expect_equal(res.3, "> <b>string</b>")
  expect_equal(res.4, "<b>string</b>")
})

context("Testing 'print_empty_line'")

test_that("it returns proper results", {
  res.1 = capture_output(print_empty_line())
  res.2 = capture_output(print_empty_line(html.output = TRUE))

  expect_equal(res.1, "")
  expect_equal(res.2, "<br/>")
})

context("Testing 'pretty_print_vector_names'")

test_that("it returns proper results", {
  vec = 1:3
  names(vec) = letters[1:3]

  res.1 = capture_output(pretty_print_vector_names(vec))
  res.2 = capture_output(pretty_print_vector_names(vec, sep = "",
                                                   vector.names.str = "letters",
                                                   with.gt = FALSE))
  res.3 = capture_output(pretty_print_vector_names(vec[1], sep = " ",
                                                   vector.names.str = "letters"))
  res.4 = capture_output(pretty_print_vector_names(c(), with.gt = FALSE,
                                                   vector.names.str = "letters"))

  expect_equal(res.1, "> 3 nodes: a, b, c")
  expect_equal(res.2, "3 letters: abc")
  expect_equal(res.3, "> 1 letter: a")
  expect_equal(res.4, "0 letters: ")
})

context("Testing 'pretty_print_vector_values'")

test_that("it returns proper results", {
  vec = 1:3

  res.1 = capture_output(pretty_print_vector_values(vec, vector.values.str = "numbers"))
  res.2 = capture_output(pretty_print_vector_values(vec[3], vector.values.str = "numbers"))

  expect_equal(res.1, "> 3 numbers: 1, 2, 3")
  expect_equal(res.2, "> 1 number: 3")
})

context("Testing 'pretty_print_vector_names_and_values'")

test_that("it returns proper results", {
  vec = 1:3
  names(vec) = letters[1:3]

  res.1 = capture_output(pretty_print_vector_names_and_values(vec))
  res.2 = capture_output(pretty_print_vector_names_and_values(vec[3]))
  res.3 = capture_output(pretty_print_vector_names_and_values(vec, n = 0))
  res.4 = capture_output(pretty_print_vector_names_and_values(vec, n = 1))
  res.5 = capture_output(pretty_print_vector_names_and_values(vec, n = 2))

  expect_equal(res.1, "> a: 1, b: 2, c: 3")
  expect_equal(res.2, "> c: 3")
  expect_equal(res.3, "> a: 1, b: 2, c: 3")
  expect_equal(res.4, "> a: 1")
  expect_equal(res.5, "> a: 1, b: 2")
})

context("Testing 'pretty_print_name_and_value'")

test_that("it returns proper results", {
  res.1 = capture_output(pretty_print_name_and_value("a", 1, with.gt = TRUE))
  res.2 = capture_output(pretty_print_name_and_value("b", 2, with.comma = FALSE))

  expect_equal(res.1, "> a: 1, ")
  expect_equal(res.2, "b: 2")
})
