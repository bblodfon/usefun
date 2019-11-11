context("Testing 'get_common_names'")

test_that("it returns proper results", {
  vec1 = 1:3
  vec2 = 1:5
  vec3 = 1
  names(vec1) = c("t", "y", "r")
  names(vec2) = c("y", "w", "o", "r", "l")
  names(vec3) = "yut"
  expected.names = c("y", "r")

  sink(tempfile())
  expect_equal(get_common_names(vec1, vec2), expected.names)
  expect_false(get_common_names(vec1, vec3))
  expect_false(get_common_names("", ""))
  sink()
})

context("Testing 'get_common_values'")

test_that("it returns proper results", {
  vec1 = 1:3
  vec2 = 1:5
  vec3 = 7:10

  sink(tempfile())
  expect_equal(get_common_values(vec1, vec2), c(1,2,3))
  expect_true(is.null(get_common_values(vec1, vec3)))
  expect_true(is.null(get_common_values(vec1, "")))
  sink()
})

context("Testing 'get_stats_for_unique_values'")

test_that("it returns proper results when there is a unique value in vec1", {
  vec1 = c(rep(1,10))
  vec2 = c(rep(2,10))
  names.vec = c(seq(1,10))
  names(vec1) = names.vec
  names(vec2) = names.vec

  res = get_stats_for_unique_values(vec1, vec2)
  expected.res = cbind(1,2,0)
  colnames(expected.res) = c("vec1.unique", "vec2.mean", "vec2.sd")

  expect_equal(res, as.data.frame(expected.res))
})

test_that("it returns proper results when there are 2 unique values in vec1", {
  vec1 = c(rep(c(1,2),5))
  vec2 = c(rep(2,5), rep(3,5))
  names.vec = c(seq(1,10))
  names(vec1) = names.vec
  names(vec2) = names.vec

  res = get_stats_for_unique_values(vec1, vec2)
  expected.res = cbind(c(1,2), c(2.4, 2.6), c(0.5477226, 0.5477226))
  colnames(expected.res) = c("vec1.unique", "vec2.mean", "vec2.sd")

  expect_equal(res, as.data.frame(expected.res), tolerance = .0000001)
})

test_that(paste("it returns proper results when there are ",
                "3 unique values out of 4 in vec1", sep = ""), {
  vec1 = c(1, 2, 3, 2)
  vec2 = c(20, 2, 2.5, 8)
  names.vec = c(seq(1,4))
  names(vec1) = names.vec
  names(vec2) = names.vec

  res = get_stats_for_unique_values(vec1, vec2)
  expected.res = cbind(c(1,2,3), c(20,5,2.5), c(0,4.2426407,0))
  colnames(expected.res) = c("vec1.unique", "vec2.mean", "vec2.sd")

  expect_equal(res, as.data.frame(expected.res), tolerance = .00000001)
})

context("Testing 'get_percentage_of_matches'")

test_that("it returns proper results", {
  a = c(1,1,1,1)
  b = c(1,0,1,1)
  c = c(0,0,1,1)
  d = c(1,0,0,0)
  e = c(0,0,0,0)
  f = c(1, 2, 3, 2)
  g = c(20, 2, 2.5, 8)
  h = c(1, 2, 333, 222)
  k = c(2, 333, 222, 1)

  res.1 = get_percentage_of_matches(a,a)
  res.2 = get_percentage_of_matches(a,b)
  res.3 = get_percentage_of_matches(a,c)
  res.4 = get_percentage_of_matches(a,d)
  res.5 = get_percentage_of_matches(a,e)
  res.6 = get_percentage_of_matches(f,g)
  res.7 = get_percentage_of_matches(f,h)
  res.8 = get_percentage_of_matches(h,k)

  a[4] = NA
  a[3] = NaN
  res.9 = get_percentage_of_matches(a,b)
  res.10 = get_percentage_of_matches(a,a)

  res = c(res.1, res.2, res.3, res.4, res.5, res.6, res.7, res.8, res.9, res.10)

  expected.res.1 = 1
  expected.res.2 = 0.75
  expected.res.3 = 0.5
  expected.res.4 = 0.25
  expected.res.5 = 0
  expected.res.6 = 0.25
  expected.res.7 = 0.5
  expected.res.8 = 0
  expected.res.9 = 0.25
  expected.res.10 = 0.5

  expected.res = c(expected.res.1, expected.res.2, expected.res.3,
                   expected.res.4, expected.res.5, expected.res.6,
                   expected.res.7, expected.res.8, expected.res.9,
                   expected.res.10)

  expect_equal(res, expected.res)
})

context("Testing 'prune_columns_from_df'")

test_that("it returns proper results", {
  df.1 = data.frame(c(0,0,0), c(0,1,0), c(1,0,0), c(0,0,0))
  df.2 = data.frame(c(2,2,2), c(2,1,0), c(1,0,1), c(1,1,1))
  df.3 = data.frame()

  res.1 = prune_columns_from_df(df.1, value = 0)
  res.2 = prune_columns_from_df(df.2, value = 2)
  res.3 = prune_columns_from_df(res.2, value = 1)
  res.4 = prune_columns_from_df(df.3, value = 69)

  expected.res.1 = data.frame(c(0,1,0), c(1,0,0))
  expected.res.2 = data.frame(c(2,1,0), c(1,0,1), c(1,1,1))
  expected.res.3 = data.frame(c(2,1,0), c(1,0,1))

  expect_equal(res.1, expected.res.1)
  expect_equal(res.2, expected.res.2)
  expect_equal(res.3, expected.res.3)
  expect_equal(res.4, df.3)
})

context("Testing 'prune_rows_from_df'")

test_that("it returns proper results", {
  df.1 = data.frame(c(0,0,0), c(0,1,0), c(1,0,0), c(0,0,0))
  df.2 = data.frame(c(2,2,1), c(0,2,1))
  df.3 = data.frame()
  colnames(df.1) = 1:length(colnames(df.1))
  colnames(df.2) = 1:length(colnames(df.2))

  res.1 = prune_rows_from_df(df.1, value = 0)
  res.2 = prune_rows_from_df(df.2, value = 2)
  rownames(res.2) = 1:length(rownames(res.2))
  res.3 = prune_rows_from_df(res.2, value = 1)
  res.4 = prune_rows_from_df(res.3, value = 10)
  res.5 = prune_rows_from_df(df.3, value = 69)

  expected.res.1 = data.frame(c(0,0), c(0,1), c(1,0), c(0,0))
  expected.res.2 = data.frame(c(2,1), c(0,1))
  expected.res.3 = data.frame(c(2), c(0))
  expected.res.4 = expected.res.3
  colnames(expected.res.1) = 1:length(colnames(expected.res.1))
  colnames(expected.res.2) = 1:length(colnames(expected.res.2))
  colnames(expected.res.3) = 1:length(colnames(expected.res.3))
  colnames(expected.res.4) = 1:length(colnames(expected.res.4))

  expect_equal(res.1, expected.res.1)
  expect_equal(res.2, expected.res.2)
  expect_equal(res.3, expected.res.3)
  expect_equal(res.4, expected.res.4)
  expect_equal(res.5, df.3)
})

context("Testing 'add_vector_to_df'")

test_that("it returns proper results", {
  df = data.frame(c(0,0,1), c(0,0,2))
  colnames(df) = c(1,2)
  vec = 1:5
  names(vec) = c("a","b","c","d","e")

  expected.res.df = data.frame(c("0","0","1","a","b","c","d","e"),
                               c("0","0","2","1","2","3","4","5"),
                               stringsAsFactors = FALSE)
  colnames(expected.res.df) = c(1,2)

  res.df = add_vector_to_df(df, vec)
  res.df.2 = add_vector_to_df(df, c()) # add nothing

  expect_equal(res.df, expected.res.df)
  expect_equal(res.df.2, df)
})

context("Testing 'prune_and_reorder_vector'")

test_that("it returns proper results", {
  a = c(1,2,3,4,5,6)
  names(a) = c("MPAK","ADER","FEK2","AKT","OLGA","BAD")
  nodes = c("AKT","MPAK","FEK2","BAD")

  res = prune_and_reorder_vector(a, nodes)
  expected.res = c(4,1,3,6)
  names(expected.res) = c("AKT","MPAK","FEK2","BAD")
  expect_equal(res, expected.res)

  nodes.2 = c("ADER","FEK2","AKT","OLGA","no","BAD")
  res.2 = prune_and_reorder_vector(a, nodes.2)
  expected.res.2 = c(2,3,4,5,6)
  names(expected.res.2) = c("ADER","FEK2","AKT","OLGA","BAD")
  expect_equal(res.2, expected.res.2)

  filter.vec = c("", "no", "yes")
  res.3 = prune_and_reorder_vector(a, filter.vec)
  expect_true(length(res.3) == 0)
})

context("Testing 'get_ternary_class_id'")

test_that("it returns proper results", {
  expect_equal(get_ternary_class_id(value = -6, threshold = 5), -1)
  expect_equal(get_ternary_class_id(value = -4, threshold = 5), 0)
  expect_equal(get_ternary_class_id(value = 0, threshold = 5), 0)
  expect_equal(get_ternary_class_id(value = 3, threshold = 5), 0)
  expect_equal(get_ternary_class_id(value = 4, threshold = 5), 0)
  expect_equal(get_ternary_class_id(value = 7, threshold = 5), 1)

  expect_equal(get_ternary_class_id(value = 0.78, threshold = 0.77), 1)
  expect_equal(get_ternary_class_id(value = 0.33, threshold = 0.77), 0)
  expect_equal(get_ternary_class_id(value = -0.33, threshold = 0.77), 0)
  expect_equal(get_ternary_class_id(value = -0.88, threshold = 0.77), -1)
})

context("Testing 'add_row_to_ternary_df'")

test_that("it returns proper results", {
  df = data.frame(c(0,-1,0), c(0,1,-1), c(1,0,0), c(0,0,0))
  colnames(df) = c("A","B","C","D")

  df.1 = add_row_to_ternary_df(df, values.pos = c(), values.neg = c())
  df.2 = add_row_to_ternary_df(df, values.pos = c(), values.neg = c(), pos = "first", row.name = "AAAA")
  df.3 = add_row_to_ternary_df(df, values.pos = c(), values.neg = c(), pos = "last",  row.name = "NULL")
  expect_equal(as.numeric(df.1[1,]), c(0,0,0,0))
  expect_equal(rownames(df.1)[1], "1")
  expect_equal(as.numeric(df.2[1,]), c(0,0,0,0))
  expect_equal(rownames(df.2)[1], "AAAA")
  expect_equal(as.numeric(df.3[4,]), c(0,0,0,0))
  expect_equal(rownames(df.3)[4], "NULL")

  df.4 = add_row_to_ternary_df(df, values.pos = c("A","B"), values.neg = c("C","D"))
  expect_equal(as.numeric(df.4[1,]), c(1,1,-1,-1))

  df.5 = add_row_to_ternary_df(df, values.pos = c("A"), values.neg = c())
  expect_equal(as.numeric(df.5[1,]), c(1,0,0,0))

  # values.pos %in% col.names
  expect_error(add_row_to_ternary_df(df, values.pos = c("no","yes"), values.neg = c("C","D")))
})

context("Testing 'ldf_arrange_by_rownames'")

test_that("it returns proper results", {
  df.1 = data.frame(matrix(data = 0, nrow = 3, ncol = 3, dimnames =
                             list(c("row1", "row2", "row3"), c("C.1", "C.2", "C.3"))))
  df.2 = data.frame(matrix(data = 1, nrow = 3, ncol = 3, dimnames =
                             list(c("row1", "row2", "row4"), c("C.1", "C.2", "C.3"))))

  list_df = list(df.1, df.2)
  names(list_df) = c("zeros", "ones")

  res_list_df = ldf_arrange_by_rownames(list_df)

  expect_equal(length(res_list_df), 4)
  expect_equal(names(res_list_df), c("row1", "row2", "row3", "row4"))
  expect_equal(as.numeric(res_list_df[["row1"]]["zeros", ]),
               as.numeric(list_df[["zeros"]]["row1", ]))
  expect_equal(as.numeric(res_list_df[["row2"]]["ones", ]),
               as.numeric(list_df[["ones"]]["row2", ]))
})

context("Testing 'binarize_to_thres'")

test_that("it returns proper results", {
  mat = matrix(data = -4:4, nrow = 3, ncol = 3)

  expected.mat.1 = matrix(data = c(1,1,1,1,0,1,1,1,1), nrow = 3, ncol = 3)
  expected.mat.2 = matrix(data = c(1,1,1,0,0,0,1,1,1), nrow = 3, ncol = 3)
  expected.mat.3 = matrix(data = c(1,1,0,0,0,0,0,1,1), nrow = 3, ncol = 3)
  expected.mat.4 = matrix(data = c(1,0,0,0,0,0,0,0,1), nrow = 3, ncol = 3)
  expected.mat.5 = matrix(data = 0, nrow = 3, ncol = 3)

  expect_equal(binarize_to_thres(mat, thres = 0.5), expected.mat.1)
  expect_equal(binarize_to_thres(mat, thres = 1.5), expected.mat.2)
  expect_equal(binarize_to_thres(mat, thres = 2.5), expected.mat.3)
  expect_equal(binarize_to_thres(mat, thres = 3.5), expected.mat.4)
  expect_equal(binarize_to_thres(mat, thres = 5), expected.mat.5)
})
