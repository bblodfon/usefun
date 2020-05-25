context("Testing save_* functions")
test_that("data is written properly", {
  vec = c(2,3,2,1)
  names(vec) = LETTERS[1:4]

  temp_file = tempfile()
  save_vector_to_file(vector = vec, file = temp_file, with.row.names = TRUE)

  vec_as_df = read.table(temp_file, stringsAsFactors = FALSE)
  expect_equal(dim(vec_as_df), c(4,2))
  expect_equal(vec_as_df[,1], LETTERS[1:4])
  expect_equal(vec_as_df[,2], unname(vec))

  save_df_to_file(df = vec_as_df, file = temp_file)
  df = read.table(temp_file, stringsAsFactors = FALSE)

  expect_true(all.equal(vec_as_df, df, check.attributes = FALSE))

  save_mat_to_file(mat = as.matrix(vec_as_df), file = temp_file)
  df2 = read.table(temp_file, stringsAsFactors = FALSE)

  expect_true(all.equal(vec_as_df, df2, check.attributes = FALSE))
})

