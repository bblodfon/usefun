context("Testing 'get_conf_mat_for_thres'")

test_that("it returns proper results", {
  predictions = c(-3, -2, -1, 0, 1)
  observed    = c(1, 0, 1, 0, 0)

  res = get_conf_mat_for_thres(predictions, observed, thres = -4)
  expect_equal(res[["TP"]], 0)
  expect_equal(res[["FN"]], 2)
  expect_equal(res[["TN"]], 3)
  expect_equal(res[["FP"]], 0)

  res = get_conf_mat_for_thres(predictions, observed, thres = -1.5)
  expect_equal(res[["TP"]], 1)
  expect_equal(res[["FN"]], 1)
  expect_equal(res[["TN"]], 2)
  expect_equal(res[["FP"]], 1)

  res = get_conf_mat_for_thres(predictions, observed, thres = 0.5)
  expect_equal(res[["TP"]], 2)
  expect_equal(res[["FN"]], 0)
  expect_equal(res[["TN"]], 1)
  expect_equal(res[["FP"]], 2)

  res = get_conf_mat_for_thres(predictions, observed, thres = 2)
  expect_equal(res[["TP"]], 2)
  expect_equal(res[["FN"]], 0)
  expect_equal(res[["TN"]], 0)
  expect_equal(res[["FP"]], 3)
})

context("Testing 'get_roc_stats'")

test_that("it returns proper results", {
  predictions = c(-3, -2, -1, 0, 1)
  observed    = c(1, 0, 1, 0, 0)

  roc_df = cbind.data.frame(predictions, observed)
  res = get_roc_stats(df = roc_df, pred_col = "predictions", obs_col = "observed")

  expect_equal(res$AUC, 0.83333, tolerance = .00001)
})
