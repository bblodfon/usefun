#' @title Compare two Precision-Recall curves
#'
#' @description
#' Test the hypothesis that the true difference in PR AUCs is equal to 0.
#' We implement the same bootstrap method based on the idea from [pROC::roc.test()].
#' The PR AUC is calculated using [PRROC::pr.curve()] with the interpolation
#' method of `r mlr3misc::cite_bib("davis2006")`.
#'
#' @param labels `numeric()`\cr
#' Vector of responses/labels (only two classes/values allowed: cases/positive
#' class = 1 and controls/negative class = 0)
#' @param pred1 `numeric()`\cr
#' Vector of prediction values. Higher values denote positive class.
#' @param pred2 `numeric()`\cr
#' Vector of prediction values. Higher values denote positive class.
#' Must have the same length as `pred1`.
#' @param boot.n `numeric(1)`\cr
#' Number of bootstrap resamples. Default: 10000
#' @param boot.stratified `logical(1)`\cr
#' Whether the bootstrap resampling is stratified (same number of cases/controls
#' in each replicate as in the original sample) or not.
#' It is advised to use stratified resampling when classes from `labels` are
#' imbalanced. Default: TRUE.
#' @param alternative `character(1)` \cr
#' Specifies the alternative hypothesis. Either "two.sided", "less" or "greater".
#' Default: "two.sided".
#'
#' @return a list with the AUCs of the two original prediction vectors and the
#' p-value of the bootstrap-based test.
#'
#' @references
#' `r mlr3misc::format_bib("davis2006")`
#'
#' @examples
#' set.seed(42)
#' # imbalanced labels
#' labels = sample(c(0,1), 20, replace = TRUE, prob = c(0.8,0.2))
#' # predictions
#' pred1 = rnorm(20)
#' pred2 = rnorm(20)
#' pr.test(labels, pred1, pred2, boot.n = 1000, boot.stratified = FALSE)
#' pr.test(labels, pred1, pred2, boot.n = 1000, boot.stratified = TRUE)
#'
#' @importFrom PRROC pr.curve
#' @importFrom stats pnorm
#' @export
pr.test = function(labels, pred1, pred2, boot.n = 10000, boot.stratified = TRUE,
  alternative = "two.sided") {
  checkmate::assert_choice(alternative,
                           choices = c("two.sided", "less", "greater"),
                           null.ok = FALSE)
  # 2 classes only (0 => neg, 1 => pos)
  stopifnot(all(sort(unique(labels)) == c(0,1))) # 2 classes only (0 => neg, 1 => pos)
  stopifnot(length(labels) == length(pred1))
  stopifnot(length(pred1)  == length(pred2))

  diffs = vapply(1:boot.n, function(i) {
    if (boot.stratified) {
      # get the two classes values
      cl1 = unique(labels)[1]
      cl2 = unique(labels)[2]
      # find indexes of those
      cl1_indxs = which(labels == cl1)
      cl2_indxs = which(labels == cl2)
      # resample with replacement each class on its own
      indx1 = sample(cl1_indxs, replace = TRUE)
      indx2 = sample(cl2_indxs, replace = TRUE)
      # combine to indx
      indx = c(indx1, indx2)
    } else {
      # if not stratified, you might end up with only one class
      # if the dataset is too imbalanced
      indx = sample(1:length(labels), replace = TRUE)
    }

    # resampled labels and prediction values
    rsmp_labels = labels[indx]
    rsmp_pred1  = pred1[indx]
    rsmp_pred2  = pred2[indx]

    # calculate the two PR AUCs: AUC1, AUC2
    auc1 = PRROC::pr.curve(scores.class0 = rsmp_pred1,
      weights.class0 = rsmp_labels)$auc.davis.goadrich
    auc2 = PRROC::pr.curve(scores.class0 = rsmp_pred2,
      weights.class0 = rsmp_labels)$auc.davis.goadrich

    # AUC diff
    auc1 - auc2
  }, numeric(1))

  # remove NA values if they exist
  diffs = diffs[!is.na(diffs)]

  # AUC1 and AUC2 are the PR AUCs on the original data
  auc1 = PRROC::pr.curve(scores.class0 = pred1,
    weights.class0 = labels)$auc.davis.goadrich
  auc2 = PRROC::pr.curve(scores.class0 = pred2,
    weights.class0 = labels)$auc.davis.goadrich
  # AUC difference
  obs_diff = auc1 - auc2
  # Calculate statistic
  stat = obs_diff / sd(diffs)

  # compare stat with normal distribution, according to the value of `alternative`
  # Alternative hypothesis: true difference in PR AUC is not equal to 0
  if (alternative == "two.sided")
    pval = 2 * pnorm(-abs(stat))
  else if (alternative == "greater")
    pval = pnorm(-stat)
  else # less
    pval = pnorm(stat)

  # return results
  list(
    auc1 = auc1,
    auc2 = auc2,
    p.value = pval
  )
}

#' @title Bootstrap Confidence Intervals for Precision-Recall Curves
#' @description
#' This functions calculates bootstrap percentile CIs for PR curves
#' using [precrec].
#' These can then be used in a plotting function, see example.
#'
#' @param labels `numeric()`\cr
#' Vector of responses/labels (only two classes/values allowed: cases/positive
#' class = 1 and controls/negative class = 0)
#' @param preds `numeric()`\cr
#' Vector of prediction values. Higher values denote positive class.
#' @param boot.n `numeric(1)`\cr
#' Number of bootstrap resamples. Default: 10000
#' @param boot.stratified `logical(1)`\cr
#' Whether the bootstrap resampling is stratified (same number of cases/controls
#' in each replicate as in the original sample) or not.
#' It is advised to use stratified resampling when classes from `labels` are
#' imbalanced. Default: TRUE.
#' @param alpha `numeric(1)`\cr
#' Confidence level for bootstrap percentile intervals (between 0 and 1).
#' Default is 0.1, corresponding to 90% confidence intervals.
#' @param ... \cr
#' Other parameters to pass on to [precrec::evalmod], except `mode`
#' (set to `rocpr`) and `raw_curves` (set to `TRUE`). For example `x_bins`
#' indicates the minimum number of recall points on the x-axis.
#' @return A tibble with columns:
#' - `recall`: recall of original data
#' - `precision`: precision of original data
#' - `low_precision`: low quantile value of the bootstrap confidence interval
#' - `high_precision`: high quantile value of the bootstrap confidence interval
#'
#' @references
#' `r mlr3misc::format_bib("saito2016")`
#'
#' @examples
#' set.seed(42)
#' # imbalanced labels
#' labels = sample(c(0,1), 100, replace = TRUE, prob = c(0.8,0.2))
#' # predictions
#' preds = rnorm(100)
#'
#' # get CIs for PR curve
#' pr_tbl = pr.boot(labels, preds, boot.n = 100, x_bins = 30)
#' pr_tbl
#'
#' # draw PR curve + add the bootstrap percentile confidence bands
#' library(ggplot2)
#'
#' pr_tbl |>
#'   ggplot(aes(x = recall, y = precision)) +
#'   geom_step() +
#'   ylim(c(0,1)) +
#'   geom_ribbon(aes(ymin = precision_low, ymax = precision_high), alpha = 0.2)
#'
#' @importFrom precrec mmdata
#' @importFrom precrec evalmod
#' @importFrom dplyr tibble
#' @importFrom tibble add_column
#' @importFrom stats quantile
#' @export
pr.boot = function(labels, preds, boot.n = 10000, boot.stratified = TRUE, alpha = 0.1, ...) {
  stopifnot(all(sort(unique(labels)) == c(0,1))) # 2 classes only (0 => neg, 1 => pos)
  stopifnot(alpha >= 0, alpha <= 1)
  checkmate::assert_numeric(preds)
  stopifnot(length(labels) == length(preds))

  # original PR values
  data_orig = precrec::mmdata(scores = preds, labels = labels, dsids = 1, posclass = 1)
  res_orig  = precrec::evalmod(mdat = data_orig, mode = "rocpr", raw_curves = TRUE, ...)
  recall_orig    = res_orig$prcs[[1]]$x
  precision_orig = res_orig$prcs[[1]]$y
  pr_tbl = tibble(recall = recall_orig, precision = precision_orig)

  # bootstrap data
  boot_data = lapply(1:boot.n, function(i) {
    if (boot.stratified) {
      # get the two classes values
      cl1 = unique(labels)[1]
      cl2 = unique(labels)[2]
      # find indexes of those
      cl1_indxs = which(labels == cl1)
      cl2_indxs = which(labels == cl2)
      # resample with replacement each class on its own
      indx1 = sample(cl1_indxs, replace = TRUE)
      indx2 = sample(cl2_indxs, replace = TRUE)
      # combine to indx
      indx = c(indx1, indx2)
    } else {
      # if not stratified, you might end up with only one class
      # if the dataset is too imbalanced
      indx = sample(1:length(labels), replace = TRUE)
    }

    # resampled labels and prediction values
    rsmp_labels = labels[indx]
    rsmp_preds  = preds[indx]

    list(labels = rsmp_labels, preds = rsmp_preds)
  })

  # get PR curves from bootstrap data
  labels_list = lapply(boot_data, function(obj) obj$labels)
  preds_list  = lapply(boot_data, function(obj) obj$preds)
  data = precrec::mmdata(scores = preds_list, labels = labels_list,
                         dsids = 1:length(labels_list), posclass = 1)
  res  = precrec::evalmod(mdat = data, mode = "rocpr", raw_curves = TRUE, ...)
  # recall = lapply(res$prcs, function(obj) obj$x) # don't need it
  precision = lapply(res$prcs, function(obj) obj$y)

  # Make precision vectors the same length as the original PR values
  nvals = nrow(pr_tbl)
  precision = lapply(precision, function(vec) {
    if (length(vec) < nvals) {
      # pad with last precision value
      c(vec, rep(vec[length(vec)], nvals - length(vec)))
    } else {
      # remove elements
      vec[1:nvals]
    }
  })

  precision_mat = do.call(rbind, precision) # boot.n x thres.n

  # calculate low and high bootstrap quantile intervals for the precision values
  low  = apply(precision_mat, 2, function(vec) {
    quantile(x = vec, probs = alpha/2)
  })
  high = apply(precision_mat, 2, function(vec) {
    quantile(x = vec, probs = 1 - (alpha/2))
  })

  pr_tbl |> tibble::add_column(precision_low = low, precision_high = high)
}
