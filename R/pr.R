#' @title Compare two Precision-Recall curves
#'
#' @description
#' Test the hypothesis that the true difference in PR AUCs is equal to 0.
#' We implement the same bootstrap method based on the idea from [pROC::roc.test()].
#' The PR AUC is calculated using [PRROC::pr.curve()] using the interpolation
#' method of Davis & Goadrich (2006).
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
#' Whether the boostrap resampling is stratified (same number of cases/controls
#' in each replicate as in the original sample) or not.
#' Advised to use epsecially when classes from `labels` are imbalanced.
#' Default: TRUE.
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
  match.arg(alternative, c("two.sided", "less", "greater"))
  stopifnot(all(sort(unique(labels)) == c(0,1))) # 2 classes only (0 => neg, 1 => pos)
  stopifnot(length(pred1) == length(pred2))

  diffs = sapply(1:boot.n, function(i) {
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
  })

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
