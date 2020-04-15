#' Generate ROC statistics
#'
#' Use this function to generate the most useful statistics related to the
#' generation of a basic ROC (Receiver Operating Characteristic) curve.
#'
#' @param df a \code{data.frame} with (at least) two columns. See next two
#' parameters for what values these two columns should have (which should match
#' one to one).
#' @param pred_col string. The name of the column of the \code{df} data.frame
#' that has the prediction values. The values can be any numeric, negative,
#' positive or zero. What matters is the \strong{ranking} of these values and
#' \strong{lower} means closer to a \emph{positive} classification/prediction (the
#' class labeled as \emph{1} in the \code{obs_col}). Thus, if for example the
#' \code{pred_col} values represent probabilities of positive predictions, i.e.
#' a higher probability indicates a positive classification (class labeled
#' as \emph{1}), then the complementary (1-\code{pred_col}) values must be provided.
#' @param obs_col string. The name of the column of the \code{df} data.frame
#' that has the true positive labelings/observed classes for the
#' prediction values. This column should have either \emph{1} or \emph{0}
#' elements representing either a \emph{positive} or \emph{negative} classification
#' for the corresponding values.
#'
#' @return A list with two elements:
#' \itemize{
#'   \item \code{roc.stats}: a \code{tibble} which includes the \strong{thresholds}
#'   for the ROC curve and the \strong{confusion matrix stats} for each threshold as
#'   follows: \emph{TP} (#True Positives), \emph{FN} (#False Negatives),
#'   \emph{TN} (#True Negatives), \emph{FP} (#False Positives),
#'   \emph{FPR} (False Positive Rate - the x-axis values for the ROC curve) and
#'   \emph{TPR} (True Positive Rate - the y-axis values for the ROC curve).
#'   Also included are the \emph{dist-from-chance} (the vertical distance of
#'   the corresponding (FPR,TPR) point to the chance line or positive diagonal)
#'   and the \emph{dist-from-0-1} (the euclidean distance of the corresponding
#'   (FPR,TPR) point from (0,1)).
#'   \item \code{AUC}: a number representing the Area Under the (ROC) Curve.
#' }
#'
#' The returned results provide an easy way to compute two optimal \emph{cutpoints}
#' (thresholds) that dichotomize the predictions to positive and negative.
#' The first is the \emph{Youden index}, which is the maximum vertical distance from the
#' ROC curve to the chance line or positive diagonal. The second is the point
#' of the ROC curve closest to the (0,1) - the point of perfect differentiation.
#' See examples below.
#'
#' @examples
#' # load libraries
#' library(readr)
#' library(dplyr)
#'
#' # load test tibble
#' test_file = system.file("extdata", "test_df.tsv", package = "usefun", mustWork = TRUE)
#' test_df = readr::read_tsv(test_file, col_types = "di")
#'
#' # get ROC stats
#' res = get_roc_stats(df = test_df, pred_col = "score", obs_col = "observed")
#'
#' # Plot ROC with a legend showing the AUC value
#' plot(x = res$roc_stats$FPR, y = res$roc_stats$TPR,
#'   type = 'l', lwd = 2, col = '#377EB8', main = 'ROC curve',
#'   xlab = 'False Positive Rate (FPR)', ylab = 'True Positive Rate (TPR)')
#' legend('bottomright', legend = round(res$AUC, digits = 3),
#'   title = 'AUC', col = '#377EB8', pch = 19)
#' grid()
#' abline(a = 0, b = 1, col = '#FF726F', lty = 2)
#'
#' # Get two possible cutoffs
#' youden_index_df = res$roc_stats %>%
#'   filter(dist_from_chance == max(dist_from_chance))
#' min_classification_df = res$roc_stats %>%
#'   filter(dist_from_0_1 == min(dist_from_0_1))
#'
#' @importFrom dplyr %>% pull as_tibble
#' @export
get_roc_stats = function(df, pred_col, obs_col) {
  # checks
  stopifnot(ncol(df) >= 2, pred_col %in% colnames(df), obs_col %in% obs_col)

  predictions = df %>% pull(pred_col)
  observed = df %>% pull(obs_col)
  stopifnot(all(observed %in% c(0,1)))
  thresholds = sort(unique(predictions))

  stats = list()
  index = 1
  for(thres in thresholds) {
    stats[[index]] = c(thres, get_conf_mat_for_thres(predictions, observed, thres))
    index = index + 1
  }

  # get the (1,1) point in the ROC curve!
  stats[[index]] = c(thres + 0.001, get_conf_mat_for_thres(predictions, observed, thres + 0.001))

  roc_stats = as.data.frame(do.call(rbind, stats))
  colnames(roc_stats)[1] = 'threshold'

  x = roc_stats$FPR
  y = roc_stats$TPR
  AUC = sum(diff(x) * (head(y,-1)+tail(y,-1)))/2

  res_list = list()
  res_list$roc_stats = as_tibble(roc_stats)
  res_list$AUC = AUC
  return(res_list)
}

# get the confusion matrix values (TP, FN, TN, FP) + TPR, FPR by comparing the
# values from the `predictions` vector to the `thres` value, given the true class
# labelings from `observed`
get_conf_mat_for_thres = function(predictions, observed, thres) {
  tp = 0
  fn = 0
  tn = 0
  fp = 0

  index = 1
  for(prediction in predictions) {
    obs = observed[index]
    if (prediction < thres & obs == 1) {
      tp = tp + 1
    } else if (prediction < thres & obs == 0) {
      fp = fp + 1
    } else if (prediction >= thres & obs == 1) {
      fn = fn + 1
    } else if (prediction >= thres & obs == 0) {
      tn = tn + 1
    }
    index = index + 1
  }

  tpr = tp / (tp + fn)
  fpr = fp / (fp + tn)
  dist.from.chance = tpr - fpr
  dist.from.0.1 = (fpr - 0)^2 + (tpr - 1)^2

  res = c(tp, fn, tn, fp, fpr, tpr, dist.from.chance, dist.from.0.1)
  names(res) = c('TP', 'FN', 'TN', 'FP', 'FPR', 'TPR', 'dist_from_chance', 'dist_from_0_1')

  return(res)
}
