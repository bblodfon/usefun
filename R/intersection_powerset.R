#' @title Powerset Intersection Table
#'
#' @description
#' This function computes the intersection of elements for all possible
#' combinations of the provided sets of IDs.
#' A typical use case is in a cohort of patients with incomplete data across
#' multiple data types. This function helps determine how many patients
#' have complete data for specific combinations of data types, allowing
#' you to find the optimal combinations for analysis.
#'
#' @param ids `list()`\cr
#' A named list, each element being a numeric vector of ids.
#'
#' @return A tibble with columns:
#' - `set_combo`: name for combo set/vector
#' - `num_subsets`: number of subsets in the combo set
#' - `common_ids`: vector of common ids in the combo set
#' - `count`: number of common ids
#'
#' @examples
#' library(dplyr)
#' ids = list(a = 1:3, b = 2:5, c = 1:4, d = 3:6, e = 2:6)
#' res = powerset_icounts(ids)
#'
#' res |>
#'   filter(num_subsets >= 2, count > 2) |>
#'   arrange(desc(count), desc(num_subsets))
#'
#' @export
powerset_icounts = function(ids) {
  checkmate::assert_list(ids, names = "unique", min.len = 1, null.ok = FALSE, types = "numeric")
  set_names = names(ids)

  powerset =
    lapply(1:length(set_names), combinat::combn, x = set_names, simplify = FALSE) |>
    unlist(recursive = FALSE)

  # intersect ids
  iids = lapply(powerset, function(set) Reduce(intersect, ids[set]))

  tibble(
    set_combo = mlr3misc::map_chr(powerset, paste0, collapse = "-"),
    num_subsets = mlr3misc::map_int(powerset, length),
    common_ids = iids,
    count = mlr3misc::map_int(iids, length)
  )
}
