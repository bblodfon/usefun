#' Get the common names of two vectors
#'
#' This function prints and returns the common \code{names} of two vectors. The
#' two vectors don't have to be the same length.
#'
#' @param vec1 vector with \code{names} attribute
#' @param vec2 vector with \code{names} attribute
#' @param vector.names.str string. Used for printing, it tell us what are the
#' \code{names} of the two vectors (use plural form). Default value: "nodes".
#' @param with.gt logical. Determines if the ">" sign will be appended for nice
#' printing in an R notebook. Default value: TRUE.
#'
#' @return the character vector of the common names. If there is only one name
#' in common, the \code{vector.names.str} gets the last character stripped for
#' readability. If there is no common names, it returns FALSE.
#'
#' @seealso
#' \code{\link{pretty_print_vector_values}}, \code{\link{pretty_print_string}}
#'
#' @examples
#' vec1 = c(1,1,1)
#' vec2 = c(1,2)
#' names(vec1) = c("a","b","c")
#' names(vec2) = c("c","b")
#'
#' common.names = get_common_names(vec1, vec2)
#'
#' @export
get_common_names = function(vec1, vec2, vector.names.str = "nodes",
                            with.gt = TRUE) {
  common.names = intersect(names(vec1), names(vec2))

  if (is_empty(common.names)) {
    str = paste0("No common ", vector.names.str)
    pretty_print_string(str, with.gt = with.gt)
    return(FALSE)
  }
  else {
    pretty_print_vector_values(common.names, vector.values.str = vector.names.str,
                               with.gt = with.gt)
    return(common.names)
  }
}

#' Get the common values of two vectors
#'
#' This function prints and returns the common values of two vectors. The two
#' vectors don't have to be the same length.
#'
#' @param vec1 vector
#' @param vec2 vector
#' @param vector.values.str string. Used for printing, it tell us what are the
#' values of the two vectors (use plural form). Default value: "nodes".
#' @param with.gt logical. Determines if the ">" sign will be appended for nice
#' printing in an R notebook. Default value: TRUE.
#'
#' @return the vector of the common values. If there is only one value
#' in common, the \code{vector.values.str} gets the last character stripped for
#' readability. If there are no common values, it returns NULL.
#'
#' @seealso
#' \code{\link{pretty_print_vector_values}}, \code{\link{pretty_print_string}}
#'
#' @examples
#' vec1 = c(1,2,3)
#' vec2 = c(3,4,1)
#'
#' common.names = get_common_values(vec1, vec2)
#'
#' @export
get_common_values = function(vec1, vec2, vector.values.str = "nodes",
                             with.gt = TRUE) {
  common.values = intersect(vec1, vec2)

  if (is_empty(common.values)) {
    str = paste0("No common ", vector.values.str)
    pretty_print_string(str, with.gt = with.gt)
    return(NULL)
  }
  else {
    pretty_print_vector_values(common.values, with.gt = with.gt)
    return(common.values)
  }
}

#' Get average over unique values
#'
#' Use this function on two vectors with same \code{names} attribute (column
#' names), to find for each unique (numeric) value of the first vector, the
#' average and standard deviation values of the second vector's values (matching
#' is done by column name)
#'
#' @param vec1 vector with \code{names} attribute
#' @param vec2 vector with \code{names} attribute
#'
#' @return A \code{matrix} consisting of 3 column vectors. The matrix size is
#' \code{dim(matrix)= 3xn}, where n is the number of unique values of \code{vec1}).
#' The columns vectors are:
#'   \enumerate{
#'     \item the first input vector pruned to its unique values
#'     \item a vector with the average values for each unique value of the
#'  first vector (the matching is done by column name)
#'     \item a vector with the standard deviation values for each unique value
#'     of the first vector (the matching is done by column name)
#'   }
#'
#' @examples
#' vec1 = c(1, 2, 3, 2)
#' vec2 = c(20, 2, 2.5, 8)
#' names.vec = c(seq(1,4))
#' names(vec1) = names.vec
#' names(vec2) = names.vec
#'
#' res = get_average_over_unique_values(vec1, vec2)
#'
#' @importFrom stats sd
#' @export
get_average_over_unique_values = function(vec1, vec2) {
  stopifnot(names(vec1) == names(vec2))

  vec1.sorted = sort(vec1)
  vec1.sorted.unique = sort(unique(vec1))
  vec2.avg.values = numeric(length = length(vec1.sorted.unique))
  sd.values = numeric(length = length(vec1.sorted.unique))

  index = 0
  for (value in vec1.sorted.unique) {
    index = index + 1
    vec2.avg.values[index] = mean(vec2[
      (names(vec1.sorted[vec1.sorted == value]))
    ])
    sd.values[index] = sd(vec2[
      (names(vec1.sorted[vec1.sorted == value]))
    ])
  }

  # In case of NA elements in sd calculation
  # (one element vectors), replace with 0
  sd.values[is.na(sd.values)] = 0

  res = cbind(vec1.sorted.unique, vec2.avg.values, sd.values)
  colnames(res) = c("vec1.unique", "vec2.mean", "vec2.sd")

  return(res)
}

#' Get percentage of matches between two vectors
#'
#' Use this function on two numeric vectors with the same \code{names} attribute
#' (columns) and same length, in order to find the percentage of common elements
#' (value matches between the two vectors). The same \code{names} for the two
#' vectors ensures that their values are logically matched one-to-one.
#'
#' @param vec1 numeric vector with \code{names} attribute
#' @param vec2 numeric vector with \code{names} attribute
#'
#' @return the percentage of common values (exact matches) between the two
#' vectors. Can only be a value between 0 (no common elements) and 1 (perfect
#' element match).
#'
#' @examples
#' vec1 = c(1, 2, 3, 2)
#' vec2 = c(20, 2, 2.5, 8)
#' vec3 = c(1, 2, 333, 222)
#' names.vec = c(seq(1,4))
#' names(vec1) = names.vec
#' names(vec2) = names.vec
#' names(vec3) = names.vec
#'
#' match.1.2 = get_percentage_of_matches(vec1, vec2)
#' match.1.3 = get_percentage_of_matches(vec1, vec3)
#'
#' @export
get_percentage_of_matches = function(vec1, vec2) {
  stopifnot(is.numeric(vec1) && is.numeric(vec2))
  stopifnot(length(vec1) == length(vec2))
  stopifnot(names(vec1) == names(vec2))

  total = length(vec1)
  diff = vec1 - vec2
  matches = sum(diff == 0)
  matches.percentage = matches / total

  return(matches.percentage)
}

#' Prune single-value columns from a data frame
#'
#' Given a \code{data.frame} and an integer value, it checks whether there is a
#' column vector whose values match the given one. If so, it prunes that
#' single-valued column from the \code{data.frame}
#'
#' @param df \code{data.frame}
#' @param value an integer value
#'
#' @return the column-pruned \code{data.frame}
#'
#' @examples
#' df = data.frame(c(0,0,0), c(0,1,0), c(1,0,0))
#' prune_columns_from_df(df, value = 0)
#'
#' @export
prune_columns_from_df = function(df, value) {
  if (length(df) == 0) return(df)
  return(df[, colSums(df != value) > 0])
}

#' Prune single-value rows from a data frame
#'
#' Given a \code{data.frame} and an integer value, it checks whether there is a
#' row vector whose values match the given one. If so, it prunes that
#' single-valued row from the \code{data.frame}
#'
#' @param df \code{data.frame}
#' @param value an integer value
#'
#' @return the row-pruned \code{data.frame}
#'
#' @examples
#' df = data.frame(c(0,0,0), c(0,1,0), c(1,0,0))
#' prune_rows_from_df(df, value = 0)
#'
#' @export
prune_rows_from_df = function(df, value) {
  if (length(df) == 0) return(df)
  return(df[rowSums(df != value) > 0, ])
}

#' Add vector to a (n x 2) data frame
#'
#' Given a vector, adds each value and its corresponding name to a data frame
#' of 2 columns as new rows, where the name fills in the 1st column and the
#' value the 2nd column.
#'
#' @param df \code{data.frame}, with n rows and 2 columns
#' @param vec a vector
#'
#' @return a \code{data.frame} with additional rows and each element as a
#' character.
#'
#' @examples
#' df = data.frame(c(0,0,1), c(0,0,2))
#' vec = 1:3
#' names(vec) = c("a","b","c")
#'
#' add_vector_to_df(df, vec)
#'
#' @export
add_vector_to_df = function(df, vec) {
  stopifnot(ncol(df) == 2)

  if (length(vec) == 0) return(df)
  for (i in 1:length(vec)) {
    value = vec[i]
    name = names(vec)[i]
    df = rbind(df, c(name, value))
  }
  return(df)
}

#' Prune and reorder vector elements
#'
#' Given two vectors, the first one's elements are pruned and reordered according
#' to the common values of the second vector and the elements' \emph{\code{names}
#' (attribute) of the first}. If there no common such values, an empty vector is
#' returned.
#'
#' @param vec a vector with \code{names} attribute
#' @param filter.vec a character vector whose values will be used to filter the
#' \code{vec} elements
#'
#' @return the pruned and re-arranged vector.
#'
#' @examples
#' vec = c(1,2,3)
#' names(vec) = c("a","b","c")
#'
#' filter.vec1 = c("a")
#' prune_and_reorder_vector(vec, filter.vec1)
#'
#' filter.vec2 = c("c", "ert", "b")
#' prune_and_reorder_vector(vec, filter.vec2)
#'
#' @export
prune_and_reorder_vector = function(vec, filter.vec) {
  pruned.vec = vec[names(vec) %in% filter.vec]
  reordered.vec = pruned.vec[order(match(names(pruned.vec), filter.vec))]

  return(reordered.vec)
}

#' Get ternary class id
#'
#' Helper function that checks if a \emph{value} surpasses the given
#' \emph{threshold} either positively, negatively or not at all and returns
#' a value indicating in which class (i.e. interval) it belongs.
#'
#' @param value numeric
#' @param threshold numeric
#'
#' @return an integer. There are 3 cases:
#'   \itemize{
#'     \item 1: when \eqn{value > threshold}
#'     \item -1: when \eqn{value < -threshold}
#'     \item 0: otherwise
#'   }
#'
#' @export
get_ternary_class_id = function(value, threshold) {
  if (value > threshold) return(1) # active
  if (value < -threshold) return(-1) # inhibited
  return(0) # no biomarker
}

#' Add a row to a 3-valued (ternary) \code{data.frame}
#'
#' Use this function on a \code{data.frame} object (with values only
#' in the 3-element set \{-1,0,1\} ideally - specifying either a positive,
#' negative or none/absent condition/state/result about something) and add an
#' extra \strong{first or last row vector} with zero values, where \emph{1}
#' and \emph{-1} will be filled when the column names of the given
#' \code{data.frame} match the values in the \emph{values.pos} or
#' \emph{values.neg} vector parameters respectively.
#'
#' @param df a \code{data.frame} object with values only in the
#' the 3-element set \{-1,0,1\}. The column names should be node names
#' (gene, protein names, etc.).
#' @param values.pos a character vector whose elements are indicators of a
#' positive state/condition and will be assigned a value of \emph{1}.
#' These elements \strong{must be a subset of the column names} of the given \code{df} parameter.
#' If empty, no values equal to \emph{1} will be added to the new row.
#' @param values.neg a character vector whose elements are indicators of a
#' negative state/condition and will be assigned a value of \emph{-1}.
#' If empty, no values equal to \emph{-1} will be added to the new row.
#' These elements \strong{must be a subset of the column names} of the given \code{df} parameter.
#' @param pos string. The position where we should put the new row that will be generated.
#' Two possible values: "first" (default) or "last".
#' @param row.name string. The name of the new row that we will added. Default
#' value: NULL.
#'
#' @return the \code{df} with one extra row, having elements from the \{-1,0,1\}
#' set depending on values of \code{values.pos} and \code{values.neg} vectors.
#'
#' @examples
#' df = data.frame(c(0,-1,0), c(0,1,-1), c(1,0,0))
#' colnames(df) = c("A","B","C")
#' df.new = add_row_to_ternary_df(df, values.pos = c("A"), values.neg = c("C"), row.name = "Hello!")
#'
#' @export
add_row_to_ternary_df =
  function(df, values.pos, values.neg, pos = "first", row.name = NULL) {
    # some checks
    stopifnot(pos %in% c("first", "last"))
    col.names = colnames(df)
    stopifnot(values.pos %in% col.names, values.neg %in% col.names)

    # initialize a 'row' data.frame
    row = as.data.frame(matrix(0, ncol = length(col.names), nrow = 1))
    colnames(row) = col.names
    rownames(row) = row.name

    # add 'positive' and 'negative' meta-values
    row[colnames(row) %in% values.pos] = 1
    row[colnames(row) %in% values.neg] = -1

    if (pos == 'first')
      res = rbind(row, df)
    else
      res = rbind(df, row)

    return(res)
  }

#' Rearrange a list of data frames by rownames
#'
#' @param list_df a (non-empty) list of \code{data.frame} objects. The data
#' frames must have the same \code{colnames} attribute.
#'
#' @return a rearranged list of data frames, where the names of the elements of
#' the \code{list_df} (the 'ids' of the data frames) and the \code{rownames} of
#' the data frames have switched places: the unique row names of the original list's
#' combined data frames serve as \code{names} for the returned list of data
#' frames, while the data frame 'ids' (\code{names} of the original list's
#' elements) now serve as \code{rownames} for the data frames in the new list.
#'
#' E.g. if in the given \code{list} there was a \code{data.frame} with id 'A':
#' \code{a = list_df[["A"]]} and \code{rownames(a) = c("row1", "row2")}, then
#' in the rearranged \code{list} there would be two data frames with ids
#' "row1" and "row2", each of them having a row with name "A" where also these
#' data rows would be the same as before: \code{list_df[["A"]]["row1", ] == returned_list[["row1"]]["A",]}
#' and \code{list_df[["A"]]["row2", ] == returned_list[["row2"]]["A",]} respectively.
#'
#' @examples
#' df.1 = data.frame(matrix(data = 0, nrow = 3, ncol = 3,
#'   dimnames = list(c("row1", "row2", "row3"), c("C.1", "C.2", "C.3"))))
#' df.2 = data.frame(matrix(data = 1, nrow = 3, ncol = 3,
#'   dimnames = list(c("row1", "row2", "row4"), c("C.1", "C.2", "C.3"))))
#' list_df = list(df.1, df.2)
#' names(list_df) = c("zeros", "ones")
#' res_list_df = ldf_arrange_by_rownames(list_df)
#'
#' @export
ldf_arrange_by_rownames = function(list_df) {
  # some checks
  stopifnot(is.list(list_df), length(list_df) > 0)
  stopifnot(all(sapply(list_df, function(df) { is.data.frame(df) })))
  column_names_mat = sapply(list_df, function(df) { colnames(df) })
  stopifnot(all(duplicated(column_names_mat, MARGIN = 2)[-1])) # same column names

  column_names = colnames(list_df[[1]])
  unique_row_names = unique(unlist(sapply(list_df, function(df) { rownames(df) })))

  # initialize `res` list
  res = list()
  for (row_name in unique_row_names) {
    df = as.data.frame(matrix(data = NA, nrow = 0, ncol = length(column_names)))
    colnames(df) = column_names
    res[[row_name]] = df
  }

  # fill in `res` list
  for (df_name in names(list_df)) {
    df = list_df[[df_name]]
    for (row_name in rownames(df)) {
      res[[row_name]][df_name,] = df[row_name, ]
    }
  }

  return(res)
}
