#' Save vector to a specified file
#'
#' Function for saving a \code{vector} with or without its row names to a
#' specified file.
#'
#' @param vector vector
#' @param file string. The name of the file, can be a full path.
#' @param with.row.names logical. If TRUE, then the \code{names(vector)} will be
#' included in the output file. Default value: FALSE.
#'
#' @importFrom utils write.table
#' @export
save_vector_to_file = function(vector, file, with.row.names = FALSE) {
  write.table(vector, file = file, quote = FALSE, col.names = FALSE,
              row.names = with.row.names, sep = "\t")
}

#' Save data frame to a specified file
#'
#' Function for saving a \code{data.frame} to a specified file.
#'
#' @param df data.frame
#' @param file string. The name of the file, can be a full path.
#'
#' @importFrom utils write.table
#' @export
save_df_to_file = function(df, file) {
  write.table(df, file = file, quote = FALSE, col.names = TRUE,
              row.names = TRUE, sep = "\t")
}

#' Save matrix to a specified file
#'
#' Function for saving a \code{matrix} to a specified file. Uses the
#' \code{\link{save_df_to_file}} function.
#'
#' @param mat matrix
#' @param file string. The name of the file, can be a full path.
#'
#' @export
save_mat_to_file = function(mat, file) {
  save_df_to_file(mat, file)
}
