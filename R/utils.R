#' Check that a data.frame has required columns
#'
#' @param df data.frame
#' @param vec_cols vector of column names
#'
#' @return nothing, pops an error if any of the columns
#'         are not present in df
#'
#' @export
require_columns <- function(df,
                            vec_cols) {
  if (!all(vec_cols %in% colnames(df))) {
    vec_missing_cols <- setdiff(vec_cols,
                                colnames(df))
    stop("Missing required columns: ",
         paste0(vec_missing_cols, collapse = ", "))
  }
}
