# Function to apply column alignments
#' @description
#' Aligns columns based on their type. Numeric columns are right aligned.
#' Text columns are left aligned.
#'
#'
#' @param gt_object
#'
#' @return
#' @export
#'
#' @examples
align_columns <- function(gt_object) {

  check_gt_object(gt_object)

  gt_object %>%
    gt::cols_align(
      align = "left",
      columns = tidyselect::where(is.character)
    ) %>%
    gt::cols_align(
      align = "right",
      columns = tidyselect::where(is.numeric)
    )
}
