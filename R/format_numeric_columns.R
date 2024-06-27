#' Formats numeric columns
#' @description
#' Provides pre-set formatting for whole numbers and decimals.
#' All whole numbers are formatted with a comma, all decimals with a percentage.
#'
#' @param gt_object An existing gt table object of class `gt_tbl`
#'
#' @return An object of class `gt_tbl`.
#' @export
#'
#' @examples
format_numeric_columns <- function(gt_object) {

  check_gt_object(gt_object)

  # Extract the data from the gt object
  data <- gt_object$`_data`

  numeric_cols <- data %>%
    dplyr::select(tidyselect::where(is.numeric)) %>%
    colnames()

  for (col in numeric_cols) {
    if (all(data[[col]] == floor(data[[col]]), na.rm = TRUE)) {
      # Whole number formatting with comma
      gt_object <- gt_object %>%
        gt::fmt_number(
          columns = col,
          decimals = 0,
          use_seps = TRUE
        )
    } else {
      # Decimal value formatting as percentage
      gt_object <- gt_object %>%
        gt::fmt_percent(
          columns = col,
          decimals = 0
        )
    }
  }

  return(gt_object)
}

