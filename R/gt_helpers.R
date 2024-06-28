# Helpers


# Check for valid gt_object
#' check_gt_object
#'
#' @param gt_object An object of class `gt_tbl` to be checked.
#'
#' @return an error message
#' @export
#'
#' @examples
check_gt_object <- function(gt_object) {
  stopifnot(`'gt_object' must be a 'gt_tbl', have you accidentally passed raw data?` = "gt_tbl" %in% class(gt_object))
}


# Adjust table row padding
#' adjust_row_padding
#'
#' @param gt_object An existing gt table object of class `gt_tbl`
#' @param padding_setting Adjusts the density of the text in a table
#'
#' @return An object of class `gt_tbl`.
#' @export
#'
#' @examples
adjust_row_padding <- function(gt_object, padding_setting = "regular") {

  padding_setting <- match.arg(padding_setting, choices = c("condensed", "regular", "relaxed"))

  padding_value <- switch(padding_setting,
                          condensed = gt::px(3),  # Condensed padding
                          regular = gt::px(7),    # Regular padding
                          relaxed = gt::px(12))   # Relaxed padding

  gt_object %>%
    gt::tab_options(data_row.padding = padding_value)
}


# Function to add row striping
#' @title Row striping
#' @description
#' Add faint row striping to a table based on your color of choice.
#'
#' @param gt_object An existing gt table object of class `gt_tbl`
#' @param stripe_color Fill color for the stripes, defaults to `glitr::si_palettes$hw_slate_t[5]`
#'
#' @return An object of class `gt_tbl`.
#' @export
#'
#' @examples
apply_row_striping <- function(gt_object, stripe_color = "#E8E8E9") {
  gt_object %>%
    gt::tab_options(
      row.striping.include_table_body = TRUE,
      row.striping.background_color = stripe_color
    )
}


#' Helper function to quickly add title, subtitle and caption or source
#' @description
#' Quick way to add a table title, subtitle and source.
#'
#' @param gt_object An existing gt table object of class `gt_tbl`
#' @param title Text passed to the `tab_header()` function
#' @param subtitle Text passed to the `tab_header()` function
#' @param caption Text passed to the `tab_source_note()` function
#' @param ... Other gt options
#'
#' @return An object of class `gt_tbl`.
#' @export
#'
#' @examples
add_labs <- function(gt_object, title = NULL, subtitle = NULL, caption = NULL, ...) {

  check_gt_object(gt_object)

  gt_object %>%
    gt::tab_header(
      title = title,
      subtitle = subtitle
    ) %>%
    gt::tab_options(
      table_body.border.bottom.style = "solid",
    ) %>%
    gt::tab_source_note(
      source_note = caption
    )
}



