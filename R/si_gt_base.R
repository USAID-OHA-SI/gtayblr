# Creates a base SI theme from which variations can be created
#' SI base theme
#'
#' @param gt_object An existing gt table object of class `gt_tbl`
#' @param header_fill Fill color for the header background, defaults to `glitr::hw_slate`
#' @param header_font_color Font color for the header text, defaults to white
#' @param table_font Font type as called from `google_font()`
#' @param table_font_color Font color for the table body, defaults to `glitr::hw_slate`
#' @param ... Optional additional arguments to `gt::table_options()`
#'
#' @return An object of class `gt_tbl`.
#' @export
#'
#' @examples
si_gt_base <- function(gt_object,
                       header_fill = "#626672",
                       header_font_color = "white",
                       table_font = "Source Sans Pro",
                       table_font_color = "#626672",
                       ...) {

  # Test that the object entered is in fact a gt object, if not it needs to be passed through gt()
  check_gt_object(gt_object)

  # Base NYTimes theme settings with some tweaks
  gt_object %>%
    gt::tab_options(
      heading.align = "left",
      column_labels.border.top.style = "none",
      table.border.top.style = "none",
      column_labels.border.bottom.style = "none",
      column_labels.border.bottom.width = 1,
      column_labels.border.bottom.color = "#334422",
      table_body.border.top.style = "none",
      table_body.border.bottom.color = "#d1d3d4",
      table_body.hlines.width = 0,
      heading.border.bottom.style = "none", # No lines
      data_row.padding = gt::px(5), # Controls squishiness of rows
      column_labels.font.size = gt::px(14),
      row_group.padding = gt::px(4),
      source_notes.font.size = gt::px(10),
      ...
    ) %>%
    gt::tab_style(
      style = gt::cell_text(
        color = header_font_color, font = gt::google_font(table_font),
        weight = 700, transform = "uppercase"
      ),
      locations = gt::cells_column_labels(tidyselect::everything())
    ) %>%
    # Create a header that is filled in our slate color
    gt::tab_style(
      style = gt::cell_fill(color = header_fill),
      locations = gt::cells_column_labels(tidyselect::everything())
    ) %>% # Table header --RN font is set as Libre Franklin
    gt::tab_style(
      style = gt::cell_text(
        color = table_font_color, font = gt::google_font("Libre Franklin"), weight = 750,
        transform = "uppercase"
      ),
      locations = gt::cells_title(groups = "title")
    ) %>% # Setting cell text to be a monospaced font
    gt::tab_style(
      style = gt::cell_text(color = table_font_color, font = gt::google_font(table_font), weight = 400),
      locations = gt::cells_body()
    ) %>% # When groupname_col is specified, format headers
    gt::tab_style(
      style = gt::cell_text(
        color = table_font_color, font = gt::google_font(table_font), weight = 600,
        transform = "uppercase"
      ),
      locations = gt::cells_row_groups()
    ) %>% # When rowname_group is specified, format text
    gt::tab_style(
      style = gt::cell_text(
        color = table_font_color, font = gt::google_font(table_font), weight = 500,
        transform = "uppercase"
      ),
      locations = gt::cells_stub()
    ) %>% # Source Notes formatting
    gt::tab_style(
      style = gt::cell_text(color = table_font_color, font = gt::google_font(table_font), weight = 400),
      locations = gt::cells_source_notes()
    )
}
