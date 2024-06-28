#' Render Column as Image
#'
#' @description
#' Transforms specified columns in a gt table to display images instead of text. The images are specified by their URLs.
#'
#' @param gt_object A gt table object.
#' @param columns The columns to transform into images.
#' @param height The height of the images in pixels (default is 30).
#'
#' @return A gt table object with specified columns rendered as images.
#' @export
render_column_as_image <- function(gt_object, columns, height = 30){

  # Test that the object entered is in fact a gt object, if not it needs to be passed through gt()
  check_gt_object(gt_object)

  # Using web_image and purrr to map the urls to images in the selected columns
  gt_object %>%
      gt::text_transform(
        locations = gt::cells_body(columns = columns),
        fn = function(x) { # below maps the
          purrr::map(x, ~ gt::web_image(url = .x, height = height))
        }
      )
}


#' Generate Achievement URL
#'
#' @description
#' Creates a URL to one of four images stored with the SI gt adornment package.
#' Options are: `above_target`, `at_risk`, `concerned`, and `ontarget`.
#'
#' @param filename The name of the image file.
#' @param url_suffix The ending of the file. Can be .png or .svg
#'
#' @return A complete URL string to the image.
#' @export
#'
#' @examples
#' generate_achv_url("above_target")
#' generate_achv_url("at_risk")
generate_achv_url <- function(filename, url_suffix = ".png") {
  paste0("https://raw.githubusercontent.com/USAID-OHA-SI/gtayblr/main/inst/extdata/", filename, url_suffix)
}



#' Generate Achievement HTML
#'
#' @description
#' Generates the HTML for embedding an image corresponding to the achievement status.
#' Acceptable inputs are: `above_target`, `at_risk`, `concerned`, and `on_target` and `achv_legend`.
#'
#' @param filename The name of the image file. Acceptable inputs are `above_target`, `at_risk`, `concerned`, `on_target` and `achv_legend`.
#'
#' @return The URL string to the image.
#' @export
#'
#' @examples
#' generate_achv_html(above_target)
#' generate_achv_html("at_risk")
generate_achv_html <- function(filename) {

  # Define valid options
  valid_options <- c("above_target", "at_risk", "concerned", "on_target", "achv_legend")

  # Convert input to a string and check against valid options
  filename <- rlang::as_string(enexpr(filename))

  if (!filename %in% valid_options) {
    stop("Invalid input: Please enter one of 'above_target', 'at_risk', 'concerned', or 'on_target' or 'achv_legend'.")
  }

  img_url <- generate_achv_url(filename)

  glue::glue("{img_url}") %>% as.character()
}

