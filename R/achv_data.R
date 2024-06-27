# R/achv_data.R

#' Example Dataset: Program Indicators with results, targets and achievement
#'
#' This dataset contains example program indicators for two operating units, Minoria and Hyrule. It includes information on prevention, testing, and treatment indicators, with results, targets, and achievements.
#'
#' @format A data frame with 12 rows and 6 variables:
#' \describe{
#'   \item{operating_unit}{Operating unit name, character}
#'   \item{program_area}{Program area name, character}
#'   \item{indicator}{Indicator name, character}
#'   \item{Result}{Result value, integer}
#'   \item{Target}{Target value, integer}
#'   \item{Achievement}{Achievement value, numeric}
#' }
#' @source Generated for demonstration purposes
#' @examples
#' data(achv_data)
#' head(achv_data)
"achv_data"
