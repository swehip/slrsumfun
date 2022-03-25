#' Convert object with attributes in "map" to factor
#'
#' Used to extract texts from a variable containing numeric values.
#' @param x Object with "map" attributes, where levels and labels exist
#' @return Factor
#' @examples
#' df <- structure(
#'   data.frame(cars = 1:3),
#'   map = data.frame(levels = 1:3, labels = c("Volvo", "Saab", "Opel"))
#' )
#' attr_to_factor(df$cars)
#' @export
attr_to_factor <- function(x) factor(x, attr(x, "map")$levels, attr(x, "map")$labels)
