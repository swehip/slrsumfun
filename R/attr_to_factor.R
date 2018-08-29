#' Convert object with attributes in "map" to factor
#'
#' Used to extract texts from a variable containing numeric values.
#' @param x Object with "map" attributes, where levels and labels exist
#' @return Factor
#' @examples
#' df <- data.frame(cars = 1:3)
#' attr(df$cars, "map") <- data.frame(levels = 1:3, labels = c("Volvo", "Saab", "Opel"))
#' attr_to_factor(df$cars)
#' @export


attr_to_factor <- function(x){

  x <- factor(x,
              levels = attr(x, "map")$levels,
              labels = attr(x, "map")$labels)
  return(x)
}
