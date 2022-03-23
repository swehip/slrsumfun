
#' Add attributes from one data frame to another
#'
#' Add "map" attributes from df2 to df1.
#' @param df1 Data frame to receive attributes.
#' @param df2 Data frame to give attributes.
#' @return Data frame.
#' @examples
#' df1 <- data.frame(cars = 1:3)
#' df2 <- data.frame(cars = 1:3)
#' attr(df2$cars, "map") <- data.frame(levels = 1:3,
#' labels = c("Volvo", "Saab", "Opel"))
#' df1 <- add_attr(df1, df2)
#' df1$cars
#' @export
add_attr <- function(df1, df2) {
  for (i in names(df1)) {
    attr(df1[[i]], "map") <- attr(df2[[i]], "map")
  }
  df1
}
#' Filter variable for existing data
#'
#' Filter variable where there are existing data in other variable.
#' @param x Variable to be filtered.
#' @param y Condition variable.
#' @return Filtered variable.
#' @examples
#' df <- data.frame(
#'   cars = 1:3,
#'   cars_model = 4:6,
#'   cars_speed = 7:9,
#'   condition = c(NA, 1, 2)
#' )
#' dplyr::mutate_at(
#'   df,
#'   dplyr::vars(dplyr::starts_with("cars")),
#'   filter_cens, y = df$condition
#' )
#' @export
filter_cens <- function(x, y){
  dplyr::case_when(!is.na(y) ~ x)
}

#' Check if object has attribute "map"
#'
#' Useful in mutate_if to be used with mutate_label
#' @param x Vector
#' @return TRUE or FALSE
#' @examples
#' df <- data.frame(cars = 1:3)
#' attr(df$cars, "map") <-
#' data.frame(levels = 1:3, labels = c("Volvo", "Saab", "Opel"))
#' check_attr(df$cars)
#' @export
check_attr <- function(x){
  !is.null(attr(x, "map"))
}

#' Extract attribute "map" and mutate object to be labelled
#'
#' Labelled are used to get right labels when saving to spss using package haven
#' @param x Vector
#' @return Labelled
#' @examples
#' df <- data.frame(cars = 1:3)
#' attr(df$cars, "map") <- data.frame(levels = 1:3,
#' labels = c("Volvo", "Saab", "Opel"))
#' df <- dplyr::mutate(df, dplyr::across(where(check_attr), mutate_label))
#' @export
mutate_label <- function(x) {
  if (is.numeric(x)) {
    labels <- stats::setNames(as.numeric(as.character(attr(x, "map")$levels)),
                              as.character(attr(x, "map")$labels))

  } else if (is.factor(x)) {
    labels <- stats::setNames(as.character(attr(x, "map")$levels),
                              as.character(attr(x, "map")$labels))
    x <- as.character(x)
  } else{
    labels <- stats::setNames(attr(x, "map")$levels, attr(x, "map")$labels)
  }

  haven::labelled(x, labels)
}
