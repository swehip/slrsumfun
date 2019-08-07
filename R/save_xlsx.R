#' New xlsx saver
#'
#' Used to save xlsx files.
#' @param file Filename.
#' @param ... Data frames or matrices, where each data frame will be saved into
#'   one sheet.
#' @param row_names If TRUE, row names are included in output.
#' @param col_names If TRUE, column names are included in output.
#' @param bold If TRUE, column names are centered and bold.
#' @export
save_xlsx <- function(file, ..., row_names = FALSE, col_names = TRUE, bold = TRUE) {
  objects <- list(...)
  fargs <- as.list(match.call(expand.dots = TRUE))
  objnames <- as.character(fargs)[-c(1, 2)]
  names(objects) <- objnames
  nobjects <- length(objects)
  writexl::write_xlsx(objects, file, col_names = col_names, format_headers = bold)
  print(paste("Workbook", file, "has", nobjects, "worksheets."))
}
