#' Old xlsx saver
#'
#' Used to save xlsx files.
#' @param file Filename.
#' @param ... Data frames or matrices, where each data frame will be saved into one sheet.
#' @param row_names If TRUE, row names are included in output.
#' @param col_names If TRUE, column names are included in output.
#' @export


save.xlsx <- function (file, ..., row_names = FALSE, col_names = TRUE)
  {
      require(xlsx, quietly = TRUE)
      objects <- list(...)
      fargs <- as.list(match.call(expand.dots = TRUE))
      objnames <- as.character(fargs)[-c(1, 2)]
      nobjects <- length(objects)
      for (i in 1:nobjects) {
          if (i == 1)
              write.xlsx(objects[[i]], file, sheetName = objnames[i], row.names=row_names, col.names=col_names)
          else write.xlsx(objects[[i]], file, sheetName = objnames[i],
              append = TRUE, row.names=row_names, col.names=col_names)
      }
      print(paste("Workbook", file, "has", nobjects, "worksheets."))
}

