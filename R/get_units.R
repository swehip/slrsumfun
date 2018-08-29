#' Extract units from data
#'
#' Get units for specific period. Only used as a help function for all_units.
#' @param x Vector
#' @return All unique units sorted in alpabetic order.
#' @examples
#' get_units(dataOperations$P_Unit)
#' @export


get_units <- function(x){


  x <- sort(as.character(unique(attr_to_factor(x))))

  x <- x[!x %in% c("Ingen enhet - okänd",
                   "Registercentrum",
                   "Utländsk klinik",
                   NA)]

  return(x)


}
