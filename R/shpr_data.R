#' Load SHPR data layer
#'
#' Load all SHPR data, with attributes or factor format
#' @param factor_format If TRUE, variables with attributes are in factor format.
#' @examples
#' shpr_data()
#' shpr_data(TRUE)
#' @export

shpr_data <- function(factor_format = FALSE){
  if(factor_format){
    load("//rc-r/r$/Datalayers/SHPR/factordata.RData", envir = .GlobalEnv)
  }else{
    load("//rc-r/r$/Datalayers/SHPR/.RData", envir = .GlobalEnv)
  }
}
