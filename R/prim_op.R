#' Primary operations
#'
#' Get number of primary operations for each unit and specific period.
#' @param period Year/years of primary operation.
#' @return Data frame with units, period, number of operations, number of THR and number of hemi.
#' @examples
#' # Get number of operations for current year
#' prim_op()
#' # Get number of operations for 2016-2017
#' prim_op(2016:2017)
#' # Get number of operations for 1900-2020 (all existing years)
#' prim_op(1900:2020)
#' @export

prim_op <- function(period = lubridate::year(Sys.Date())){
  require(dplyr)
  if(!"dataOperations" %in% ls(envir = .GlobalEnv)){
    load("//rc-r/r$/Datalayers/SHPR/.RData", envir = .GlobalEnv)
    # rm(list = c("dataComponents", "dataOperations_headers", "dataOperations_vlab",
    #             "dataProfiles_factors", "dataProfiles_headers", "dataProfiles_vlab",
    #             "dataPROMAfter_factors", "dataPROMAfter_headers", "dataPROMAfter_vlab",
    #             "dataPROMBefore_factors", "dataPROMBefore_headers", "dataPROMBefore_vlab",
    #             "Descriptors", "Excerpt", "Get", "Get.ValueLabels", "Get.VariableHeaders",
    #             "KV", "Map", "Merge", "R.MakeFactors"), envir = .GlobalEnv)
  }

  period_string <- ifelse(length(period) == 1, period, paste(period[1], period[length(period)], sep = "-"))

  dataOperations$P_ProstType[is.na(dataOperations$P_ProstType)] <- 1

  hosp_dat <- dplyr::select(dataOperations, SubjectKey, P_SurgDate, P_Side, P_Unit,P_ProstType) %>%
    dplyr::mutate(P_SurgDate = as.Date(P_SurgDate, tz = "CET"), Operation_year = lubridate::year(P_SurgDate)) %>%
    dplyr::filter(Operation_year %in% period) %>%
    dplyr::distinct(SubjectKey, P_Side, .keep_all = TRUE) %>%
    dplyr::mutate(Unit = attr_to_factor(P_Unit))

  hosp_dat %>%
    dplyr::group_by(Unit) %>%
    dplyr::summarise(Operation_year = period_string,
                     Count = n(),
                     THR =  sum(P_ProstType == 1, na.rm = TRUE),
                     Hemi =  sum(P_ProstType == 2, na.rm = TRUE)) %>%
    dplyr::ungroup() %>%
    dplyr::arrange(as.character(Unit)) %>%
    as.data.frame() -> prim_ops

  return(prim_ops)
}
