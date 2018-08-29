#' Reoperations
#'
#' Get number of reoperations for each unit and specific period.
#' @param period Year/years of reoperation.
#' @return Data frame with units, period, number of reoperations, number of THR and number of hemi.
#' @examples
#' # Get number of reoperations for current year
#' re_op()
#' # Get number of reoperations for 2016-2017
#' re_op(2016:2017)
#' # Get number of reoperations for 1900-2020 (all existing years)
#' re_op(1900:2020)
#' @export

re_op <- function(period = lubridate::year(Sys.Date())){
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

  hosp_dat <- dplyr::select(dataOperations, SubjectKey, R_SurgDate, P_Side, R_Unit, P_ProstType) %>%
    dplyr::mutate(R_SurgDate = as.Date(R_SurgDate, tz = "CET"), Operation_year = lubridate::year(R_SurgDate)) %>%
    dplyr::filter(Operation_year %in% period) %>%
    dplyr::mutate(Unit = attr_to_factor(R_Unit))

  hosp_dat %>%
    dplyr::group_by(Unit) %>%
    dplyr::summarise(Operation_year = period_string,
                     Count = n(),
                     THR =  sum(P_ProstType == 1, na.rm = TRUE),
                     Hemi =  sum(P_ProstType == 2, na.rm = TRUE)) %>%
    dplyr::ungroup() %>%
    dplyr::arrange(as.character(Unit)) %>%
    as.data.frame() -> re_ops

  return(re_ops)
}
