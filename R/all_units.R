#' All active units
#'
#' Get all active units for specific period.
#' @param period Year/years of primary operation and year/years of PROM date.
#' @return List containing all units, units who register primaries, units who register reoperations and units who register postoperative PROM.
#' @examples
#' # Get all active units for 2018
#' all_units()
#' # Get all active units for 2016-2017
#' all_units(2016:2017)
#' # Get all active units for 1900-2020 (all existing years)
#' all_units(1900:2020)
#' @export



all_units <- function(period = lubridate::year(Sys.Date())){
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

  primaries <- dplyr::filter(dataOperations,
                             lubridate::year(P_SurgDate) %in% period)

  reops <- dplyr::filter(dataOperations,
                         lubridate::year(R_SurgDate) %in% period)

  dataPROMAfter <- dplyr::filter(dataPROMAfter,
                                 lubridate::year(POSTP_Date) %in% period)

  enheter <- get_units(primaries$P_Unit)
  enheter_R <- get_units(reops$R_Unit)
  enheter_PROM <- get_units(dataPROMAfter$POSTP_Unit)

  total_units <- sort(unique(c(enheter, enheter_R, enheter_PROM)))

  return(list(Period = period_string,
              All_units_total = total_units,
              Primaries = enheter,
              Reoperations = enheter_R,
              Postop_PROM = enheter_PROM))
}
