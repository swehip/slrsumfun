#' Operation profile units
#'
#' Get units with and without profile for specific period.
#' @param period Year/years of primary operation.
#' @return List containing units with and without profile.
#' @examples
#' # Get units with and without profile for 2018
#' profile_units()
#' # Get units with and without profile for 2016-2017
#' profile_units(2016:2017)
#' # Get units with and without profile for 1900-2020 (all existing years)
#' profile_units(1900:2020)
#' @export

profile_units <- function(period = lubridate::year(Sys.Date())){
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

  op_milj <- dplyr::filter(dataProfiles,
                           E_Year %in% period)

  primaries <- dplyr::filter(dataOperations,
                             lubridate::year(P_SurgDate) %in% period,
                             !P_Unit %in% op_milj$E_Unit)

  reops <- dplyr::filter(dataOperations,
                         lubridate::year(R_SurgDate) %in% period,
                         !R_Unit %in% op_milj$E_Unit)

  has_opmilj <- get_units(op_milj$E_Unit)
  no_prim_opmilj <- get_units(primaries$P_Unit)
  no_reop_opmilj <- get_units(reops$R_Unit)

  no_opmilj <- sort(unique(c(no_prim_opmilj, no_reop_opmilj)))

  return(list(Period = period_string,
              Has_profile = has_opmilj,
              Has_no_profile = no_opmilj))
}
