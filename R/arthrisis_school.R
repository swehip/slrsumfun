#' Arthrisis School attendance
#'
#' Number of arthrisis patients that attended arthrisis school.
#' @param period Year/years of primary operation.
#' @return Data frame with units, period, number of THR (only arthrisis diagnosis, M16.0-M16.9),
#' number of arthrisis question answered and number of participants.
#' @examples
#' # Get number of arthrisis patients that attended arthrisis school for 2018
#' arthrisis_school()
#' # Get number of arthrisis patients that attended arthrisis school for 2016-2017
#' arthrisis_school(2016:2017)
#' # Get number of arthrisis patients that attended arthrisis school for 1900-2020
#' # (all existing years)
#' arthrisis_school(1900:2020)
#' @export

arthrisis_school <- function(period = lubridate::year(Sys.Date())){
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

  dataPROMBefore <- dplyr::select(dataPROMBefore, -DateCorrectness, -DateOfDeath, -SubjectID)

  hosp_dat <- dplyr::select(dataOperations, SubjectKey, P_SurgDate, P_Side, P_Unit, P_ProstType, P_Diagnosis) %>%
    dplyr::mutate(P_SurgDate = as.Date(P_SurgDate, tz = "CET"), Operation_year = lubridate::year(P_SurgDate)) %>%
    dplyr::filter(Operation_year %in% period,
                  P_ProstType == 1,
                  grepl("^M16", P_Diagnosis)) %>%
    dplyr::arrange(SubjectKey, P_Side, P_SurgDate) %>%
    dplyr::distinct(SubjectKey, P_Side, .keep_all = TRUE) %>%
    dplyr::mutate(Unit = attr_to_factor(P_Unit))

  prims <- dplyr::distinct(hosp_dat, SubjectKey, P_SurgDate)

  # Preop

  suppressWarnings(dataPROMBefore2 <- left_join(dataPROMBefore, prims, by = "SubjectKey"))

  dataPROMBefore2 <- dplyr::mutate(dataPROMBefore2,
                                   reg_time = as.numeric(difftime(P_SurgDate, PREP_Date, units = "days"))) %>%
    dplyr::filter(between(reg_time, 0, 180)) %>%
    dplyr::arrange(SubjectKey, P_SurgDate, reg_time) %>%
    dplyr::distinct(SubjectKey, P_SurgDate, .keep_all = TRUE)

  suppressWarnings(hosp_dat <- left_join(hosp_dat, dataPROMBefore2, by = c("SubjectKey", "P_SurgDate")))

  hosp_dat %>%
    dplyr::group_by(Unit) %>%
    dplyr::summarise(Operation_year = period_string,
                     OA_THR = n(),
                     ArthrisisSchool_Answered = sum(!is.na(PREP_ArthrisisSchool)),
                     Went_ArthrisisSchool = sum(PREP_ArthrisisSchool == 1, na.rm = TRUE)) %>%
    dplyr::ungroup() %>%
    dplyr::arrange(as.character(Unit)) %>%
    as.data.frame() -> prom_dat

  return(prom_dat)
}
