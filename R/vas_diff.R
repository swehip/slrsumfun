#' Difference in VAS health
#'
#' Mean of the difference in VAS health after operation and before operation.
#' @param period Year/years of primary operation.
#' @return Data frame with units, period, number of operations, number of THR, number of hemi,
#' existing VAS data (both before and 1/6/10 years after operation),
#' mean difference in VAS health (between 1/6/10 years after operation and before operation)
#' @examples
#' # Get difference in VAS health for 2018 (does not exist since not enough follow up time)
#' vas_diff()
#' # Get difference in VAS health for 2016-2017 (only 1 year follow up exist)
#' vas_diff(2016:2017)
#' # Get difference in VAS health for 1900-2020 (all existing years)
#' vas_diff(1900:2020)
#' @export


vas_diff <- function(period = lubridate::year(Sys.Date())){
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

  dataPROMAfter <- dplyr::select(dataPROMAfter, -DateCorrectness, -DateOfDeath, -SubjectID)

  dataPROMBefore <- dplyr::select(dataPROMBefore, -DateCorrectness, -DateOfDeath, -SubjectID)

  hosp_dat <- dplyr::select(dataOperations, SubjectKey, P_SurgDate, P_Side, P_Unit, P_ProstType) %>%
    dplyr::mutate(P_SurgDate = as.Date(P_SurgDate, tz = "CET"), Operation_year = lubridate::year(P_SurgDate)) %>%
    dplyr::filter(Operation_year %in% period) %>%
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

  # PROM 1 year

  suppressWarnings(dataPROMAfter_2 <- left_join(dataPROMAfter, prims, by = "SubjectKey"))

  dataPROMAfter_1yrs <- dplyr::mutate(dataPROMAfter_2,
                                      reg_time = as.numeric(difftime(POSTP_Date, P_SurgDate, units = "days"))) %>%
    dplyr::filter(between(reg_time, 365 - 90, 365 + 180)) %>%
    dplyr::mutate(reg_time = abs(365-reg_time)) %>%
    dplyr::arrange(SubjectKey, P_SurgDate, reg_time) %>%
    dplyr::distinct(SubjectKey, P_SurgDate, .keep_all = TRUE)

  names(dataPROMAfter_1yrs)  <- paste(names(dataPROMAfter_1yrs), '1yr', sep ='_')


  suppressWarnings(hosp_dat <- left_join(hosp_dat, dataPROMAfter_1yrs, by = c("SubjectKey" = "SubjectKey_1yr",
                                                                              "P_SurgDate" = "P_SurgDate_1yr")))

  # PROM 6 years

  dataPROMAfter_6yrs <- dplyr::mutate(dataPROMAfter_2,
                                      reg_time = as.numeric(difftime(POSTP_Date, P_SurgDate, units = "days"))) %>%
    dplyr::filter(between(reg_time, 6*365 - 365, 6*365 + 365)) %>%
    dplyr::mutate(reg_time = abs(6*365-reg_time)) %>%
    dplyr::arrange(SubjectKey, P_SurgDate, reg_time) %>%
    dplyr::distinct(SubjectKey, P_SurgDate, .keep_all = TRUE)

  names(dataPROMAfter_6yrs)  <- paste(names(dataPROMAfter_6yrs), '6yrs', sep ='_')

  suppressWarnings(hosp_dat <- left_join(hosp_dat, dataPROMAfter_6yrs, by = c("SubjectKey" = "SubjectKey_6yrs",
                                                                              "P_SurgDate" = "P_SurgDate_6yrs")))


  # PROM 10 years

  dataPROMAfter_10yrs <- dplyr::mutate(dataPROMAfter_2,
                                       reg_time = as.numeric(difftime(POSTP_Date, P_SurgDate, units = "days"))) %>%
    dplyr::filter(between(reg_time, 10*365 - 365, 10*365 + 365)) %>%
    dplyr::mutate(reg_time = abs(10*365-reg_time)) %>%
    dplyr::arrange(SubjectKey, P_SurgDate, reg_time) %>%
    dplyr::distinct(SubjectKey, P_SurgDate, .keep_all = TRUE)

  names(dataPROMAfter_10yrs)  <- paste(names(dataPROMAfter_10yrs), '10yrs', sep ='_')

  suppressWarnings(hosp_dat <- left_join(hosp_dat, dataPROMAfter_10yrs, by = c("SubjectKey" = "SubjectKey_10yrs",
                                                                               "P_SurgDate" = "P_SurgDate_10yrs")))

  hosp_dat %>%
    dplyr::group_by(Unit) %>%
    dplyr::summarise(Operation_year = period_string,
                     Count = n(),
                     THR =  sum(P_ProstType == 1, na.rm = TRUE),
                     Hemi =  sum(P_ProstType == 2, na.rm = TRUE),
                     Existing_VAS_1yr = sum(!is.na(PREP_VASHealth) & !is.na(POSTP_VASHealth_1yr)),
                     VAS_diff_1yr = mean(POSTP_VASHealth_1yr - PREP_VASHealth, na.rm = TRUE),
                     Existing_VAS_6yrs = sum(!is.na(PREP_VASHealth) & !is.na(POSTP_VASHealth_6yrs)),
                     VAS_diff_6yrs = mean(POSTP_VASHealth_6yrs - PREP_VASHealth, na.rm = TRUE),
                     Existing_VAS_10yrs = sum(!is.na(PREP_VASHealth) & !is.na(POSTP_VASHealth_10yrs)),
                     VAS_diff_10yrs = mean(POSTP_VASHealth_10yrs - PREP_VASHealth, na.rm = TRUE)) %>%
    dplyr::ungroup() %>%
    dplyr::arrange(as.character(Unit)) %>%
    as.data.frame() -> prom_dat

  return(prom_dat)
}
