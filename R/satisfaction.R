#' Satisfaction PROM
#'
#' Number of satisfied/unsatisfied patients 1 year after primary operation for a specific period. Satisfaction associated with the hip that is operated.
#' @param period Year/years of primary operation.
#' @return Data frame with units, period, number of operations, number of THR, number of hemi,
#' existing satisfaction data, number of satisfied (answered 4 or 5) and number of unsatisfied (answered 1 or 2).
#' @examples
#' # Get number of satisfied/unsatisfied for current year
#' # (does not exist since not enough follow up time)
#' satisfaction()
#' # Get number of satisfied/unsatisfied for 2016-2017
#' satisfaction(2016:2017)
#' # Get number of satisfied/unsatisfied for 1900-2020 (all existing years)
#' satisfaction(1900:2020)
#' @export


satisfaction <- function(period = lubridate::year(Sys.Date())){
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

  hosp_dat <- dplyr::select(dataOperations, SubjectKey, P_SurgDate, P_Side, P_Unit, P_ProstType) %>%
    dplyr::mutate(P_SurgDate = as.Date(P_SurgDate, tz = "CET"), Operation_year = lubridate::year(P_SurgDate)) %>%
    dplyr::filter(Operation_year %in% period) %>%
    dplyr::arrange(SubjectKey, P_Side, P_SurgDate) %>%
    dplyr::distinct(SubjectKey, P_Side, .keep_all = TRUE) %>%
    dplyr::mutate(Unit = attr_to_factor(P_Unit))

  prims <- dplyr::distinct(hosp_dat, SubjectKey, P_SurgDate)

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

  # Satisfaction variable

  hosp_dat <- dplyr::mutate(hosp_dat,
                            POSTP_Satisfaction_1yr = case_when(!is.na(POSTP_Satisfaction_1yr) ~ POSTP_Satisfaction_1yr,
                                                               P_Side == 1 ~ POSTP_SatisfactionR_1yr,
                                                               P_Side == 2 ~ POSTP_SatisfactionL_1yr))

  hosp_dat %>%
    dplyr::group_by(Unit) %>%
    dplyr::summarise(Operation_year = period_string,
                     Count = n(),
                     THR =  sum(P_ProstType == 1, na.rm = TRUE),
                     Hemi =  sum(P_ProstType == 2, na.rm = TRUE),
                     Existing_satisfaction_1yr = sum(!is.na(POSTP_Satisfaction_1yr)),
                     Satisfied_1yr = sum(POSTP_Satisfaction_1yr > 3, na.rm = TRUE),
                     Unsatisfied_1yr = sum(POSTP_Satisfaction_1yr < 3, na.rm = TRUE)) %>%
    dplyr::ungroup() %>%
    dplyr::arrange(as.character(Unit)) %>%
    as.data.frame() -> prom_dat

  return(prom_dat)
}
