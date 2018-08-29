#' Revisions
#'
#' Get number of revisions for each unit and specific period.
#' @param period Year/years of revision.
#' @return Data frame with units, period, number of revisions, number of THR and number of hemi.
#' @examples
#' # Get number of revisions for current year
#' revision_op()
#' # Get number of revisions for 2016-2017
#' revision_op(2016:2017)
#' # Get number of revisions for 1900-2020 (all existing years)
#' revision_op(1900:2020)
#' @export

revision_op <- function(period = lubridate::year(Sys.Date())){
  require(dplyr)
  if(!"dataOperations" %in% ls(envir = .GlobalEnv)){
    load("//rc-r/r$/Datalayers/SHPR/.RData", envir = .GlobalEnv)
  }

  period_string <- ifelse(length(period) == 1, period, paste(period[1], period[length(period)], sep = "-"))

  hosp_dat <- dplyr::select(dataOperations, SubjectKey, R_SurgDate, P_Side, R_Unit, P_ProstType, R_ReSurgType) %>%
    dplyr::mutate(R_SurgDate = as.Date(R_SurgDate, tz = "CET"), Operation_year = lubridate::year(R_SurgDate)) %>%
    dplyr::filter(Operation_year %in% period,
                  R_ReSurgType != 903) %>%
    dplyr::mutate(Unit = attr_to_factor(R_Unit))

  hosp_dat %>%
    dplyr::group_by(Unit) %>%
    dplyr::summarise(Operation_year = period_string,
                     Count = n(),
                     THR =  sum(P_ProstType == 1, na.rm = TRUE),
                     Hemi =  sum(P_ProstType == 2, na.rm = TRUE)) %>%
    dplyr::ungroup() %>%
    dplyr::arrange(as.character(Unit)) %>%
    as.data.frame() -> revision_ops

  return(revision_ops)
}
