#' Load SLR data layer
#'
#' @param  form Which tables to load. Either 'all', 'allhip', 'allknee', or one or several FormIDs.
#' '1002' HipOperation (primary + reoperations)
#' '2259' HipEnvironment
#' '2253' HipPreProm
#' '2254' HipPostProm
#' '2334' KneeOperation (primary + reoperations)
#' '2346' KneePreProm
#' '2347' KneePostProm
#' '2335' OsteoOperation (primary + reoperations)
#' 'all'  All forms
#' 'allhip' All hip forms
#' 'allknee' All knee forms
#' @param  functions Include functions from data layer?
#' @examples
#' \dontrun{
#' get_data()
#' get_data('allhip')
#' get_data(form = c(1002, 2334), functions = TRUE)
#' }
#' @export

get_data <- function(form = 'all',
                     functions = FALSE){

  hipFormID <- c(1002, 2259, 2253, 2254)
  kneeFormID <- c(2334, 2346, 2347, 2335)

  if ('all' %in% form) {

    formID <- c(hipFormID, kneeFormID)

  } else if ('allknee' %in% form){

    formID <- kneeFormID

  } else if ('allhip' %in% form){

    formID <- hipFormID

  } else {

    formID <- form

  }


  if (functions) {
    formID <- c(formID, 'functions')

  }


  for (i in formID){
    load(
      paste(
        '//rc-r/r$/Datalayers/SLR/',
        i,
        '.RData', sep = ''),
      envir = .GlobalEnv
    )
  }
}
