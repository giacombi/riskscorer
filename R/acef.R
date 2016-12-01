#' Calculates the ACEF score
#'
#' Function for calculation without limits
#'
#' @param age numeric age
#' @param crea serum creatinin level in mg/dl
#' @param lv_func Left ventricular fuction expressed with numeric %
#'
#' @return predicted probability of mortality
#' @export
acef <- function(age,
                 crea = NULL,
                 lv_func = NULL
) {

  acef_score <- age/lv_func
  if (crea >=2) {
      acef_score <- acef_score + 1
  }
  return(round(acef_score, digits = 4))
}

#' Calculates the ACEF
#'
#' Calculates the ACEF with limiting values
#'
#' @param age Age in years, must be within 20 and 100
#' @param crea Serum creatinin levels expresses with mg/dl
#' @param lv_func s based on left ventricular ejection fraction. Numeric
#'
#' @return the calculated risk score
#' @export
#' Plumber decoration
#* @get /calc_esII
#* @post /calc_esII
calc_acef <- function(age,
                      crea = NULL,
                      lv_func = NULL
) {
  queryList <- list()

  age <- readr::parse_number(age)
  queryList$age <- ensurer::ensure(age, is.numeric(.), . >= 1 , . <= 110)
  queryList$crea <- ensurer::ensure(crea, is.numeric(.), . >= 1 , . <= 300)
  queryList$lv_func <- ensurer::ensure(lv_func, is.numeric(.), . >= 1 , . <= 99)
  #print(queryList)
  do.call(acef, queryList)
}


