#' This is a date math function for months
#' @param 
#' @keywords date, month, time
#' @export
#' @examples
#' add_months('2017-11-19')
#' add_months(your_boos_bday)
#' add_months()

add_months <- function(date, add_months = 1) {
    require(dplyr)
    add_months_frac <- add_months/12
    date_char <- date %>% as.character()
    date_yearmon <- date %>% zoo::as.yearmon()
    new_date_yearmon <- date_yearmon + add_months_frac
    new_date_char <- new_date_yearmon %>% as.Date() %>% as.character()
    year <- substr(new_date_char,1,4)
    month <- substr(new_date_char,6,7)
    day <- substr(date_char,9,10)
    new_date <- paste0(year,"-",month,"-",day) %>% lubridate::ymd()
    return(new_date)
}