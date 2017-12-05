#' This function converts a six-digit numeric date (yyyymm) to a value in a proper date format
#' @param 
#' @keywords date, time
#' @export
#' @examples
#' num_to_date(200105)

num_to_date <- function(six_digit_date, day_of_month = 15) {
	require(dplyr)
    year <- substr(six_digit_date,1,4)
    month <- substr(six_digit_date,5,6)
    date <- paste0(year,"-",month,"-",day_of_month) %>% lubridate::ymd()
    return(date)
}