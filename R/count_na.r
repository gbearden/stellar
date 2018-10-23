#' This function counts NAs in a data frame for each variable
#' Note that input data object must be a data frame
#' @param
#' @keywords na, data frame
#' @export
#' @examples
#' count_na(your_df)

count_na <- function(df){
	require(dplyr)

    nas <- NA

    for(i in 1:ncol(df)){
        nas[i] <- is.na(df[,i]) %>% sum()
    }

    na_df <- dplyr::data_frame(var = df %>% colnames(), na_count = nas) %>%
        dplyr::arrange(desc(na_count))

    return(na_df)
}
