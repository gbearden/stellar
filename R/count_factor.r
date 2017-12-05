#' This function counts factors in a data frame for each variable
#' @param 
#' @keywords factor, data frame
#' @export
#' @examples
#' count_factor(your_df)

count_factor <- function(df){
	require(dplyr)

	factors_df <- df %>% 
	mutate_if(is.character, as.factor) %>% 
	select_if(is.factor) %>% 
	gather('variable', 'value', 1:ncol(.)) %>% 
	group_by(variable) %>% 
	summarise(n_factors = n_distinct(value)) %>%
	arrange(desc(n_factors))

    return(factors_df)
}
