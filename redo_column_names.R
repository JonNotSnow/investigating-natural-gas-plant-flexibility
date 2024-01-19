redo_col_names <- function(df_in) {
  temp <- df_in
  
  temp <- temp %>%
    rename_with(~ gsub("\r\n", "_", .x, fixed = TRUE)) %>%
    rename_with(~ gsub("\n", "_", .x, fixed = TRUE)) %>%
    rename_with(~ gsub(" ", "_", .x, fixed = TRUE)) %>%
    rename_with(~ gsub("__", "_", .x, fixed = TRUE)) %>% 
    rename_with(~ gsub("(", "", .x, fixed = TRUE)) %>%
    rename_with(~ gsub(")", "", .x, fixed = TRUE)) %>%
    rename_with(~ gsub("?", "", .x, fixed = TRUE)) %>%
    rename_with(~ gsub("...", "_", .x, fixed = TRUE))
  
  return(temp)
}