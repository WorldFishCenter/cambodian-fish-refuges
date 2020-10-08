# make an id based on a prefix and a number
make_id <- function(x, prefix){
  stopifnot(is.numeric(x))
  paste0(prefix,
         stringr::str_pad(string = x,
                          width = floor(log10(max(x))) + 1,
                          pad = "0"))
}

# check whether there is a single row for each value in column_name
has_single_row <- function(df, column_name){
  df %>%
    dplyr::count({{column_name}}) %$%
    magrittr::is_greater_than(n, 1) %>%
    any()
}
