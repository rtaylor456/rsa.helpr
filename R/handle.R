#' Separate disability
#'
#' @import tidyverse
separate_disability <- function(df) {
  df %>%
    tidyr::separate(E43_Primary_Disability_911,
                    into = c("E43_Primary_Impairment_911",
                             "E43_Primary_Cause_911"),
                    sep = ";") %>%
    tidyr::separate(E44_Secondary_Disability_911,
                    into = c("E44_Secondary_Impairment_911",
                             "E44_Secondary_Cause_911"),
                    sep = ";")
}

separate_disability <- function(var) {
  tidyr::separate(var, into = c(""))
}
