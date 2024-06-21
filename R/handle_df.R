#' Separate disability
#'
#' @import data.table
#' @import tidyverse

# using tidyverse
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

# using data.table
# separate_disability <- function(df) {
#   # Convert to data.table if not already
#   setDT(df)
#
#   # Separate E43_Primary_Disability_911 into E43_Primary_Impairment_911 and
#   #   E43_Primary_Cause_911
#   df[, c("E43_Primary_Impairment_911",
#          "E43_Primary_Cause_911") := tstrsplit(E43_Primary_Disability_911,
#                                                ";",
#                                                fixed = TRUE)]
#
#   # Separate E44_Secondary_Disability_911 into E44_Secondary_Impairment_911 and
#   #   E44_Secondary_Cause_911
#   df[, c("E44_Secondary_Impairment_911",
#          "E44_Secondary_Cause_911") := tstrsplit(E44_Secondary_Disability_911,
#                                                  ";",
#                                                  fixed = TRUE)]
#
#   return(df)
# }


# using tidyverse
handle_id_repeats <- function(df){
  df_clean <- df |>
    group_by(Participant_ID, E1_Year_911, E2_Quarter_911) |>
    mutate(occurrences_per_quarter = n()) |>
    slice(which.max(E7_Application_Date_911)) |>
    ungroup()
  return(df_clean)
}

# using data.table
# this isn't faster than the version using the tidyverse it seems :(
# handle_id_repeats2 <- function(df) {
#   # Ensure the input is a data.table
#   setDT(df)
#
#   # Group by Participant_ID, E1_Year_911, and E2_Quarter_911 and compute
#   #   occurrences_per_quarter
#   df[, occurrences_per_quarter := .N, by = .(Participant_ID, E1_Year_911,
#                                              E2_Quarter_911)]
#
#   # Convert date variables to actual date representation
#   date_vars <- grep("E7_Application_Date_911", names(df), value = TRUE)
#   df[, (date_vars) := lapply(.SD, function(x) as.Date(as.numeric(x),
#                                                       origin = "1899-12-30")),
#      .SDcols = date_vars]
#
#   # Select the row with the maximum E7_Application_Date_911 for each group
#   df_clean <- df[, .SD[which.max(E7_Application_Date_911)],
#                  by = .(Participant_ID, E1_Year_911, E2_Quarter_911)]
#
#   return(df_clean)
# }
