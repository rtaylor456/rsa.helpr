library(dplyr)

bad_ids <- metadata |>
  filter(if_any(contains("Time_Passed"), ~ . < 0)) |>
  pull(Participant_ID)

length(bad_ids)

# 11100  98902  107215 108352 108369 113798 115880 115969 118892 119223 121042
