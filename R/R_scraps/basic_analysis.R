library(tidyverse)

race_cols <- grep("(?i)(_indian|_asian|_black|_hawaiian|_islander|_white|hispanic)(?!.*(?i)_desc)",
                  names(metadata),
                  value = TRUE, perl = TRUE)

# proportions <- metadata |> select(any_of(race_cols)) |>
#   lapply(function(x) round(table(x) / nrow(metadata), 2))

race_proportions <- metadata |>
  select(any_of(race_cols)) |>
  lapply(function(x) round(sum(x == 1) / nrow(metadata), 2))

race_proportions <- race_proportions[order(unlist(race_proportions),
                                           decreasing = TRUE)]

race_proportions

skills_deficient <- metadata |>
  select(E69_Plan_Skills_Deficient_911) |>
  # table() / nrow(metadata) |>
  # round(3) |>
  lapply(function(x) round(sum(x == 1) / nrow(metadata), 2))

skills_deficient

prov_cols <- grep("((?i)_provide)(?!.*(?i)_desc)",
                        names(metadata), value = TRUE, perl = TRUE)

provided_proportions <- metadata |>
  select(any_of(prov_cols)) |>
  lapply(function(x) round(sum(x == 1) / nrow(metadata), 2))

provided_proportions <- provided_proportions[order(unlist(provided_proportions),
                                          decreasing = TRUE)]

head(provided_proportions)


