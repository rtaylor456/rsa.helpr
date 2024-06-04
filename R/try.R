data_merged <- readRDS("data-raw/data_merged.rds")

py20 <- read.csv("data-raw/original/PY20exit.csv")

py21 <- read.csv("data-raw/original/PY21exit.csv")

data_cleaned <- readRDS("data-raw/data_cleaned.rds")
View(data_cleaned)

names(data)
names(py20)
names(py21)
View(data_merged[c(29, 30, 33:36)])

data_cleaned_snippet <- data_cleaned |>
  select(E38_Eligibility_Date_911, E39_Eligibility_Extension_911,
         E42_Has_Disability_911, contains("E43"))

View(data_cleaned_snippet)

intersect(names(data), names(py20))

library(tidyverse)
data <- data_merged |>
  select(-contains("desc"))

# py20 vs utah
# amt spent: "title", "amt" - ut
#


