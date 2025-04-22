library(tidyverse)

# uncleaned, character, just to do basic check
table(data$`Age at Application`)

# see individual counts
table(as.numeric(data$`Age at Application`))

# numeric summary
summary(as.numeric(data$`Age at Application`))

# plot
hist(as.numeric(data$`Age at Application`),
     main = "Age at Application, raw data", xlab = "Age in Years")

# look at data to see what's going on
data |> mutate(E7_Application_Date_911 = rsa.helpr::handle_excel_date(
  E7_Application_Date_911)) |>
  select(Participant_ID, E7_Application_Date_911 ,
               `Age at Application`, E74_SWD_Age_911) |>
  filter(`Age at Application` > 22) |>
  View()


library(tidyverse)

# Filter metadata and select Participant_ID values where Age_at_Application > 22
metadata |>
  filter(Age_At_Application > 22) |>
  pull(Participant_ID)

# [1] 11100  11885  71108  109122 113478 115894 121299 121742 123446 126377
#    130506 132387 133406 134382

data |> mutate(age = as.numeric(`Age at Application`)) |>
  filter(age > 22) |>
  pull(Participant_ID) |>
  unique() |>
  length()

# 28,335 unique participants in raw data who are over 22

data_clean |> filter(Age_At_Application > 22) |>
  pull(Participant_ID) |>
  unique() |>
  length()

# 26,797 unique participants in cleaned full RSA-911 data who are over 22

merged |> filter(Age_At_Application > 22) |>
  pull(Participant_ID) |>
  unique() |>
  length()
# 14

metadata |> filter(Age_At_Application > 22) |>
  select(E7_Application_Date_911, Participant_ID, Age_At_Application,
         E74_SWD_Age_911, E77_Plan_Grade_Level_911,
         E22_SWD_911) |>
  View()

check <- metadata |> filter(Age_At_Application > 22) |>
  select(E7_Application_Date_911, Participant_ID, Age_At_Application,
         E74_SWD_Age_911, E77_Plan_Grade_Level_911,
         E22_SWD_911)
check$Participant_ID
