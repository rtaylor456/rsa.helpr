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
  View()
