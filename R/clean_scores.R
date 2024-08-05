library(tidyverse)
library(data.table)
library(lubridate)

clean_scores <- function(data,
                         aggregate = TRUE) {

  # Convert scores to data.table
  setDT(data)

  # Remove "(MST)" and convert 'Completed' to POSIXct
  data[, Completed := mdy_hms(gsub(" \\(MST\\)", "", Completed))]

  # Group by Participant.ID, Service, Pre.Post and calculate the count
  data[, count := .N, by = .(Participant.ID, Service, Pre.Post)]

  # Create Has_Multiple_Scores column
  data[, Has_Multiple_Scores := as.factor(ifelse(count > 1, 1, 0))]

  # Remove the count column
  data[, count := NULL]

  # Convert to factors
  data[, c("Participant.ID", "Service", "Provider",
           "Has_Multiple_Scores") := lapply(.SD, as.factor),
       .SDcols = c("Participant.ID", "Service", "Provider",
                   "Has_Multiple_Scores")]

  # Group by Participant.ID, Service and calculate the Time_Passed_Days
  data[, Time_Passed_Days := as.numeric(difftime(max(Completed),
                                                      min(Completed),
                                                      units = "days")),
            by = .(Participant.ID, Service)]
  data[, Time_Passed_Days := round(Time_Passed_Days)]

  # Sort by Participant.ID
  setorder(data, Participant.ID)


  # Filter rows where Pre.Post is "Pre"
  pre_data <- data[Pre.Post == "Pre"]

  # Filter rows where Pre.Post is "Post"
  post_data <- data[Pre.Post == "Post"]

  if (aggregate == TRUE){
    # Find the earliest 'Pre' record for each Participant.ID, Provider, Service
    pre_data <- pre_data[order(Completed), .SD[1],
                             by = .(Participant.ID, Provider, Service)]

    # Find the latest 'Post' record for each Participant.ID, Provider, Service
    post_data <- post_data[order(-Completed), .SD[1],
                             by = .(Participant.ID, Provider, Service)]
  }

  # Select and rename columns in earliest_pre
  pre_selected <- pre_data[, .(Participant.ID,
                                            Completed_Pre = Completed,
                                            Service,
                                            Provider,
                                            Has_Multiple_Scores,
                                            Time_Passed_Days,
                                            Pre_Score = Score)]

  # Select and rename columns in latest_post
  post_selected <- post_data[, .(Participant.ID,
                                          Completed_Post = Completed,
                                          Service,
                                          Provider,
                                          Has_Multiple_Scores,
                                          Time_Passed_Days,
                                          Post_Score = Score,
                                          Difference)]

  # Perform a full join
  merged_data <- merge(pre_selected, post_selected,
                       by = c("Participant.ID", "Service", "Provider",
                              "Has_Multiple_Scores", "Time_Passed_Days"),
                       all = TRUE)

  # need tidyverse for this--data.table way isn't as clean
  scores_final <- merged_data |>
    pivot_wider(
      names_from = Service,
      values_from = c(Pre_Score, Post_Score, Difference, Time_Passed_Days),
      names_glue = "{.value}_{Service}"
    )

  # Reshape the data
  # scores_final <- dcast(
  #   merged_data,
  #   formula = . ~ Service,
  #   value.var = c("Pre_Score", "Post_Score", "Difference", "Time_Passed_Days"),
  #   fun.aggregate = list
  # )
  #
  # # Adjust column names to match names_glue pattern
  # setnames(scores_final, old = names(scores_final)[grep("^(Pre_Score|Post_Score|Difference|Time_Passed_Days)_", names(scores_final))],
  #          new = gsub("^(Pre_Score|Post_Score|Difference|Time_Passed_Days)_(.+)", "\\1_\\2", names(scores_final)))


  return(scores_final)

}






# dim(scores_final)
# [1] 8414   45

# using tidyverse
# clean_scores2 <- function(data,
#                          aggregate = TRUE) {
#
#   # Group by participant.id, service, and pre.post, and count the scores
#   scores2 <- data |>
#     mutate(Completed = mdy_hms(gsub(" \\(MST\\)", "", Completed))) |>
#     group_by(Participant.ID, Service, Pre.Post) |>
#     mutate(count = n()) |>
#     ungroup() |>
#     mutate(Has_Multiple_Scores = as.factor(if_else(count > 1, 1, 0))) |>
#     select(-count) |>
#     group_by(Participant.ID, Service) |>
#     mutate(Time_Passed_Days = as.numeric(difftime(max(Completed),
#                                                   min(Completed),
#                                                   units = "days"))) |>
#     mutate(Time_Passed_Days = round(Time_Passed_Days)) |>
#     # mutate(Time_Passed_Days = format(Time_Passed_Days, scientific = FALSE)) |>
#     ungroup() |>
#     arrange(Participant.ID)
#
#
#   pre_data <- scores2 |>
#     filter(Pre.Post == "Pre")
#
#   post_data <- scores2 |>
#     filter(Pre.Post == "Post")
#
#   if (aggregate == TRUE) {
#     pre_data <- pre_data |>
#       arrange(Completed) |>
#       group_by(Participant.ID, Provider, Service, Completed) |>
#       slice(1)
#     post_data <- post_data |>
#       arrange(desc(Completed)) |>
#       group_by(Participant.ID, Provider, Service) |>
#       slice(1)
#   }
#
#   merged_data <- full_join(
#     pre_data %>%
#       select(Participant.ID,
#              Completed_Pre = Completed,
#              Service, Provider,
#              Has_Multiple_Scores,
#              Time_Passed_Days,
#              Pre_Score = Score),
#     post_data %>%
#       select(Participant.ID,
#              Completed_Post = Completed,
#              Service, Provider,
#              Has_Multiple_Scores,
#              Time_Passed_Days,
#              Post_Score = Score,
#              Difference),
#     by = c("Participant.ID", "Service", "Provider",
#            "Has_Multiple_Scores", "Time_Passed_Days")
#   )
#
#
#   scores_final <- merged_data %>%
#     pivot_wider(
#       names_from = Service,
#       values_from = c(Pre_Score, Post_Score, Difference, Time_Passed_Days),
#       names_glue = "{.value}_{Service}"
#     )
#
#   return(scores_final)
#
# }


# # compare time, to see if using data.table is worth it
# start1 <- Sys.time()
# try1 <- clean_scores(scores)
# end1 <- Sys.time()
# time1 <- end1 - start1
#
# start2 <- Sys.time()
# try2 <- clean_scores2(scores)
# end2 <- Sys.time()
# time2 <- end2 - start2
#
# time1
# time2
# # clean_scores is twice as fast as clean_scores2

