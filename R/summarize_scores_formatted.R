library(data.table)

summarize_scores_formatted <- function(data, robust_measures = TRUE,
                                       save_output = FALSE,
                                       file_name = "summary_scores.csv") {
  # Convert to data.table if not already
  dt <- as.data.table(data)

  # Extract service acronyms from column names
  services <- unique(gsub("^(Pre_Score|Post_Score|Difference)_", "",
                          grep("^(Pre_Score|Post_Score|Difference)_[A-Z]+$",
                               names(dt), value = TRUE)))

  # Create an empty list to store results
  summary_list <- list()

  # Loop through each service and calculate summary statistics
  for (service in services) {
    pre_col <- paste0("Pre_Score_", service)
    post_col <- paste0("Post_Score_", service)
    diff_col <- paste0("Difference_", service)

    if (all(c(pre_col, post_col, diff_col) %in% names(dt))) {
      # Compute standard and robust measures
      pre_mean_sd <- sprintf("%.2f (%.2f)", mean(dt[[pre_col]], na.rm = TRUE),
                             sd(dt[[pre_col]], na.rm = TRUE))
      post_mean_sd <- sprintf("%.2f (%.2f)", mean(dt[[post_col]], na.rm = TRUE),
                              sd(dt[[post_col]], na.rm = TRUE))
      diff_mean_sd <- sprintf("%.2f (%.2f)", mean(dt[[diff_col]], na.rm = TRUE),
                              sd(dt[[diff_col]], na.rm = TRUE))

      # Count valid (non-NA) entries
      pre_count <- sum(!is.na(dt[[pre_col]]))
      post_count <- sum(!is.na(dt[[post_col]]))
      diff_count <- sum(!is.na(dt[[diff_col]]))

      counts_str <- paste0("Pre: ", pre_count, "; Post: ", post_count,
                           "; Diff: ", diff_count)

      if (robust_measures) {
        pre_median_iqr <- sprintf("%.2f (%.2f)", median(dt[[pre_col]],
                                                        na.rm = TRUE),
                                  IQR(dt[[pre_col]], na.rm = TRUE))
        post_median_iqr <- sprintf("%.2f (%.2f)", median(dt[[post_col]],
                                                         na.rm = TRUE),
                                   IQR(dt[[post_col]], na.rm = TRUE))
        diff_median_iqr <- sprintf("%.2f (%.2f)", median(dt[[diff_col]],
                                                         na.rm = TRUE),
                                   IQR(dt[[diff_col]], na.rm = TRUE))

        summary_list[[service]] <- rbindlist(list(
          data.table(
            Service = service,
            Measure = "Mean (SD)",
            Pre = pre_mean_sd,
            Post = post_mean_sd,
            Diff = diff_mean_sd
          ),
          data.table(
            Service = service,
            Measure = "Median (IQR)",
            Pre = pre_median_iqr,
            Post = post_median_iqr,
            Diff = diff_median_iqr
          ),
          data.table(
            Service = service,
            Measure = "Counts",
            Pre = pre_count,
            Post = post_count,
            Diff = diff_count
          )
        ))
      } else {
        summary_list[[service]] <- rbindlist(list(
          data.table(
            Service = service,
            Measure = "Mean (SD)",
            Pre = pre_mean_sd,
            Post = post_mean_sd,
            Diff = diff_mean_sd
          ),
          data.table(
            Service = service,
            Measure = "Counts",
            Pre = pre_count,
            Post = post_count,
            Diff = diff_count
          )
        ))
      }
    }
  }

  # Combine into a final table
  summary_table <- rbindlist(summary_list)

  # Save as CSV if requested
  if (save_output) {
    fwrite(summary_table, file = file_name)
    message("Summary table saved as: ", file_name)
  }

  return(summary_table)
}



combine_summaries <- function(var_df, not_var_df, var_name, var_name2,
                              output_csv = FALSE) {
  # Ensure data.table
  library(data.table)
  var_df <- as.data.table(var_df)
  not_var_df <- as.data.table(not_var_df)

  # Rename columns
  setnames(var_df, old = c("Pre", "Post", "Diff"),
           new = c(paste0("Pre_", var_name), paste0("Post_", var_name),
                   paste0("Diff_", var_name)))

  setnames(not_var_df, old = c("Pre", "Post", "Diff"),
           new = c(paste0("Pre_", var_name2), paste0("Post_", var_name2),
                   paste0("Diff_", var_name2)))

  # Merge datasets on Service and Measure
  merged <- merge(
    var_df,
    not_var_df,
    by = c("Service", "Measure"),
    all = TRUE
  )

  # Reorder columns
  setcolorder(merged, c(
    "Service", "Measure",
    paste0("Pre_", var_name), paste0("Pre_", var_name2),
    paste0("Post_", var_name), paste0("Post_", var_name2),
    paste0("Diff_", var_name), paste0("Diff_", var_name2)
  ))

  # Save as CSV if requested
  if (output_csv) {

    file_name <- paste0("combined_summary_", var_name, "_", var_name2)

    fwrite(merged, file = file_name)
    message("Combined summary table saved as: ", file_name)
  }

  return(merged)
}
