#' Clean Scores Data, with Provider Variables Focus
#'
#' This function cleans a scores dataset, based on the standard data
#'   structure, returning a dataset focused on provider-variable analysis.
#'
#' @param data The scores dataset.
#' @param condense Defaults to FALSE. When true, take medians across
#'  participants.
#'
#' @returns A cleaned data frame, including only provider-relevant variables,
#'   restructured.
#'
#' @export
#' @import data.table


clean_provider <- function(data, condense = FALSE) {
  # Convert to data.table
  setDT(data)

  # Identify dynamic column names
  participant <- grep("(?i)^(?=.*participant)|(?=.*\\bid\\b)(?!.*\\bid\\B)",
                      names(data), value = TRUE, perl = TRUE)
  state <- grep("(?i)^(?=.*state)|(?=.*\\bst\\b)(?!.*\\bst\\B)",
                names(data), value = TRUE, perl = TRUE)
  provider <- grep("(?i)provider", names(data), value = TRUE, perl = TRUE)
  service <- grep("(?i)serv", names(data), value = TRUE, perl = TRUE)
  mode <- grep("(?i)mode", names(data), value = TRUE, perl = TRUE)
  pre_post <- grep("(?i)^(?=.*pre)(?=.*post)",
                   names(data), value = TRUE, perl = TRUE)
  completed <- grep("(?i)^(?=.*complete)|(?=.*date)",
                    names(data), value = TRUE, perl = TRUE)
  caseload <- grep("(?i)^(?=.*case)|(?=.*caseload)|(?=.*workload)",
                    names(data), value = TRUE, perl = TRUE)
  group_freq <- grep("(?i)^(?=.*group)|(?=.*grp)(?=.*freq)",
                     names(data), value = TRUE, perl = TRUE)
  online_freq <- grep("(?i)^(?=.*online)(?=.*freq)",
                     names(data), value = TRUE, perl = TRUE)
  rural_freq <- grep("(?i)^(?=.*rural)(?=.*freq)",
                     names(data), value = TRUE, perl = TRUE)

  question_cols <- grep("(?i)(q_|question)", names(data), value = TRUE,
                        perl = TRUE)

  if (remove_questions) {
    data[, (question_cols) := NULL]
  }

  # Rename columns for consistency
  names(data)[names(data) %in% participant] <- "Participant_ID"
  names(data)[names(data) %in% state] <- "State"
  names(data)[names(data) %in% provider] <- "Provider"
  names(data)[names(data) %in% service] <- "Service"
  names(data)[names(data) %in% mode] <- "Mode"
  names(data)[names(data) %in% pre_post] <- "Pre_Post"
  names(data)[names(data) %in% completed] <- "Completed"
  names(data)[names(data) %in% caseload] <- "Caseload"
  names(data)[names(data) %in% group_freq] <- "Group_freq"
  names(data)[names(data) %in% online_freq] <- "Online_freq"
  names(data)[names(data) %in% rural_freq] <- "Rural_freq"


  # Apply state filter, if provided by user
  if (!is.null(state_filter)) {
    data <- data[State %in% state_filter]
  }

  # Order data by participant
  data <- data[order(Participant_ID)]

  ## CONVERT VARIABLES

  # Clean up Provider (remove stuff in parentheses)
  data[, (provider) := lapply(.SD, function(x) sub("\\s*\\([^\\)]+\\)",
                                                   "", x)),
       .SDcols = provider]
  # convert to factor

  # Remove "(MST)" and convert 'Completed' to POSIXct
  data[, Completed := as.POSIXct(gsub(" \\(MST\\)", "", Completed),
                                 format = "%m/%d/%Y %H:%M:%S", tz = "UTC")]

  # Clean and convert Caseload
  # Nominal: general, specialized, NULL
  # Clean and convert Caseload (Nominal)
  data[, Caseload := tolower(Caseload)]
  data[, Caseload := ifelse(Caseload %in% c("general", "specialized"),
                            Caseload, NA_character_)]

  # Clean and convert frequency variables
  # Ordinal: never > rarely > sometimes > often > always, NULL
  freq_levels <- c("never", "rarely", "sometimes", "often", "always")
  frequency_vars <- c("Group_freq", "Online_freq", "Rural_freq")
  for (var in frequency_vars) {
    data[[var]] <- tolower(data[[var]])
    data[[var]] <- factor(data[[var]], levels = freq_levels, ordered = TRUE)
  }

  # Clean and convert Mode
  # Ordinal: no help > observer > with help, NULL
  mode_levels <- c("no help", "observer", "with help")
  data[, Mode := tolower(Mode)]
  data[, Mode := factor(Mode, levels = mode_levels, ordered = TRUE)]

  # Clean and convert Pre_Post
  # Ordinal: no help > observer > with help, NULL
  pre_post_levels <- c("pre", "post")
  data[, Pre_Post := tolower(Pre_Post)]
  data[, Pre_Post := factor(Pre_Post, levels = pre_post_levels, ordered = TRUE)]

  # Keep only the most recent score per Participant, Provider, and Service
  data <- data[order(Participant_ID, Provider, Service, -Completed)]
  data <- data[, .SD[1], by = .(Participant_ID, Provider, Service)]

  data[, c("Completed", "Participant_ID") := NULL]


  # Condense the data if 'condense = TRUE'
  if (condense) {
    data <- data[, .(Score = median(Score, na.rm = TRUE),
                     Difference = median(Difference, na.rm = TRUE),
                     Mode = names(sort(table(Mode), decreasing = TRUE))[1],
                     Provider = unique(Provider),
                     Service = unique(Service),
                     Pre_Post = unique(Pre_Post),
                     Caseload = names(sort(table(Caseload),
                                           decreasing = TRUE))[1],
                     Group_freq = names(sort(table(Group_freq),
                                             decreasing = TRUE))[1],
                     Online_freq = names(sort(table(Online_freq),
                                              decreasing = TRUE))[1],
                     Rural_freq = names(sort(table(Rural_freq),
                                             decreasing = TRUE))[1]),
                 by = .(Provider, Service, Pre_Post)]
  }



  # Sort by Provider, Service, and Pre_Post
  setorder(data, Provider, Service, Pre_Post)

  return(data)
}
