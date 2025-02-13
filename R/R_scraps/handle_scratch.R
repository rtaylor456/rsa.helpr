# handle_splits <- function(data, var_name){
#   # split up the values by the ";"
#   split_list <- strsplit(data[[var_name]], ";")
#   # now, find out the max_length of elements after splitting
#   max_length <- max(sapply(split_list, length))
#   # now, add blanks to each element that doesn't have the same length as the max
#   split_list <- lapply(split_list, function(x) {
#     length(x) <- max_length; x[is.na(x)] <- ""; x
#     })
#   # create a matrix of these values
#   split_matrix <- do.call(rbind, split_list)
#
#   # new_var_list <- list()
#   # for (i in 1:max_length) {
#   #   var_name_i <- sub("_911$", paste0("_var", i), var_name)
#   #   new_var_list[[var_name_i]] <- split_matrix[, i]
#   # }
#
#   # Create the names for the new variables
#   var_names <- paste0(sub("_911$", "", var_name), "_var", seq_len(max_length),
#                       "_911")
#
#   # Create the new variable list with setNames
#   new_var_list <- setNames(as.list(as.data.frame(split_matrix,
#                                                  stringsAsFactors = FALSE)),
#                            var_names)
#   # Append the new variables to the data
#   data <- cbind(data, new_var_list)
#
#   # Remove the original variable
#   data <- subset(data, select = -which(names(data) == var_name))
#
#   return(data)
# }



# handle_splits <- function(data, var_names){
#   for(var_name in var_names) {
#     # split up the values by the ";"
#     split_list <- strsplit(data[[var_name]], ";")
#     # now, find out the max_length of elements after splitting
#     max_length <- max(sapply(split_list, length))
#     # now, add blanks to each element that doesn't have the same length as the
#     #    max
#     split_list <- lapply(split_list, function(x) {
#       length(x) <- max_length; x[is.na(x)] <- ""; x
#     })
#     # create a matrix of these values
#     split_matrix <- do.call(rbind, split_list)
#
#     # Create the names for the new variables
#     var_names_new <- paste0(sub("_911$", "", var_name), "_var",
#                             seq_len(max_length),
#                             "_911")
#
#     # Create the new variable list with setNames
#     new_var_list <- setNames(as.list(as.data.frame(split_matrix,
#                                                    stringsAsFactors = FALSE)),
#                              var_names_new)
#     # Append the new variables to the data
#     data <- cbind(data, new_var_list)
#   }
#
#   # Remove the original variables
#   data <- subset(data, select = -which(names(data) %in% var_names))
#
#   return(data)
# }




# apply_handle_splits <- function(data, special_cols, sep = ";") {
#   # Filter columns that exist in the data
#   existing_cols <- intersect(special_cols, names(data))
#
#   # Apply handle_splits2 function to each column in existing_cols using map
#   split_results <- map(existing_cols, ~ handle_splits2(data[[.]], .x, sep))
#
#   # Combine the results with the original data
#   data <- bind_cols(data, as.data.frame(split_results, stringsAsFactors = FALSE))
#
#   # Remove the original columns
#   data <- select(data, -one_of(existing_cols))
#
#   return(data)
# }



# shorten_provider_names <- function(names, method = c("initials",
#                                                      "first_full")) {
#   sapply(names, function(name) {
#     words <- unlist(strsplit(name, " "))  # Split name into words
#     if (length(words) == 1) {
#       return(words)  # Return as is if only one word
#     } else if (method == "first_full") {
#       return(paste0(words[1], paste(substr(words[-1], 1, 1),
#                                     collapse = "")))
#       # Keep first word, initials of the rest
#     } else {  # Default: initials of all words
#       return(paste0(substr(words, 1, 1), collapse = ""))
#     }
#   })
# }
