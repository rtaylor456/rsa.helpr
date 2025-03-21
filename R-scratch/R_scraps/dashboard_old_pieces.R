## within SERVER

# potential helper function to minimize redundant code
# Helper function to read and combine multiple files
read_and_combine_files <- function(file_paths, file_type) {
  if (file_type == "csv") {
    df_list <- lapply(file_paths, read.csv,
                      stringsAsFactors = FALSE, row.names = NULL)
  } else if (file_type == "xlsx") {
    df_list <- lapply(file_paths, read_excel)
  }
  df_combined <- do.call(rbind, df_list)
  return(df_combined)
}


## works great, just doesn't include progress bar
# Function to read and clean RSA-911 CSV files
read_and_clean_rsa_data <- reactive({
  req(input$rsa_data)

  file_paths <- input$rsa_data$datapath
  file_types <- tools::file_ext(input$rsa_data$name)

  # df_list <- mapply(function(path, type) {
  #   if (type == "csv") {
  #     read.csv(path, stringsAsFactors = FALSE, row.names = NULL)
  #   } else {
  #     read_excel(path)
  #   }
  # }, file_paths, file_types, SIMPLIFY = FALSE)

  df_list <- mapply(function(path, type) {
    if (type == "csv") {
      fread(path, stringsAsFactors = FALSE)
    } else {
      as.data.table(read_excel(path))
    }
  }, file_paths, file_types, SIMPLIFY = FALSE)


  df_combined <- do.call(rbind, df_list)
  # df_combined <- rbindlist(df_list, use.names = TRUE, fill = TRUE)

  df_cleaned <- clean_utah(df_combined,
                           aggregate = input$aggregate_utah,
                           unidentified_to_0 = input$unidentified_to_0,
                           convert_sex = input$convert_sex,
                           convert_employ = input$convert_employ,
                           clean_specials = input$clean_specials,
                           remove_desc = input$remove_desc,
                           remove_strictly_na = input$remove_strictly_na)
  rv$rsa_data_cleaned <- df_cleaned
  return(df_cleaned)
})


## again, works great, just missing progress bar
# Function to read and clean scores data files
read_and_clean_scores_data <- reactive({
  req(input$scores_data)
  file_paths <- input$scores_data$datapath
  file_types <- tools::file_ext(input$scores_data$name)

  # df_scores_list <- mapply(function(path, type) {
  #   if (type == "csv") {
  #     read.csv(path, stringsAsFactors = FALSE, row.names = NULL)
  #   } else {
  #     read_excel(path, row.names = NULL)
  #   }
  # }, file_paths, file_types, SIMPLIFY = FALSE)

  df_scores_list <- mapply(function(path, type) {
    if (type == "csv") {
      fread(path, stringsAsFactors = FALSE)
    } else {
      read_excel(path, row.names = NULL)
    }
  }, file_paths, file_types, SIMPLIFY = FALSE)


  df_scores_combined <- do.call(rbind, df_scores_list)
  # df_scores_combined <- rbindlist(df_scores_list, use.names = TRUE,
                                  # fill = TRUE)

  cleaned_scores <- clean_scores(df_scores_combined,
                                 aggregate = input$aggregate_scores)
  rv$scores_data_cleaned <- cleaned_scores
  return(cleaned_scores)
})


## again, works great, just missing progress bar
# Function to merge cleaned RSA-911 and scores data
merged_data <- reactive({
  req(read_and_clean_rsa_data(), read_and_clean_scores_data())
  merged <- merge_scores(read_and_clean_rsa_data(),
                         read_and_clean_scores_data(),
                         quarterly_ID = input$quarterly_ID,
                         scores_ID = input$scores_ID)
  rv$merged_data <- merged
  return(merged)
})


## again, works great, no progress bar
# Function to create metadata from merged data
generate_metadata <- eventReactive(input$generate_metadata, {
  # if no merged_data() has been created, create metadata from simply rsa-911
  if (is.null(merged_data()) | nrow(merged_data()) < 1){
    print("no merged data available")
    create_metadata(read_and_clean_rsa_data(),
                    includes_scores = FALSE)
  }
  # else if merged_data() has been created AND user has left use_merged box
  #   checked, use merged data (and turn on includes_scores argument in
  #   function)
  else if (input$use_merged){
    metadata <- create_metadata(merged_data(),
                                includes_scores = TRUE)
  # else, just use rsa-911 data if use doesn't want to use scores data
  } else {
    metadata <- create_metadata(read_and_clean_rsa_data(),
                                includes_scores = FALSE)
  }
  # metadata <- create_metadata(merged_data(), includes_scores = input$use_scores)
  rv$metadata <- metadata
  return(metadata)
})
