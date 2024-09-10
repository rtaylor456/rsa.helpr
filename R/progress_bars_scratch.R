read_and_clean_rsa_data <- reactive({
  req(input$rsa_data)

  # Show custom progress bar
  session$sendCustomMessage(type = 'showProgressBar', message = NULL)
  session$sendCustomMessage(type = 'updateProgressBar', message = 0)

  withProgress(message = 'Cleaning RSA-911 Data...', value = 0, {
    file_paths <- input$rsa_data$datapath
    file_types <- tools::file_ext(input$rsa_data$name)

    df_list <- mapply(function(path, type) {
      if (type == "csv") {
        fread(path, stringsAsFactors = FALSE)
      } else {
        as.data.table(read_excel(path))
      }
    }, file_paths, file_types, SIMPLIFY = FALSE)

    incProgress(1/3, detail = "Combining data...")
    df_combined <- do.call(rbind, df_list)

    # Assuming clean_utah() can be broken into steps
    # For example, process the data in chunks
    total_steps <- 10
    step_size <- nrow(df_combined) / total_steps
    df_cleaned <- data.table()

    incProgress(1/3, detail = "Cleaning data...")
    for (i in seq_len(total_steps)) {
      # Process a chunk of data
      start <- (i - 1) * step_size + 1
      end <- min(i * step_size, nrow(df_combined))
      chunk <- df_combined[start:end, ]

      # Call the clean_utah function on this chunk
      df_chunk_cleaned <- clean_utah(chunk,
                                     aggregate = input$aggregate_utah,
                                     unidentified_to_0 = input$unidentified_to_0,
                                     convert_sex = input$convert_sex,
                                     clean_specials = input$clean_specials,
                                     remove_desc = input$remove_desc,
                                     remove_strictly_na = input$remove_strictly_na)

      df_cleaned <- rbind(df_cleaned, df_chunk_cleaned)

      # Increment progress
      incProgress(1 / total_steps * (2/3), detail = paste("Cleaning chunk", i, "of", total_steps))
    }

    incProgress(1/3, detail = "Finalizing...")
    session$sendCustomMessage(type = 'updateProgressBar', message = 100)
    session$sendCustomMessage(type = 'hideProgressBar', message = NULL)

    rv$rsa_data_cleaned <- df_cleaned
    return(df_cleaned)
  })
})
