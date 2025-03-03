library(pryr)

# Calculate memory needed for current RSA data upload
# Measure memory before
rsa_mem_before <- mem_used()
data <- data.table::fread("data-raw/data_load_2025-02-17.csv",
                          stringsAsFactors = FALSE)
# Measure memory after
rsa_mem_after <- mem_used()
# Calculate the difference
rsa_memory_needed <- rsa_mem_after - rsa_mem_before
rsa_memory_needed # 1.47 GB -- this is the issue,


# Calculate memory needed for current TRT scores data upload
scores_mem_before <- mem_used()
scores <- data.table::fread("data-raw/TRT Data_1.28.2025 at 12_00pm.csv",
                            stringsAsFactors = FALSE)
scores_mem_after <- mem_used()
scores_memory_needed <- scores_mem_after - scores_mem_before
scores_memory_needed # 10 MB

################################################################################

library(profvis)

data <- data.table::fread("data-raw/data_load_2025-02-17.csv",
                          stringsAsFactors = FALSE)

scores <- data.table::fread("data-raw/TRT Data_1.28.2025 at 12_00pm.csv",
                            stringsAsFactors = FALSE)


profvis({
  cleaned_data <- rsa.helpr::clean_utah(data)
})

profvis({
  cleaned_data2 <- clean_utah2(data)
})

profvis({
  cleaned_scores <- rsa.helpr::clean_scores(scores)
})

