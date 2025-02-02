################
# INSTALLATION #
################

## First, install rsa.helpr package

# install devtools and data.table packages first if not installed already.
install.packages(devtools)
install.packages(data.table)

# now, we can install my public package, accessed through Github
devtools::install_github("rtaylor456/rsa.helpr")

########
# LOAD #
########

# You can load rsa.helpr package and take a look at the functions
library(rsa.helpr)
?clean_utah
?clean_scores
# etc.


############################################################################
# LOAD DATA FROM BOX DESKTOP. (Alternatively, read pre-downloaded data in) #
############################################################################

## Access quarterly data, compile, and download full dataset.
directory <- c("your directory to Box desktop here/Box/911 Data and Related Projects/911 Data/Utah Quarterly Data/USU Data Request")

# use our function to grab all the correct data files and combine them uniformly
# (This might take a minute to run, but you'll see its progress :) )

quarterly <- rsa.helpr::load_data(directory)

# This will load the data in your session AND download the compiled data to your
#   working directory, check with getwd()
quarterly <- rsa.helpr::load_data(directory, download_csv = TRUE)


## Access new scores data
directory <- c("your directory to Box desktop here/Box/911 Data and Related Projects/911 Data/TRT Data_1.28.2025 at 12:00pm.csv")
scores <- data.table::fread(directory, stringsAsFactors = FALSE)



##########
# CLEAN! #
##########

quarterly_clean <- rsa.helpr::clean_utah(quarterly)

scores_clean <- rsa.helpr::clean_scores(scores)

provider_data <- rsa.helpr::clean_provider(scores)


#########
# MERGE #
#########

merged_data <- rsa.helpr::merge_scores(quarterly_clean, scores_clean)


###################
# CREATE METADATA #
###################

metadata <- rsa.helpr::create_metadata(merged_data)


