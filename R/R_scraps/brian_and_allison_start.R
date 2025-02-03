################
# INSTALLATION #
################

## First, install rsa.helpr package

# Install devtools package first if not installed already.
install.packages(devtools)

# Install data.table--the package on which rsa.helpr relies
# (when trying to install rsa.helpr, it should automatically prompt you to
#   install these packages if not already installed)
install.packages(data.table)
install.packages(readxl)

# now, we can install my public rsa.helpr package, accessed through Github
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

# If you want to keep the quarterly data in Box, run these lines of code every
#   session:

## Access quarterly data, compile, and download full dataset.
directory <- c("your directory to Box desktop here/Box/911 Data and Related Projects/911 Data/Utah Quarterly Data/USU Data Request")

# use our function to grab all the correct data files and combine them uniformly
# (This might take a minute to run, but you'll see its progress :) )

quarterly <- rsa.helpr::load_data(directory)

# Alternatively,

# This will load the data in your session AND download the compiled data to your
#   working directory, check with getwd(). This is helpful if you aren't worried
#   about keeping the datasets on Box.
quarterly <- rsa.helpr::load_data(directory, download_csv = TRUE)

# Then, to load the already-compiled data into a new session, run this code:
quarterly <- data.table::fread("file name", stringsAsFactors = FALSE)


## Access new scores data
# This directory can be a Box directory or other--it's jsut a simple fread()
directory <- c("your directory to Box desktop here/Box/911 Data and Related Projects/911 Data/TRT Data_1.28.2025 at 12:00pm.csv")
scores <- data.table::fread(directory, stringsAsFactors = FALSE)



##########
# CLEAN! #
##########

# Ideally, rerun these following lines of code every session--this will ensure
#   that all of the variables are stored as the correct types for analysis.

quarterly_clean <- suppressWarnings(rsa.helpr::clean_utah(quarterly))

scores_clean <- rsa.helpr::clean_scores(scores, state_filter = "Utah")

provider_data <- rsa.helpr::clean_provider(scores)


#########
# MERGE #
#########
# Merge the quarterly and TRT scores data based on matching participant IDs.
merged_data <- rsa.helpr::merge_scores(quarterly_clean, scores_clean)


###################
# CREATE METADATA #
###################
# Condense the merged data to one row per participant--this allows for more
#  clear modeling and visualization.

metadata <- rsa.helpr::create_metadata(merged_data)


