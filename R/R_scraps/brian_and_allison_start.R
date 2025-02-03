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

################################################################################

########
# LOAD #
########

# If you want to take a look at the function documentations
library(rsa.helpr)
?clean_utah
?clean_scores
# etc.

################################################################################

############################################################################
# LOAD DATA FROM BOX DESKTOP. (Alternatively, read pre-downloaded data in) #
############################################################################

#######################
# Quarterly data load #
#######################

# If you want to keep the quarterly data in Box, run these lines of code every
#   session:
## Access quarterly data, compile, and download full dataset.

# store the directory where quarterly data can be found
directory <- c("/Users/RuthTaylor/Library/CloudStorage/Box-Box/911 Data and Related Projects/911 Data/Utah Quarterly Data/USU Data Request")

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


#################
# TRT data load #
#################
## Access new scores data
# This directory can be a Box directory or other--it's just a simple fread()
directory <- c("/Users/RuthTaylor/Library/CloudStorage/Box-Box/911 Data and Related Projects/911 Data/TRT Data_1.28.2025 at 12:00pm.csv")
scores <- data.table::fread(directory, stringsAsFactors = FALSE)


################################################################################

#########
# CLEAN #
#########

########################
# Quarterly data clean #
########################

# Ideally, rerun these following lines of code every session--this will ensure
#   that all of the variables are stored as the correct types for analysis.

quarterly_clean <- suppressWarnings(rsa.helpr::clean_utah(quarterly))


## FYI:
# An example of cleaning some variables with special characters, if that is of
#. interest to you. (Cleaning all 37 is a very slow process, so they have to be
#  specified)
quarterly_clean <- suppressWarnings(rsa.helpr::clean_utah(quarterly,
                            clean_specials = c("E394_App_Public_Support_911",
                                               "E395_App_Medical_911",
                                               "E396_Exit_Public_Support_911")))


##################
# TRT data clean #
##################
scores_clean <- rsa.helpr::clean_scores(scores, state_filter = "Utah")


######################
# Provider data load #
######################

provider_data <- rsa.helpr::clean_provider(scores, state_filter = "Utah",
                                           condense = TRUE)
      # condense = TRUE results in one row per unique combination of Provider,
      #   Service, and Pre_Post


################################################################################


#########
# MERGE #
#########

# Merge the cleaned quarterly and TRT scores data based on matching participant
#   IDs.
merged_data <- rsa.helpr::merge_scores(quarterly_clean, scores_clean)


################################################################################


###################
# CREATE METADATA #
###################

# Condense the merged data to one row per participant--this allows for more
#  clear modeling and visualization.

metadata <- rsa.helpr::create_metadata(merged_data)


