# check that account is set up
rsconnect::accounts()
# if returns as empty, need to set up account:

#####################################################
### Set up account steps:
# Log into shinyapps.io
# Go to Account (side panel) click to get drop down menu
# Go to Tokens
# Click show to get Secret, then copy and paste code
# Run in R
# Should be this (unless specific token and secret values change)
# rsconnect::setAccountInfo(name='rsa-data-dashboard',
#                           token='1BC21B9730C1D491A607DE641C85A066',
#                           secret='h4WBYUlWWDCFvpXCMzq+9irkOQoIgo/UImTi1yES')

# check that account is set up
# rsconnect::accounts()

#########################################################

# Then, set working directory in R to folder with app
setwd("C:/Users/Ruth Taylor/Desktop/rsa.helpr/R/dashboard")

# now deploy app
rsconnect::deployApp()

