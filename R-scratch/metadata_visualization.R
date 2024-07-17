data <- readRDS("data-raw/data_merged.rds")

# apply cleaning function
data_clean <- utah_clean(data)


data_aggregate <- data_clean |>
  # select(-X) |> # only need this when using stored csv file
  # remove the rows that don't contain an application date --these are pretty
  #   empty anyway
  filter(!is.na(E7_Application_Date_911)) |>
  group_by(Participant_ID, E1_Year_911, E2_Quarter_911) |>
  mutate(occurrences_per_quarter = n()) |>
  arrange(E7_Application_Date_911) |>
  slice(1) |>
  # for some reason, this stopped working...
  # slice(which.max(E7_Application_Date_911)) |>
  ungroup()


# read in scores dataset
scores_clean <- read.csv("data-raw/scores_clean.csv")

data_merged <- merge(data_aggregate, scores_clean,
                     by.x = "Participant_ID",
                     by.y = "Participant.ID",
                     all = FALSE) # all = TRUE adds all rows

# create a function to get the most common values (modes) for my factor
#   variables
get_mode <- function(x) {
  unique_x <- unique(x)
  unique_x[which.max(tabulate(match(x, unique_x)))]
}


metadata <- data_merged |>
  mutate(Provider = as.factor(Provider)) |>
  group_by(Participant_ID) |>
  # create enrollment length variable
  mutate(Overall_Quarter = ((E1_Year_911 - 2020) * 4 + E2_Quarter_911)) |>
  mutate(Min_Overall_Quarter = min(Overall_Quarter),
         Max_Overall_Quarter = max(Overall_Quarter)) |>
  mutate(Enroll_Length = Max_Overall_Quarter -
           Min_Overall_Quarter + 1) |>
  # create variables that count the number of years and quarters and then remove
  #   the year and quarter columns so we can condense the data
  arrange(Participant_ID, E1_Year_911, E2_Quarter_911) |>
  mutate(
    Total_Years = n_distinct(E1_Year_911),
    Total_Quarters = n_distinct(E2_Quarter_911)
  ) |>
  select(-c(E1_Year_911, E2_Quarter_911)) |>
  # convert the rest of numeric variables to medians
  mutate(across(where(is.numeric), ~ median(., na.rm = TRUE))) |>

  # handle date variables
  mutate(across(where(lubridate::is.Date),
                ~ as.Date(ifelse(all(is.na(unique(.))), NA,
                                 max(., na.rm = TRUE)))
  )) |>
  # handle factor variables--keep only the most common values for each
  #   participant
  mutate(across(where(is.factor), ~ as.factor(get_mode(.)))) |>
  # calculate
  rowwise() |>
  mutate(
    Differences_Available = sum(!is.na(c_across(starts_with("Difference_") ))),
    Median_Difference_Score = median(c_across(starts_with("Difference_")),
                                     na.rm = TRUE)
  ) |>
  ungroup() |>

  # Summarise to condense rows, keeping one row per participant
  group_by(Participant_ID) |>
  summarise(across(everything(), first)) |>
  ungroup()





library(tidyverse)

differences <- metadata |>
  select(contains("difference"))

differences |>
  lapply(function(x) sum(!is.na(x)))

# get the counts of scores for each test
metadata |>
  select(contains("pre_") | contains("post_")) |>
  lapply(function(x) sum(!is.na(x)))

differences |>
  select(contains("difference")) |>
  lapply(function(x) sum(!is.na(x)))

for (col in colnames(differences)){
  hist(differences[[col]], main = col, xlab = col)
}



hist(metadata$Median_Difference_Score)
# some are 0s, because some participants have only taken a pre or post score :(
sort(unique(metadata$Differences_Available))
barplot(table(metadata$Differences_Available),
        main = "Difference Scores Available per Participant",
        xlab = "Number of Difference Scores per Participant")

length(metadata$Differences_Available[metadata$Differences_Available == 0])
# 103
sum(!is.na(metadata$Median_Difference_Score))
# 226

metadata_clean <- metadata |>
  filter(!is.na(metadata$Median_Difference_Score))
# So, we have 226 observations to work with


# Remove columns that contain strictly NA values
metadata_clean <- metadata_clean |>
  select(where(~ !all(is.na(.)))) |>
  separate_disability()





boxplot(Median_Difference_Score ~ E9_Gender_911,
        data = metadata_clean)

boxplot(Median_Difference_Score ~ E22_SWD_911,
        data = metadata_clean)

boxplot(Median_Difference_Score ~ E67_Plan_Low_Income_911,
        data = metadata_clean)

boxplot(Median_Difference_Score ~ E68_Plan_English_Learner_911,
        data = metadata_clean)

boxplot(Median_Difference_Score ~ E64_Plan_Foster_Care_911,
        data = metadata_clean)

boxplot(Median_Difference_Score ~ E45_Disability_Priority_911,
        data = metadata_clean)

# we have no veterans in this dataset
# boxplot(Median_Difference_Score ~ E16_Veteran_Status_911,
#         data = metadata_clean)


overall_median <- median(metadata_clean$Median_Difference_Score)
overall_median # 14.29

boxplot(Median_Difference_Score ~ Provider,
        data = metadata_clean,
        main = "Median Difference Scores Across Providers",
        ylab = "Median Difference Score")
abline(h = overall_median, lty = 2, col = "blue")



# Add custom x-axis labels
axis(1, at = 1:length(unique(metadata_clean$Provider)),
     labels = unique(metadata_clean$Provider), las=2, cex.axis=0.7)

# hispanic/latino and white are the only two highly populated races

boxplot(Median_Difference_Score ~ E14_White_911,
        data = metadata_clean)
boxplot(Median_Difference_Score ~ E15_Hispanic_Latino_911,
        data = metadata_clean)




library(caret)

set.seed(123)

# Create the train and test split
trainIndex <- createDataPartition(metadata_clean$Median_Difference_Score,
                                  p = 0.8, list = FALSE)

# Split the data into training and testing sets
train <- metadata_clean[trainIndex, ]
dim(train) # 183 rows left (349 variables)

test <- metadata_clean[-trainIndex, ]




# function to find the variables with one unique value
single_unique <- function(data) {
  # Apply the function to get the number of unique values for each column
  unique_counts <- lapply(data, function(x) length(unique(x)))

  # Identify columns with exactly one unique value
  single_unique_vars <- names(data)[unlist(unique_counts) < 2]

  # Return the names of these columns
  return(single_unique_vars)
}

# get the vector of variable names with no unique values
single_vars <- single_unique(train)


# pre and post variables
pre_vars <- train |>
  select(contains("Pre_")) |>
  names()

post_vars <- train |>
  select(contains("Post_")) |>
  names()

# difference variables
difference_vars <- train |>
  select(contains("Difference_") & -contains("Median")) |>
  names()


use_data <- train |>
  # remove variables I don't want to try for prediction
  select(-c(Participant_ID,
            Overall_Quarter,
            Min_Overall_Quarter,
            Max_Overall_Quarter),
         -any_of(c(single_vars, pre_vars, post_vars, difference_vars))
  )

# Check remaining variables with only one level
remaining_single_level_vars <- use_data |>
  select(where(is.factor)) |>
  sapply(function(x) length(unique(x)))

# Print out any remaining single-level variables
remaining_single_level_vars[remaining_single_level_vars < 2]

# Remove any remaining single-level factor variables
use_data <- use_data |>
  select(-any_of(names(remaining_single_level_vars[remaining_single_level_vars < 2])))


numeric_data <- use_data |>
  select(where(is.numeric)) |>
  select(-Median_Difference_Score)

# Calculate the proportion of NA values for each column
na_proportion <- sapply(numeric_data,
                        function(x) sum(is.na(x)) / nrow(use_data))

# Filter columns where the proportion of NAs is less than 0.2
selected_numerics <- names(na_proportion[na_proportion < 0.2])

selected_numerics


factor_data <- use_data |>
  select(where(is.factor))

# Calculate the proportion of 0s for each factor column
proportion_zeros <- sapply(factor_data, function(x) {
  sum(x == "0", na.rm = TRUE) / length(x)
})

# Filter columns where the proportion of 0s is less than 0.8
selected_factors <- names(proportion_zeros[proportion_zeros < 0.8])

selected_factors




lm1 <- lm(Median_Difference_Score ~ E9_Gender_911 + E10_Indian_Alaskan_911 +
            E11_Asian_911 + E12_Black_African_911 +
            E13_Hawaiian_Pacific_Islander_911 + E14_White_911 +
            E15_Hispanic_Latino_911 + E22_SWD_911 +
            E52_Plan_Hourly_Wage_911 + E53_Plan_Weekly_Hours_Worked_911 +
            E43_Primary_Impairment_911 + Provider,
          data = train)
summary(lm1)



columns_to_exclude <- c("E18_Postal_Code_911",
                        "E19_FIPS_Code_911",
                        "E20_ZIP_Code_911",
                        "E51_Plan_Occupation_911",
                        "E74_SWD_Age_911",
                        "E142_FourYear_Comp_911",
                        "E156_Vocational_Comp_911",
                        "E176_Basic_Comp_911",
                        "E183_JobReadiness_Comp_911",
                        "E190_Disability_Comp_911",
                        "E218_Assessment_Comp_911",
                        "E225_Diagnosis_Comp_911",
                        "E232_Counseling_Comp_911",
                        "E239_JobSearch_Comp_911",
                        "E246_JobPlacement_Comp_911",
                        "E357_Exit_Occupation_911",
                        "E394_App_Public_Support_911",
                        "E395_App_Medical_911",
                        "E396_Exit_Public_Support_911",
                        "E397_Exit_Medical_911",
                        "E338_Other_Vendor_911",
                        "E296_Maintenance_Vendor_911",
                        "E296_Maintenance_Vendor_911",
                        "E256_OnJobSE_Vendor_911",
                        "E186_Disability_Vendor_911",
                        "E172_Basic_Vendor_911",
                        "E61_YouthBuild_911",
                        "E45_Disability_Priority_911",
                        "E58_Voc_Rehab_911",
                        "E77_Plan_Grade_Level_911",
                        "E105_WorkLearning_Vendor_911",
                        "E355_Exit_Reason_911")

selected_factors2 <- setdiff(selected_factors, columns_to_exclude)

try_factors <- c("E9_Gender_911",
                 "E14_White_911",
                 "E21_Referral_Source_911",
                 "E22_SWD_911",
                 "E42_Has_Disability_911",
                 "E50_Plan_Work_Status_911",
                 "E62_Plan_Long_Term_Unemployment_911",
                 "E67_Plan_Low_Income_911",
                 "E69_Plan_Skills_Deficient_911",
                 "Provider")

formula_factor <- as.formula(paste("Median_Difference_Score ~",
                                   paste(try_factors, collapse = " + ")))
lm_factors <- lm(formula_factor, data = use_data)
summary(lm_factors)



# Construct the formula for lm
formula_numeric <- as.formula(paste("Median_Difference_Score ~",
                                    paste(selected_numerics, collapse = " + ")))
# Fit the linear model
lm_numerics <- lm(formula_numeric, data = use_data)
summary(lm_numerics)


columns_to_exclude <- c("E52_Plan_Compensation_Amt")


selected_numerics2 <- setdiff(selected_numerics, columns_to_exclude)

# Construct the formula for lm
formula2 <- as.formula(paste("Median_Difference_Score ~",
                             paste(selected_numerics2, collapse = " + ")))
# Fit the linear model
lm_numerics2 <- lm(formula2, data = use_data)
summary(lm_numerics2)


## FULL linear model

all_variables <- c(try_factors, selected_numerics2)

# Construct the formula for lm
formula_all <- as.formula(paste("Median_Difference_Score ~",
                                paste(all_variables, collapse = " + ")))
# Fit the linear model
lm_all <- lm(formula_all, data = use_data)
summary(lm_all)



# predict using the linear model
lm_predictions_all <- predict(lm_all, newdata = use_data)
# calculate the MSE
mse_lm_all <- mean((use_data$Median_Difference_Score - lm_predictions_all)^2)
mse_lm_all




library(randomForest)
library(caret)

# Construct the formula for lm
formula_rf <- as.formula(paste("Median_Difference_Score ~",
                               paste(all_variables, collapse = " + ")))

# rf_model <- randomForest(Median_Difference_Score ~ E9_Gender_911 +
#                            E394_App_SSDI_Amt +
#                            Age_at_Application +
#                            Enroll_Length, data = use_data,
#                          ntree = 100,
#                          mtry = 2,
#                          importance = TRUE)

rf<- randomForest(formula_rf, data = use_data,
                  ntree = 100,
                  mtry = 2,
                  importance = TRUE)


randomForest::varImpPlot(rf,
                         sort=FALSE,
                         main="Variable Importance Plot")

# importance_plot <- caret::varImp(rf, scale = FALSE)
# plot(importance_plot)


# predict using the random forest model
rf_predictions <- predict(rf, newdata = use_data)
# calculate MSE
mse_rf <- mean((use_data$Median_Difference_Score - rf_predictions)^2)
mse_rf




library(glmnet)

y <- use_data$Median_Difference_Score

X <- use_data |>
  select(any_of(all_variables))

# Fit Lasso regression using glmnet
lasso_model <- glmnet(x = as.matrix(X), y = y, alpha = 1)  # alpha = 1 for Lasso

# Print the Lasso model object
print(lasso_model)

# Plot the Lasso coefficient path
plot(lasso_model, xvar = "lambda", label = TRUE)

# Cross-validation to select the best lambda (optional but recommended)
cv_model <- cv.glmnet(x = as.matrix(X), y = y, alpha = 1)
plot(cv_model)

# Get the best lambda value chosen by cross-validation
best_lambda <- cv_model$lambda.min
cat("Best lambda selected by cross-validation:", best_lambda, "\n")

# Extract coefficients for the best lambda
coef_lasso <- coef(lasso_model, s = best_lambda)
print(coef_lasso)

predictions <- predict(lasso_model, newx = X, s = "lambda.min")

# Calculate MSE (mean squared error)
mse <- mean((predictions - y)^2)
cat("Mean Squared Error (MSE):", mse, "\n")




library(lme4)

mixed_model_vars <- setdiff(all_variables, "Provider")

formula_all <- as.formula(paste("Median_Difference_Score ~",
                                paste(mixed_model_vars, collapse = " + "), ))

model <- lmer(Median_Difference_Score ~ E9_Gender_911 +
                E53_Plan_Weekly_Hours_Worked_911 + (1 | Provider),
              data = use_data)
summary(model)




# Print the MSEs
cat("MSE for Full Linear Model: ", mse_lm_all, "\n")
cat("MSE for Random Forest Model: ", mse_rf, "\n")
# cat("MSE for Mixed Model: ", mse_mixed)


plot(use_data$Median_Difference_Score, rf_predictions,
     main = "RF: True vs. Predicted Score ",
     xlab = "True Score",
     ylab = "Predicted Score")
abline(0, 1, col = "blue")


plot(use_data$Median_Difference_Score, lm_predictions_all,
     main = "OLS: True vs. Predicted Score ",
     xlab = "True Score",
     ylab = "Predicted Score")
abline(0, 1, col = "blue")

