##################
# METADATA MODEL #
##################

# model_run <- reactiveVal(FALSE)

# model_metadata <- reactive({
#   req(selected_data())
#   req(input$response)
#   data <- selected_data()
#   response <- input$response
#
#   if (response == "Predict Employment Outcome"){
#     # employ_col <- grep("(?i)^(?=.*employment)(?!.*(?i)_desc)(?!.*(?i)_wage)(?!.*(?i)un)",
#     #                    names(data), value = TRUE, perl = TRUE)
#     # employ_col <- "E389_Q4_Employment_911"
#     #
#     #       exit_work_col <- grep("(?i)_exit*(?i)_work(?!.*(?i)_amt)(?!.*(?i)_desc)",
#     #                             names(data), value = TRUE, perl = TRUE)
#
#     # if (length(exit_work_col) < 1){
#     #   return("No employment variable available.")
#     # } else{
#     y <- "Final_Employment"
#     # }
#
#
#   } else if (response == "Predict Ending Wage") {
#     wage_col <- grep("(?i)^(?=.*wage)(?=.*exit)(?!.*(desc))", names(data),
#                      value = TRUE, perl = TRUE)
#
#     if (length(wage_col) < 1){
#       return("No wage variable available.")
#     } else{
#       y <- wage_col
#     }
#   } else if (response == "Predict Median Difference Score"){
#     median_diff_col <- "Median_Difference_Score"
#
#     if (length(median_diff_col) < 1){
#       return("No median difference score variable available.")
#     } else{
#       y <- median_diff_col
#     }
#   }
#
#   predictors <- c()
#
#   if (input$gender) {
#     sex_col <- grep("((?i)_sex|(?i)_gender)(?!.*(?i)_desc)", names(data),
#                     value = TRUE, perl = TRUE)
#     if (length(sex_col) < 1){
#       return("No gender/sex variable available.")
#     } else{
#       predictors <- c(predictors, sex_col)
#     }
#   }
#
#   if (input$race) {
#     race_cols <- grep("(?i)(_indian|_asian|_black|_hawaiian|_islander|_white|hispanic)(?!.*(?i)_desc)",
#                       names(data),
#                       value = TRUE, perl = TRUE)
#     if (length(race_cols) < 1){
#       return("No race variable(s) available.")
#     } else{
#       predictors <- c(predictors, race_cols)
#     }
#   }
#
#   if (input$severity) {
#     # severity_col <- grep("((?i)_SWD|(?i)_severity)(?!.*(?i)_desc|_age)",
#     #                      names(data), value = TRUE, perl = TRUE)
#     severity_col <- grep("((?i)_priority|(?i)_severity)(?!.*(?i)_desc|_age)",
#                          names(data), value = TRUE, perl = TRUE)
#     if (length(severity_col) < 1){
#       return("No disability severity variable available.")
#     } else{
#       predictors <- c(predictors, severity_col)
#     }
#   }
#
#   if (input$enroll_length) {
#     enroll_length_col <- grep("Enroll_Length",
#                               names(data), value = TRUE, perl = TRUE)
#     if (length(enroll_length_col) < 1){
#       return("No enrollment length variable available.")
#     } else{
#       predictors <- c(predictors, enroll_length_col)
#     }
#   }
#
#   if (input$prim_impairment) {
#     # prim_dis_col <- grep("(?i)^(?=.*prim)(?=.*impairment)(?!.*(desc))",
#     #                      names(data), value = TRUE, perl = TRUE)
#     if (length("Primary_Impairment_Group") < 1){
#       return("No primary impairment variable available.")
#     } else{
#       predictors <- c(predictors, "Primary_Impairment_Group")
#     }
#   }
#
#   if (input$second_impairment) {
#     # second_dis_col <- grep("(?i)^(?=.*sec)(?=.*impairment)(?!.*(desc))",
#     #                        names(data), value = TRUE, perl = TRUE)
#     if (length("Secondary_Impairment_Group") < 1){
#       return("No secondary impairment variable available.")
#     } else{
#       predictors <- c(predictors, "Secondary_Impairment_Group")
#     }
#   }
#
#   # Check if response and predictors are selected
#   req(response, length(predictors) > 0)
#
#   formula <- as.formula(paste(y, "~",
#                               paste(predictors, collapse = "+")))
#
#   # # Create the formula with interaction terms if specified
#   # if (input$interactions) {
#   #   interaction_terms <- paste(predictors, collapse = "*")
#   #   formula <- as.formula(paste(y, "~", interaction_terms))
#   # } else {
#   #   formula <- as.formula(paste(y, "~", paste(predictors, collapse = "+")))
#   # }
#
#   if (response == "Predict Ending Wage" ||
#       response == "Predict Median Difference Score") {
#     lm(formula = formula, data = data)
#   } else if (response == "Predict Employment Outcome") {
#     glm(formula, family = binomial, data = data)
#   }
#
#   # Set model_run to TRUE when the model is executed
#   # model_run(TRUE)
#
# })



model_run <- reactiveVal(FALSE)

generate_model_metadata <- function(selected_data, input) {
  req(selected_data())
  req(input$response)

  data <- selected_data()
  response <- input$response

  # Determine response variable
  if (response == "Predict Employment Outcome") {
    y <- "Final_Employment"
  } else if (response == "Predict Ending Wage") {
    wage_col <- grep("(?i)^(?=.*wage)(?=.*exit)(?!.*(desc))", names(data), value = TRUE, perl = TRUE)
    if (length(wage_col) < 1) return("No wage variable available.")
    y <- wage_col
  } else if (response == "Predict Median Difference Score") {
    median_diff_col <- "Median_Difference_Score"
    if (length(median_diff_col) < 1) return("No median difference score variable available.")
    y <- median_diff_col
  }

  # Define predictor variables
  predictors <- c()
  if (input$gender) {
    sex_col <- grep("((?i)_sex|(?i)_gender)(?!.*(?i)_desc)", names(data), value = TRUE, perl = TRUE)
    if (length(sex_col) > 0) predictors <- c(predictors, sex_col)
  }

  if (input$race) {
    race_cols <- grep("(?i)(_indian|_asian|_black|_hawaiian|_islander|_white|hispanic)(?!.*(?i)_desc)", names(data), value = TRUE, perl = TRUE)
    if (length(race_cols) > 0) predictors <- c(predictors, race_cols)
  }

  if (input$severity) {
    severity_col <- grep("((?i)_priority|(?i)_severity)(?!.*(?i)_desc|_age)", names(data), value = TRUE, perl = TRUE)
    if (length(severity_col) > 0) predictors <- c(predictors, severity_col)
  }

  if (input$enroll_length) {
    enroll_length_col <- grep("Enroll_Length", names(data), value = TRUE, perl = TRUE)
    if (length(enroll_length_col) > 0) predictors <- c(predictors, enroll_length_col)
  }

  if (input$prim_impairment) {
    predictors <- c(predictors, "Primary_Impairment_Group")
  }

  if (input$second_impairment) {
    predictors <- c(predictors, "Secondary_Impairment_Group")
  }

  # Ensure we have a response and predictors
  req(y, length(predictors) > 0)

  formula <- as.formula(paste(y, "~", paste(predictors, collapse = "+")))

  # Fit model based on response type
  model <- if (response == "Predict Ending Wage" || response == "Predict Median Difference Score") {
    lm(formula = formula, data = data)
  } else if (response == "Predict Employment Outcome") {
    glm(formula, family = binomial, data = data)
  }

  model_run(TRUE)  # Mark model as run
  return(model)
}





metadata_residuals1 <- function() {
  renderPlot({
    response <- input$response
    req(model_metadata())  # Ensure that the model metadata exists

    # Use tryCatch to suppress warnings and check if model is valid
    model <- tryCatch({
      model_metadata()  # Attempt to get the model
    }, warning = function(w) {
      # If the warning is about 0 or 1 probabilities, we suppress it
      if (grepl("glm.fit: fitted probabilities numerically 0 or 1 occurred", w$message)) {
        return(NULL)  # Return NULL if the model fitting fails due to perfect separation
      }
      stop(w)  # Otherwise, propagate the warning
    }, error = function(e) {
      # Handle errors (e.g., model fitting failed)
      showNotification("Error in fitting model: check your data or model specification.", type = "error")
      return(NULL)  # Return NULL if model fitting fails
    })

    # If model is NULL, return nothing
    if (is.null(model)) {
      return(NULL)
    }

    # residuals
    residuals <- resid(model)

    if (response == "Predict Ending Wage" ||
        response == "Predict Median Difference Score") {
      # histogram to look for normality
      hist(residuals, col = "steelblue")
    } else {
      y <- "Final_Employment"
      # Ensure the model is logistic (family = binomial)
      # Calculate predicted probabilities
      predicted_probs <- suppressWarnings(predict(model, type = "response"))

      # Use pROC to generate an ROC curve
      library(pROC)
      roc_obj <- roc(model$y, predicted_probs)

      # Plot the ROC curve
      plot(roc_obj, col = "steelblue", lwd = 2, main = "ROC Curve")
      # Add diagonal line (no skill classifier)
      # abline(a = 0, b = 1, col = "gray", lty = 2)
    }
  })

}

metadata_residuals2 <- function() {
  renderPlot({
    response <- input$response
    req(model_metadata())

    # residuals
    model <- model_metadata()
    residuals <- resid(model)

    if (response == "Predict Ending Wage" ||
        response == "Predict Median Difference Score") {

      # histogram to look for normality
      qqnorm(residuals)
      qqline(residuals, col = "steelblue")
    } else {
      arm::binnedplot(fitted(model), residuals(model, type = "response"),
                      nclass = NULL,
                      xlab = "Expected Values",
                      ylab = "Average residual",
                      main = "Binned residual plot",
                      cex.pts = 0.8,
                      col.pts = 1,
                      col.int = "gray")
    }

  })

}

metadata_residuals3 <- function() {
  renderPlot({
    response <- input$response
    req(model_metadata())

    # Extract fitted values and residuals
    model <- model_metadata()
    residuals <- resid(model)
    fitted <- fitted(model)

    if (response == "Predict Ending Wage" ||
        response == "Predict Median Difference Score") {
      # Generate residuals vs. fitted values plot
      plot(fitted, residuals,
           main = "Residuals vs Fitted",
           xlab = "Fitted Values",
           ylab = "Residuals",
           pch = 19)

      # Add a horizontal line at y = 0 for reference
      abline(h = 0, col = "steelblue", lty = 2)
    }
  })
}



roc_explanation <- function() {
  renderUI({
    req(model_metadata())
    if (input$response == "Predict Employment Outcome") {
      tags$p(HTML("<b>ROC Curve Explanation:</b> The ROC curve plots the true positive rate (sensitivity) against the false positive rate (1-specificity) at various threshold levels. The Area Under the Curve (AUC) represents the model's ability to discriminate between positive and negative outcomes. A higher AUC indicates better model performance."))
    }
  })
}

binned_explanation <- function() {
  renderUI({
    req(model_metadata())
    if (input$response == "Predict Employment Outcome") {
      tags$p(HTML("<b>Binned Residuals Plot Explanation:</b> This plot divides the data into bins based on fitted values, showing the average residual versus the average fitted value for each bin. It helps assess how well the model fits in different ranges of the predictor variable. For a reasonable model, we hope to see the residuals scattered randomly around 0 with no discernible pattern. This indicates that the model's assumptions are valid and the errors are randomly distributed. If the residuals display a systematic trend (e.g., a curve or increasing/decreasing spread), it may suggest issues such as non-linearity, heteroscedasticity, or a missing predictor variable."))
    }
  })
}

residuals_explanation <- function() {
  renderUI({
    req(model_metadata())
    if (input$response == "Predict Ending Wage" || input$response == "Predict Median Difference Score") {
      tags$p(HTML("<b>Residuals vs Fitted Plot Explanation:</b> This plot helps assess the model fit by showing the residuals (differences between observed and predicted values) against the fitted values (predicted values). Ideally, the residuals should be randomly scattered around 0, indicating that the model's assumptions are valid."))
    }
  })
}

histogram_explanation <- function() {
  renderUI({
    req(model_metadata())
    if (input$response == "Predict Ending Wage" || input$response == "Predict Median Difference Score") {
      tags$p(HTML("<b>Histogram of Residuals Explanation:</b> This plot shows the distribution of the residuals from the model. It helps assess the normality of the residuals, which is an important assumption for linear regression. Ideally, the histogram should resemble a bell-shaped curve, indicating that the residuals are approximately normally distributed."))
    }
  })

}


qqplot_explanation <- function() {
  renderUI({
    req(model_metadata())  # Ensure model_scores() exists
    if (input$response == "Predict Ending Wage" || input$response == "Predict Median Difference Score") {
      tags$p(HTML("<b>QQ Plot Explanation:</b> The QQ plot is used to check if the residuals follow a normal distribution. Points should lie approximately on the diagonal line if the residuals are normally distributed. Deviations from the line suggest departures from normality."))
    }
  })
}


# For ANOVA model
residuals_explanation2 <- function() {
  renderUI({
    req(model_scores())  # Ensure model_scores() exists
    tags$p(HTML("<b>Residuals vs Fitted Plot Explanation:</b> This plot helps assess the model fit by showing the residuals (differences between observed and predicted values) against the fitted values (predicted values). Ideally, the residuals should be randomly scattered around 0, indicating that the model's assumptions are valid."))
  })
}

histogram_explanation2 <- function() {
  renderUI({
    req(model_scores())  # Ensure model_scores() exists
    tags$p(HTML("<b>Histogram of Residuals Explanation:</b> This plot shows the distribution of the residuals from the model. It helps assess the normality of the residuals, which is an important assumption for linear regression. Ideally, the histogram should resemble a bell-shaped curve, indicating that the residuals are approximately normally distributed."))
  })
}

qqplot_explanation2 <- function() {
  renderUI({
    req(model_scores())  # Ensure model_scores() exists
    tags$p(HTML("<b>QQ Plot Explanation:</b> The QQ plot is used to check if the residuals follow a normal distribution. Points should lie approximately on the diagonal line if the residuals are normally distributed. Deviations from the line suggest departures from normality."))
  })
}


model_metadata_summary <- function() {
  renderPrint({
    req(model_metadata())

    model <- model_metadata()

    # Check if the model is a logistic regression (glm with family binomial)
    if (inherits(model, "glm") && model$family$family == "binomial") {
      coefficients <- round(coef(summary(model)), 2)
      null_deviance <- round(model$null.deviance, 2)
      residual_deviance <- round(model$deviance, 2)
      aic <- round(AIC(model), 2)

      # Print the logistic regression summary
      cat("Logistic Regression Summary:\n")
      cat("Coefficients:\n")
      print(coefficients)
      cat("\nNull Deviance:", null_deviance)
      cat("\nResidual Deviance:", residual_deviance)
      cat("\nAIC:", aic, "\n")
    } else if (inherits(model, "lm")) {
      # Handle linear model
      coefficients <- round(coef(summary(model)), 2)
      sigma <- round(summary(model)$sigma, 2)
      r_squared <- round(summary(model)$r.squared, 2)
      adj_r_squared <- round(summary(model)$adj.r.squared, 2)
      f_stat <- round(summary(model)$fstatistic[1], 2) # F-statistic

      # Print the linear model summary
      cat("Linear Model Summary:\n")
      cat("Coefficients:\n")
      print(coefficients)
      cat("\nResidual Standard Error (sigma):", sigma)
      cat("\nR-squared:", r_squared)
      cat("\nAdjusted R-squared:", adj_r_squared)
      cat("\nF-statistic:", f_stat, "\n")
    } else {
      # Handle unsupported model types
      cat("Model type not supported for summary output.\n")
    }
  })
}


model_metadata_exists <- function() {
  reactive({
    !is.null(model_metadata())
  })

}
