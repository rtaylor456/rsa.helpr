# Create a contingency table of Final_Employment by Gender
employment_gender_table <- table(metadata$Final_Employment,
                                 metadata$E9_Gender_911)

rownames(employment_gender_table) <- c("Non-competitive Employment",
                                       "Competitive Employment")

colnames(employment_gender_table) <- c("Female",
                                       "Male",
                                       "Did not identify")


# Create a bar plot with bars broken up by gender
barplot(employment_gender_table, beside = TRUE,
        col = c("lightsteelblue", "steelblue"),
        legend = c("Non-competitive",
                   "Competitive"),
        xlab = "Gender", ylab = "Count",
        main = "Exit Employment by Gender")

# Create a bar plot with bars broken up by gender
barplot(employment_gender_table, beside = TRUE,
        col = c("lightsteelblue", "steelblue"),
        legend.text = c("Non-competitive", "Competitive"),
        args.legend = list(x = "topright", bty = "n",
                           title = "Employment Type"),
        xlab = "Gender", ylab = "Count",
        main = "Exit Employment by Gender")

