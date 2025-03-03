## CONTINGENCY TABLE
# Create the contingency table
contingency_table <- table(metadata$E84_PostSecondary_Enrollment_911,
                           metadata$E109_Enrollment_Provided_911)
colnames(contingency_table) <- c("Couns. NOT provided", "Couns. provided")
rownames(contingency_table) <- c("None", "Not for credit", "Technical train.",
                                 "Post-second. ed.")
contingency_table


## WITH TOTALS
# Add row and column totals
contingency_table_with_totals <- addmargins(contingency_table)

# Rename the total rows and columns for clarity
rownames(contingency_table_with_totals)[nrow(contingency_table_with_totals)] <- "Total"
colnames(contingency_table_with_totals)[ncol(contingency_table_with_totals)] <- "Total"

contingency_table_with_totals

# 0.03726708 --> just 4% of metadata participants are receiving post-second.
#   enrollment counseling service.

## PROPORTIONS
# Convert the contingency table to proportions
contingency_proportions <- round(prop.table(contingency_table,
                                      margin = 2), 3) # Proportions by column (counseling: 0 vs 1)

# Print the proportions
contingency_proportions


# Convert the contingency table to proportions
contingency_proportions2 <- round(prop.table(contingency_table,
                                            margin = 1), 3) # Proportions by row (counseling: 0 vs 1)

# Print the proportions
contingency_proportions2
