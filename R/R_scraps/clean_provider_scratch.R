####### Scratchwork

filtered_data <- data[Provider == "Utah State University"]
boxplot(Difference ~ Service, data = filtered_data)

filtered_data2 <- data[Provider == "Utah State University" & Pre_Post == "pre"]
boxplot(Score ~ Service, data = filtered_data2)

# library(ggplot2)
#
# # Create a boxplot comparing difference scores per service
# ggplot(filtered_data, aes(x = Service, y = difference_score_1)) +
#   geom_boxplot() +
#   labs(title = "Boxplot of Difference Scores per Service at Utah State University",
#        x = "Service",
#        y = "Difference Score") +
#   theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels if needed



# disregard the score observations that are missing factor values when doing
#   analysis

anova_result <- aov(Score ~ Provider + `Pre/Post` + Caseload,
                    data = scores_NEW_filter)
summary(anova_result)

# Example: Boxplot comparing Pre/Post scores by provider
library(ggplot2)
ggplot(scores_NEW_filter, aes(x = `Pre/Post`, y = Score, fill = Provider)) +
  geom_boxplot() +
  facet_wrap(~Caseload) +
  labs(title = "Comparison of Pre/Post Scores by Provider and Caseload")

