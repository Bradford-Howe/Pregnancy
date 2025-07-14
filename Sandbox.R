# Load necessary packages
library(stats)  # For chi-square test
library(knitr)  # For table formatting

# Heartbeat observed data
heartbeat_csection = 100
heartbeat_non_csection = 1900
heartbeat_total = 2000
heartbeat_rate = heartbeat_csection / heartbeat_total

# U.S. expected data
us_rate = 0.321
expected_csection = us_rate * heartbeat_total
expected_non_csection = (1 - us_rate) * heartbeat_total

# Chi-square contributions
contrib_csection = ((heartbeat_csection - expected_csection)^2) / expected_csection
contrib_non_csection = ((heartbeat_non_csection - expected_non_csection)^2) / expected_non_csection

# Chi-square test
observed = matrix(c(heartbeat_csection, heartbeat_non_csection), 
                  nrow = 1, byrow = TRUE,
                  dimnames = list("Heartbeat", c("C-section", "Non-C-section")))
chi_test = chisq.test(observed, p = c(us_rate, 1 - us_rate))

# Odds calculations
heartbeat_odds = heartbeat_csection / heartbeat_non_csection
us_odds = expected_csection / expected_non_csection
odds_ratio = heartbeat_odds / us_odds
odds_reduction = (1 - odds_ratio) * 100
inverse_odds_ratio = 1 / odds_ratio

# Create data frame for statistics table (Sections 1-4, no spacers)
stats_df = data.frame(
   Group = c("Heartbeat", "Expectation per U.S. rate", "Contributions", "χ2"),
   C_section = c(heartbeat_csection, round(expected_csection, 0), round(contrib_csection, 3), round(chi_test$statistic, 3)),
   Non_C_section = c(heartbeat_non_csection, round(expected_non_csection, 0), round(contrib_non_csection, 3), format(chi_test$p.value, scientific = TRUE, digits = 4)),
   Total = c(heartbeat_total, heartbeat_total, "", ""),
   Rate = c(round(heartbeat_rate, 3), us_rate, "", "")
)

# Create data frame for odds and statements table (Section 5)
statements_df = data.frame(
   Group = c("Heartbeat Odds of C-section", "U.S. Odds of C-section", "Odds Ratio",
             "Odds Reduction", "Relative Likelihood"),
   Value = c(
      paste(round(heartbeat_odds, 4), "to 1"),
      paste(round(us_odds, 4), "to 1"),
      round(odds_ratio, 3),
      paste("Your odds of having a C-section are", round(odds_reduction, 0), "% lower at Heartbeat."),
      paste("You are about", round(inverse_odds_ratio, 0), "times more likely to have a C-section nationally.")
   )
)

# Print statistics table
print("Statistics: Comparison of C-section Outcomes: Heartbeat vs. U.S. Rate")
kable(stats_df, format = "simple", align = "lcccr", col.names = c("", "C-section", "Non-C", "Total", "rate"))

# Print statements table
print("Odds and Interpretive Statements")
kable(statements_df, format = "simple", align = "ll", col.names = c("", "Value"))

# Export both tables to CSV for Excel
write.csv(stats_df, "csection_stats.csv", row.names = FALSE)
write.csv(statements_df, "csection_statements.csv", row.names = FALSE)