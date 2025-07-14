# Load necessary packages
library(stats)  
library(knitr)  # For table formatting
library(ggplot2)
library(extrafont)
loadfonts(device = "win", quiet = F) # For chart fonts

# Heartbeat observed data
heartbeat_csection = 100
heartbeat_non_csection = 1900
heartbeat_total = 2000
heartbeat_rate = heartbeat_csection / heartbeat_total

# Expected data for U.S., Texas, and Lubbock
us_rate = 0.321
texas_rate = 0.347
lubbock_rate = 0.38
expected_csection_us = us_rate * heartbeat_total
expected_non_csection_us = (1 - us_rate) * heartbeat_total
expected_csection_texas = texas_rate * heartbeat_total
expected_non_csection_texas = (1 - texas_rate) * heartbeat_total
expected_csection_lubbock = lubbock_rate * heartbeat_total
expected_non_csection_lubbock = (1 - lubbock_rate) * heartbeat_total

# Chi-square contributions
contrib_csection_us = ((heartbeat_csection - expected_csection_us)^2) / expected_csection_us
contrib_non_csection_us = ((heartbeat_non_csection - expected_non_csection_us)^2) / expected_non_csection_us
contrib_csection_texas = ((heartbeat_csection - expected_csection_texas)^2) / expected_csection_texas
contrib_non_csection_texas = ((heartbeat_non_csection - expected_non_csection_texas)^2) / expected_non_csection_texas
contrib_csection_lubbock = ((heartbeat_csection - expected_csection_lubbock)^2) / expected_csection_lubbock
contrib_non_csection_lubbock = ((heartbeat_non_csection - expected_non_csection_lubbock)^2) / expected_non_csection_lubbock

# Chi-square tests
observed = matrix(c(heartbeat_csection, heartbeat_non_csection), 
                  nrow = 1, byrow = TRUE,
                  dimnames = list("Heartbeat", c("C-section", "Non-C-section")))
chi_test_us = chisq.test(observed, p = c(us_rate, 1 - us_rate))
chi_test_texas = chisq.test(observed, p = c(texas_rate, 1 - texas_rate))
chi_test_lubbock = chisq.test(observed, p = c(lubbock_rate, 1 - lubbock_rate))
chisq.test()


# Odds calculations
heartbeat_odds = heartbeat_csection / heartbeat_non_csection
us_odds = expected_csection_us / expected_non_csection_us
texas_odds = expected_csection_texas / expected_non_csection_texas
lubbock_odds = expected_csection_lubbock / expected_non_csection_lubbock
odds_ratio_us = heartbeat_odds / us_odds
odds_ratio_texas = heartbeat_odds / texas_odds
odds_ratio_lubbock = heartbeat_odds / lubbock_odds
odds_reduction_us = (1 - odds_ratio_us) * 100
odds_reduction_texas = (1 - odds_ratio_texas) * 100
odds_reduction_lubbock = (1 - odds_ratio_lubbock) * 100
inverse_odds_ratio_us = 1 / odds_ratio_us
inverse_odds_ratio_texas = 1 / odds_ratio_texas
inverse_odds_ratio_lubbock = 1 / odds_ratio_lubbock

# Relative Risk calculations
Lubbock_RR = lubbock_rate/heartbeat_rate
Texas_RR = texas_rate/heartbeat_rate
us_RR = us_rate/heartbeat_rate

# Create data frame for statistics table
stats_df = data.frame(
   Group = c("Heartbeat", "Expectation per U.S. rate", "Expectation per Texas rate", "Expectation per Lubbock rate",
             "Contributions (vs. U.S.)", "Contributions (vs. Texas)", "Contributions (vs. Lubbock)", 
             "χ2 (vs. U.S.)", "χ2 (vs. Texas)", "χ2 (vs. Lubbock)"),
   C_section = c(heartbeat_csection, round(expected_csection_us, 0), round(expected_csection_texas, 0), 
                 round(expected_csection_lubbock, 0), round(contrib_csection_us, 3), 
                 round(contrib_csection_texas, 3), round(contrib_csection_lubbock, 3), 
                 round(chi_test_us$statistic, 3), round(chi_test_texas$statistic, 3), 
                 round(chi_test_lubbock$statistic, 3)),
   Non_C_section = c(heartbeat_non_csection, round(expected_non_csection_us, 0), 
                     round(expected_non_csection_texas, 0), round(expected_non_csection_lubbock, 0), 
                     round(contrib_non_csection_us, 3), round(contrib_non_csection_texas, 3), 
                     round(contrib_non_csection_lubbock, 3), 
                     format(chi_test_us$p.value, scientific = TRUE, digits = 4),
                     format(chi_test_texas$p.value, scientific = TRUE, digits = 4),
                     format(chi_test_lubbock$p.value, scientific = TRUE, digits = 4)),
   Total = c(heartbeat_total, heartbeat_total, heartbeat_total, heartbeat_total, "", "", "", "", "", ""),
   Rate = c(round(heartbeat_rate, 3), us_rate, texas_rate, lubbock_rate, "", "", "", "", "", "")
)

# Create data frame for statements table
statements_df = data.frame(
   Group = c("Heartbeat Odds of C-section", "U.S. Odds of C-section", "Texas Odds of C-section", 
             "Lubbock Odds of C-section", "Odds Ratio (vs. U.S.)", "Odds Ratio (vs. Texas)", 
             "Odds Ratio (vs. Lubbock)", "Odds Reduction (vs. U.S.)", "Odds Reduction (vs. Texas)", 
             "Odds Reduction (vs. Lubbock)", "Relative Likelihood (vs. U.S.)", 
             "Relative Likelihood (vs. Texas)", "Relative Likelihood (vs. Lubbock)"),
   Value = c(
      paste(round(heartbeat_odds, 4), "to 1"),
      paste(round(us_odds, 4), "to 1"),
      paste(round(texas_odds, 4), "to 1"),
      paste(round(lubbock_odds, 4), "to 1"),
      round(odds_ratio_us, 3),
      round(odds_ratio_texas, 3),
      round(odds_ratio_lubbock, 3),
      paste("Your odds of having a C-section are", round(odds_reduction_us, 0), "% lower at Heartbeat than U.S."),
      paste("Your odds of having a C-section are", round(odds_reduction_texas, 0), "% lower at Heartbeat than Texas."),
      paste("Your odds of having a C-section are", round(odds_reduction_lubbock, 0), "% lower at Heartbeat than Lubbock."),
      paste("You are about", round(inverse_odds_ratio_us, 0), "times more likely to have a C-section elsewhere in the U.S."),
      paste("You are about", round(inverse_odds_ratio_texas, 0), "times more likely to have a C-section elsewhere in Texas."),
      paste("You are about", round(inverse_odds_ratio_lubbock, 0), "times more likely to have a C-section elsewhere in Lubbock.")
   )
)

# Print statistics table
print("Statistics: Comparison of C-section Outcomes: Heartbeat vs. U.S., Texas, Lubbock")
kable(stats_df, format = "simple", align = "lcccr", col.names = c("", "C-section", "Non-C", "Total", "rate"))

# Print statements table
print("Odds and Interpretive Statements")
kable(statements_df, format = "simple", align = "ll", col.names = c("", "Value"))

# Export both tables to CSV for Excel
write.csv(stats_df, "csection_stats.csv", row.names = FALSE)
write.csv(statements_df, "csection_statements.csv", row.names = FALSE)

# Load necessary package
library(ggplot2)

# Prepare data for the plot
rates_data = data.frame(
   Region = c("Heartbeat", "U.S.", "Texas", "Lubbock"),
   Rate = c(heartbeat_rate, us_rate, texas_rate, lubbock_rate)*100
)

# Create bar plot with ggplot2
CSecPlot = ggplot(data = rates_data,
                  aes(x = factor(Region, levels = c("Heartbeat", "Lubbock", "Texas", "U.S.")),
                      y = Rate,
                      fill = Region)) +
   geom_bar(stat = "identity") +
   scale_fill_manual(values = c("Heartbeat" = "#f1e9e6", "U.S." = "#FF6384", 
                                "Texas" = "#4BC0C0", "Lubbock" = "#c4d8be")) +
   labs(title = "Comparison of C-section Rates\nHeartbeat vs. U.S., Texas, Lubbock",
        x = "Region", y = "C-section Rate (%)") +
   geom_text(aes(label = c("1x", paste0(round(Lubbock_RR), "x"), 
                           paste0(round(Texas_RR), "x"), 
                           paste0(round(us_RR), "x"))), 
             vjust = 1, size = 8, family = "Garamond", fontface = "bold") +
   theme_minimal(base_size = 20) +
   theme(
      legend.position = "none",
      legend.text = element_text(family = "Garamond", face = "bold"),
      plot.title = element_text(hjust = 0.5, size = 25, family = "Garamond", face = "bold"),
      axis.text.x = element_text(angle = 0, size = 20, family = "Garamond", face = "bold"),
      axis.text.y = element_text(size = 20, family = "Garamond", face = "bold"),
      axis.title.x = element_text(size = 20, family = "Garamond", face = "bold"),
      axis.title.y = element_text(size = 20, family = "Garamond", face = "bold"),
      panel.grid = element_blank(),
      axis.line.x = element_line(color = "black"),
      axis.line.y = element_line(color = "black")
   ) +
   scale_y_continuous(limits = c(0, 50), breaks = seq(0, 50, 10), expand = c(0, 0))
CSecPlot

# Save the plot

ggsave("csection_rates.png", plot = CSecPlot, width = 8, height = 6, dpi = 300)
ggsave("csection_rates.jpg", plot = CSecPlot, width = 8, height = 6, dpi = 300)
