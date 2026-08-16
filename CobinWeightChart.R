a = read.table("Corbin's Weight - For CSV Export.csv",sep=",",header=T)

library(tidyverse)
library(lubridate)

a <- a %>%
   mutate(
      Date = Date %>%
         # standardize separators
         str_replace_all("/", "-") %>%
         # add the year 2026 when it is missing
         str_replace("^(\\d{1,2}-\\d{1,2})$", "\\1-26") %>%
         # now parse as month-day-year
         mdy()
   )

# Assume your data frame is already called `df` and has columns:
#   Date          (character or factor with values like "04-28-26", "5-6-26", etc.)
#   Scale.reading (numeric)

# 1. Make sure Date is a proper Date object
a <- a %>%
   mutate(Date = as_date(Date))   # handles both "04-28-26" and "5-6-26"

# 2. Create every calendar day in the range
full_dates <- tibble(Date = seq(min(a$Date), max(a$Date), by = "1 day"))

# 3. Join so missing days become NA
plot_a <- full_dates %>%
   left_join(a, by = "Date")

# 4. Plot
ggplot(plot_a, aes(x = Date, y = Scale.reading)) +
   geom_line(linewidth = 1, color = "#2c7bb6", na.rm = TRUE) +
   geom_point(size = 2.5, color = "#2c7bb6", na.rm = TRUE) +
   scale_x_date(
      date_breaks = "1 day",
      date_labels = "%b %d",
      expand = expansion(mult = 0.02)
   ) +
   labs(x = NULL, y = "Scale Reading (lbs)", title = "Scale Reading") +
   theme_minimal(base_size = 12) +
   theme(axis.text.x = element_text(angle = 45, hjust = 1))

WeightPlot = recordPlot()
