library(readxl)
data <- read_excel("C:/Users/Owner/Downloads/dc-crimes-search-results.xlsx")

# Load required libraries
library(readr)  # or library(readxl) if Excel file
library(dplyr)
library(ggplot2)
library(lubridate)
library(tidyr)  # for complete() function

# Read the data (adjust path as needed)
data <- read.csv("C:/Users/Owner/Downloads/dc-crimes-search-results.csv")

# Convert date column to proper date format
# The format is M/D/YYYY, H:MM:SS AM/PM
data$datetime <- as.POSIXct(data$END_DATE, format = "%m/%d/%Y, %I:%M:%S %p")
data$date <- as.Date(data$datetime)

# Check for any parsing issues
cat("Date parsing check:\n")
cat("Successfully parsed dates:", sum(!is.na(data$date)), "\n")
cat("Failed to parse dates:", sum(is.na(data$date)), "\n")

# If there are any NA values, show what those entries look like
if(sum(is.na(data$date)) > 0) {
  cat("Problematic date entries:\n")
  print(head(data$END_DATE[is.na(data$date)], 5))
}

# Check the full date range of your data
cat("\nFull dataset date range:\n")
cat("Earliest date:", as.character(min(data$date, na.rm = TRUE)), "\n")
cat("Latest date:", as.character(max(data$date, na.rm = TRUE)), "\n")
cat("Total days covered:", as.numeric(max(data$date, na.rm = TRUE) - min(data$date, na.rm = TRUE)), "days\n\n")

# Manually filter to recent 2-year period for diff-in-diff analysis
data_recent <- data[data$date >= as.Date("2024-01-01") & data$date <= as.Date("2025-12-31") & !is.na(data$date), ]

cat("Filtered data (2024-2025) info:\n")
cat("Number of rows:", nrow(data_recent), "\n")
cat("Date range:", as.character(min(data_recent$date)), "to", as.character(max(data_recent$date)), "\n\n")

# Check unique offense types in filtered data
cat("Unique offense types in 2024-2025 data:\n")
print(sort(unique(data_recent$OFFENSE)))
cat("\nOffense counts in filtered data:\n")
print(table(data_recent$OFFENSE))

# Create daily crime counts by type (using filtered data)
daily_crimes <- data_recent %>%
  group_by(date, OFFENSE) %>%
  summarise(count = n(), .groups = 'drop')

# Add August 11, 2025 intervention line
intervention_date <- as.Date("2025-08-11")

# Plot daily time series for all crime types (with forced date limits)
p1 <- ggplot(daily_crimes, aes(x = date, y = count)) +
  geom_line(alpha = 0.7) +
  geom_vline(xintercept = intervention_date, color = "red", linetype = "dashed", linewidth = 1) +
  facet_wrap(~OFFENSE, scales = "free_y", ncol = 3) +
  labs(title = "Daily Crime Counts by Type (2024-2025)",
       subtitle = "Red line shows intervention date (August 11, 2025)",
       x = "Date",
       y = "Daily Crime Count") +
  theme_minimal() +
  scale_x_date(limits = c(as.Date("2024-01-01"), as.Date("2025-12-31")),
               date_breaks = "2 months", date_labels = "%b %Y") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
        strip.text = element_text(size = 8))

print(p1)

# Create weekly aggregated version for cleaner view (using filtered data)
weekly_crimes <- data_recent %>%
  mutate(week = floor_date(date, "week")) %>%
  group_by(week, OFFENSE) %>%
  summarise(count = n(), .groups = 'drop')

p2 <- ggplot(weekly_crimes, aes(x = week, y = count)) +
  geom_line(alpha = 0.8, linewidth = 0.8) +
  geom_vline(xintercept = intervention_date, color = "red", linetype = "dashed", linewidth = 1) +
  facet_wrap(~OFFENSE, scales = "free_y", ncol = 3) +
  labs(title = "Weekly Crime Counts by Type (2024-2025)",
       subtitle = "Red line shows intervention date (August 11, 2025)",
       x = "Date",
       y = "Weekly Crime Count") +
  theme_minimal() +
  scale_x_date(limits = c(as.Date("2024-01-01"), as.Date("2025-12-31")),
               date_breaks = "2 months", date_labels = "%b %Y") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
        strip.text = element_text(size = 8))

print(p2)

# Check data completeness around intervention date (using filtered data)
cat("\nData completeness check around intervention date:\n")
date_range <- seq(intervention_date - 30, intervention_date + 30, by = "day")
completeness <- data_recent %>%
  filter(date %in% date_range) %>%
  group_by(date) %>%
  summarise(total_crimes = n(), .groups = 'drop') %>%
  complete(date = date_range, fill = list(total_crimes = 0))

cat("Days with zero crimes reported in 30-day window around intervention:\n")
zero_days <- completeness[completeness$total_crimes == 0, ]
if(nrow(zero_days) > 0) {
  print(zero_days)
} else {
  cat("No days with zero crimes in this period.\n")
}

# Summary of most recent data (using filtered data)
cat("\nMost recent 5 days of data:\n")
recent_data <- data_recent %>%
  group_by(date) %>%
  summarise(total_crimes = n(), .groups = 'drop') %>%
  arrange(desc(date)) %>%
  head(5)
print(recent_data)

# Load required libraries
library(dplyr)
library(lmtest)
library(sandwich)
library(broom)

# Define treatment windows (2 weeks on either side)
aug_11_2024 <- as.Date("2024-08-11")
aug_11_2025 <- as.Date("2025-08-11")

# Create the analysis dataset with 4-week windows
analysis_data <- data_recent %>%
  filter(
    (date >= (aug_11_2024 - 14) & date <= (aug_11_2024 + 14)) |
      (date >= (aug_11_2025 - 14) & date <= (aug_11_2025 + 14))
  ) %>%
  mutate(
    # Year indicator (2024 = control, 2025 = treatment year)
    year = year(date),
    treatment_year = ifelse(year == 2025, 1, 0),
    
    # Post-intervention indicator (after Aug 11 in respective year)
    post_intervention = ifelse(
      (year == 2024 & date > aug_11_2024) | 
        (year == 2025 & date > aug_11_2025), 1, 0),
    
    # Diff-in-diff interaction term
    did_term = treatment_year * post_intervention,
    
    # Cluster variable (4 distinct periods)
    cluster_period = paste(year, ifelse(post_intervention == 1, "post", "pre"), sep = "_")
  )

# Check the setup
cat("Analysis dataset summary:\n")
cat("Total observations:", nrow(analysis_data), "\n")
cat("Date range:", as.character(min(analysis_data$date)), "to", as.character(max(analysis_data$date)), "\n\n")

cat("Observations by cluster period:\n")
print(table(analysis_data$cluster_period))
cat("\n")

# Create daily crime counts for regression
daily_counts <- analysis_data %>%
  group_by(date, year, treatment_year, post_intervention, did_term, cluster_period, OFFENSE) %>%
  summarise(count = n(), .groups = 'drop') %>%
  # Complete missing dates with zero counts
  complete(date, OFFENSE, 
           fill = list(count = 0)) %>%
  # Fill in the indicators for completed rows
  mutate(
    year = year(date),
    treatment_year = ifelse(year == 2025, 1, 0),
    post_intervention = ifelse(
      (year == 2024 & date > aug_11_2024) | 
        (year == 2025 & date > aug_11_2025), 1, 0),
    did_term = treatment_year * post_intervention,
    cluster_period = paste(year, ifelse(post_intervention == 1, "post", "pre"), sep = "_")
  )

# Aggregate to period level for proper standard errors
period_counts <- daily_counts %>%
  group_by(cluster_period, year, treatment_year, post_intervention, did_term, OFFENSE) %>%
  summarise(
    total_count = sum(count),
    days_in_period = n_distinct(date),
    mean_daily_count = total_count / days_in_period,
    .groups = 'drop'
  )

# Function to run diff-in-diff regression for each crime type (corrected for perfect multicollinearity)
run_did_regression <- function(crime_type) {
  # Filter data for this crime type
  crime_data <- period_counts %>% 
    filter(OFFENSE == crime_type)
  
  # Check if we have the required 4 observations
  if(nrow(crime_data) != 4) {
    return(data.frame(
      crime_type = crime_type,
      did_coefficient = NA,
      standard_error = NA,
      t_statistic = NA,
      p_value = NA,
      significant = "Insufficient data",
      pre_intervention_mean_2025 = NA,
      percent_change = NA,
      observations = nrow(crime_data)
    ))
  }
  
  # With exactly 4 observations and 4 parameters, we have perfect fit and no degrees of freedom
  # Calculate the DID coefficient manually using the standard 2x2 formula
  
  # Extract the four means
  pre_2024 <- crime_data$mean_daily_count[crime_data$year == 2024 & crime_data$post_intervention == 0]
  post_2024 <- crime_data$mean_daily_count[crime_data$year == 2024 & crime_data$post_intervention == 1]
  pre_2025 <- crime_data$mean_daily_count[crime_data$year == 2025 & crime_data$post_intervention == 0]
  post_2025 <- crime_data$mean_daily_count[crime_data$year == 2025 & crime_data$post_intervention == 1]
  
  # DID coefficient = (post_2025 - pre_2025) - (post_2024 - pre_2024)
  did_coef <- (post_2025 - pre_2025) - (post_2024 - pre_2024)
  
  # With perfect multicollinearity, we cannot calculate proper standard errors
  # We can only report the point estimate
  pct_change <- ifelse(pre_2025 > 0, (did_coef / pre_2025) * 100, NA)
  
  return(data.frame(
    crime_type = crime_type,
    did_coefficient = round(did_coef, 3),
    standard_error = NA,  # Cannot be calculated with 4 obs, 4 parameters
    t_statistic = NA,
    p_value = NA,
    significant = "Cannot test (perfect fit)",
    pre_intervention_mean_2025 = round(pre_2025, 2),
    percent_change = round(pct_change, 1),
    observations = 4,
    # Additional info for interpretation
    pre_2024 = round(pre_2024, 2),
    post_2024 = round(post_2024, 2),
    pre_2025 = round(pre_2025, 2),
    post_2025 = round(post_2025, 2),
    change_2024 = round(post_2024 - pre_2024, 2),
    change_2025 = round(post_2025 - pre_2025, 2)
  ))
}

# Run analysis for each crime type
crime_types <- unique(daily_counts$OFFENSE)
results_list <- lapply(crime_types, run_did_regression)
results_df <- do.call(rbind, results_list)

# Display results
cat("DIFFERENCE-IN-DIFFERENCES RESULTS\n")
cat("==================================\n\n")
cat("Intervention Date: August 11th\n")
cat("Analysis Window: 2 weeks before/after in 2024 vs 2025\n")
cat("Standard Errors: Robust (HC1) on period-level aggregated data\n\n")

print(results_df)

# Summary statistics
cat("\n\nSUMMARY:\n")
cat("========\n")
significant_crimes <- results_df[results_df$significant == "Yes", ]
cat("Crime types with significant effects (p < 0.05):", nrow(significant_crimes), "\n")

if(nrow(significant_crimes) > 0) {
  cat("\nSignificant results:\n")
  for(i in 1:nrow(significant_crimes)) {
    crime <- significant_crimes$crime_type[i]
    coef <- significant_crimes$did_coefficient[i]
    pct <- significant_crimes$percent_change[i]
    p_val <- significant_crimes$p_value[i]
    
    direction <- ifelse(coef < 0, "decreased", "increased")
    cat(sprintf("- %s: %s by %.1f daily incidents (%.1f%%, p=%.4f)\n", 
                crime, direction, abs(coef), abs(pct), p_val))
  }
}

# Create visualization of the diff-in-diff for significant results
if(nrow(significant_crimes) > 0) {
  library(ggplot2)
  
  # Calculate means for plotting using period-level data
  plot_data <- period_counts %>%
    filter(OFFENSE %in% significant_crimes$crime_type) %>%
    select(OFFENSE, year, post_intervention, mean_daily_count)
  
  p <- ggplot(plot_data, aes(x = factor(post_intervention, labels = c("Pre", "Post")), 
                             y = mean_daily_count, 
                             color = factor(year),
                             group = factor(year))) +
    geom_line(linewidth = 1) +
    geom_point(size = 3) +
    facet_wrap(~OFFENSE, scales = "free_y") +
    labs(title = "Difference-in-Differences: Crime Changes Around Aug 11th",
         subtitle = "Lines show mean daily crime counts 2 weeks before/after",
         x = "Period Relative to August 11th",
         y = "Mean Daily Crime Count",
         color = "Year") +
    theme_minimal()
  
  print(p)
}


# Find the most recent date in the dataset
most_recent_date <- max(data$date, na.rm = TRUE)
cat("Most recent incident date:", as.character(most_recent_date), "\n")

# Also show how many incidents occurred on that date
incidents_on_last_date <- sum(data$date == most_recent_date, na.rm = TRUE)
cat("Number of incidents on that date:", incidents_on_last_date, "\n")

# Show the date range for context
cat("Full date range:", as.character(min(data$date, na.rm = TRUE)), "to", as.character(most_recent_date), "\n")


library(ggplot2)
library(dplyr)

# Define the date ranges (2 weeks before/after Aug 11 in both years)
aug_11_2024 <- as.Date("2024-08-11")
aug_11_2025 <- as.Date("2025-08-11")

# Create the daily summary for the analysis windows
daily_summary <- data_recent %>%
  filter(
    (date >= (aug_11_2024 - 14) & date <= (aug_11_2024 + 14)) |
      (date >= (aug_11_2025 - 14) & date <= (aug_11_2025 + 14))
  ) %>%
  group_by(date) %>%
  summarise(total_incidents = n(), .groups = 'drop') %>%
  # Complete missing dates with zero counts
  complete(date = seq(min(date), max(date), by = "day"), 
           fill = list(total_incidents = 0)) %>%
  # Add year and relative day information
  mutate(
    year = year(date),
    days_from_aug11 = ifelse(year == 2024, 
                             as.numeric(date - aug_11_2024),
                             as.numeric(date - aug_11_2025)),
    year_label = paste("Year", year)
  ) %>%
  # Filter to only the analysis windows
  filter(abs(days_from_aug11) <= 14)

# Create the time series plot
p <- ggplot(daily_summary, aes(x = days_from_aug11, y = total_incidents, 
                               color = factor(year), group = factor(year))) +
  geom_line(linewidth = 0.8, alpha = 0.8) +
  geom_point(size = 2) +
  geom_vline(xintercept = 0, color = "red", linetype = "dashed", linewidth = 1) +
  labs(title = "Daily Crime Incidents: 2 Weeks Before/After August 11th",
       subtitle = "Red line shows intervention date (Day 0)",
       x = "Days Relative to August 11th",
       y = "Total Daily Incidents",
       color = "Year") +
  scale_x_continuous(breaks = seq(-14, 14, by = 2)) +
  scale_color_manual(values = c("2024" = "blue", "2025" = "orange")) +
  theme_minimal() +
  theme(
    panel.grid.minor.x = element_blank(),
    legend.position = "top"
  )

print(p)

# Print summary statistics
cat("\nSUMMARY STATISTICS:\n")
cat("===================\n")
summary_stats <- daily_summary %>%
  mutate(period = ifelse(days_from_aug11 < 0, "Pre", "Post")) %>%
  group_by(year, period) %>%
  summarise(
    days = n(),
    total_incidents = sum(total_incidents),
    mean_daily = round(mean(total_incidents), 1),
    .groups = 'drop'
  )
print(summary_stats)