# Use the file.choose() function which opens a file browser dialog
excel_path <- file.choose()  # This will open a file browser window
tariff_exemptions <- read_excel(excel_path)
# View the first few rows to confirm it loaded correctly
head(tariff_exemptions)

# Get a summary of the data structure
str(tariff_exemptions)

# Install required packages if you haven't already
if (!requireNamespace("readxl", quietly = TRUE)) {
  install.packages("readxl")
}

# This will open a file browser dialog
file_path <- file.choose()

# Then read the chosen file
library(data.table)
stock_tariff_data <- fread(file_path)

# If you want to see the first few rows to verify it loaded correctly
head(stock_tariff_data)

# To see all the data (though be careful if it's very large)
View(stock_tariff_data)

install.packages("stringr")
install.packages("stringdist")

# Before the matching loop, get unique company names
unique_companies <- tariff_exemptions %>%
  select(org_name) %>%
  distinct()

# Then match on unique companies instead of all entries
for (i in 1:nrow(unique_companies)) {
  tariff_name <- standardize_name(unique_companies$org_name[i])
  


#here we match names

# Load necessary libraries
library(dplyr)
library(stringr)
library(stringdist)

# Function to standardize company names
standardize_name <- function(name) {
  # Convert to uppercase
  name <- toupper(name)
  
  # Remove common legal entity suffixes
  name <- str_replace_all(name, " INC$|, INC$|\\.INC$|INC\\.$", "")
  name <- str_replace_all(name, " CORP$|, CORP$|\\.CORP$|CORP\\.$", "")
  name <- str_replace_all(name, " LLC$|, LLC$|\\.LLC$|LLC\\.$", "")
  name <- str_replace_all(name, " LTD$|, LTD$|\\.LTD$|LTD\\.$", "")
  name <- str_replace_all(name, " CO$|, CO$|\\.CO$|CO\\.$", "")
  
  # Remove punctuation and extra spaces
  name <- str_replace_all(name, "[[:punct:]]", " ")
  name <- str_replace_all(name, "\\s+", " ")
  name <- str_trim(name)
  
  return(name)
}

# Apply standardization to both datasets
stock_tariff_data <- stock_tariff_data %>%
  mutate(standardized_name = standardize_name(COMNAM))

tariff_exemptions <- tariff_exemptions %>%
  mutate(standardized_name = standardize_name(org_name))

# Create a function to find potential matches
find_matches <- function(tariff_name, crsp_names, threshold = 0.2) {
  # Calculate string distances
  distances <- stringdist(tariff_name, crsp_names, method = "jw")
  
  # Normalize distances (Jaro-Winkler returns similarity, so 0 is perfect match)
  normalized_distances <- 1 - distances
  
  # Find potential matches above threshold
  matches <- which(normalized_distances >= threshold)
  
  if (length(matches) > 0) {
    return(data.frame(
      crsp_index = matches,
      similarity = normalized_distances[matches]
    ))
  } else {
    return(NULL)
  }
}
if (nrow(matched_crsp) > 0) {
  linked_row <- data.frame(
    tariff_company = unique_companies$org_name[i],
    crsp_permco = matched_crsp$permco,
    crsp_name = stock_tariff_data$COMNAM[stock_tariff_data$permco == matched_crsp$permco][1],
    similarity_score = best_match$similarity
  )
  
  linked_tariff_data <- rbind(linked_tariff_data, linked_row)
}
cat("Processing company:", unique_companies$org_name[i], "\n")
cat("Standardized name:", tariff_name, "\n")
cat("Number of potential matches:", ifelse(!is.null(matches), nrow(matches), 0), "\n")
if (!is.null(matches) && nrow(matches) > 0) {
  cat("Best match similarity:", best_match$similarity, "\n")
}

if (!is.null(matches) && nrow(matches) > 0) {
  # Get the top match
  best_match <- matches %>% 
    arrange(desc(similarity)) %>%
    slice(1)
  
  # Debug - print the index of the best match
  cat("Best match index:", best_match$crsp_index, "\n")
  
  # Get the corresponding row from unique_crsp
  matched_crsp <- unique_crsp[best_match$crsp_index, ]
  
  # Debug - verify matched_crsp has data
  cat("Matched CRSP permco:", matched_crsp$permco, "\n")
  
  # Check if there are any rows in stock_tariff_data with this permco
  matching_rows <- sum(stock_tariff_data$permco == matched_crsp$permco)
  cat("Number of matching rows in stock_tariff_data:", matching_rows, "\n")
  
  # Only try to create linked_row if we have valid data
  if (nrow(matched_crsp) > 0 && matching_rows > 0) {
    # Find matching company name in stock_tariff_data
    matching_index <- which(stock_tariff_data$permco == matched_crsp$permco)[1]
    
    linked_row <- data.frame(
      tariff_company = unique_companies$org_name[i],
      crsp_permco = matched_crsp$permco,
      crsp_name = stock_tariff_data$COMNAM[matching_index],
      similarity_score = best_match$similarity
    )
    
    linked_tariff_data <- rbind(linked_tariff_data, linked_row)
  }
}



# Okay, now we're doing it for real

# Make sure both datasets have standardized names
if (!"standardized_name" %in% names(unique_companies)) {
  unique_companies <- unique_companies %>%
    mutate(standardized_name = standardize_name(org_name))
}

if (!"standardized_name" %in% names(unique_crsp)) {
  unique_crsp <- unique_crsp %>%
    mutate(standardized_name = standardize_name(COMNAM))
}
find_best_matches <- function(tariff_companies, crsp_companies, threshold = 0.9) {
  # Initialize results dataframe
  results <- data.frame(
    tariff_index = integer(),
    crsp_index = integer(),
    similarity = numeric(),
    stringsAsFactors = FALSE
  )
  
  # Process in smaller batches to avoid memory issues
  batch_size <- 100
  n_batches <- ceiling(nrow(tariff_companies) / batch_size)
  
  for (batch in 1:n_batches) {
    cat("Processing batch", batch, "of", n_batches, "\n")
    
    # Get batch of companies
    start_idx <- (batch - 1) * batch_size + 1
    end_idx <- min(batch * batch_size, nrow(tariff_companies))
    batch_companies <- tariff_companies[start_idx:end_idx, ]
    
    # For each company in batch
    for (i in 1:nrow(batch_companies)) {
      tariff_name <- batch_companies$standardized_name[i]
      tariff_idx <- start_idx + i - 1
      
      # Calculate string distances
      distances <- stringdist(tariff_name, crsp_companies$standardized_name, method = "jw")
      similarities <- 1 - distances
      
      # Find best match
      best_idx <- which.max(similarities)
      best_similarity <- similarities[best_idx]
      
      # Only keep matches above threshold
      if (best_similarity >= threshold) {
        results <- rbind(results, data.frame(
          tariff_index = tariff_idx,
          crsp_index = best_idx,
          similarity = best_similarity
        ))
      }
    }
  }
  
  return(results)
}
# Find best matches
matches <- find_best_matches(unique_companies, unique_crsp, threshold = 0.9)

# Show summary of matches
cat("Found", nrow(matches), "matches out of", nrow(unique_companies), "companies\n")
cat("Similarity range:", min(matches$similarity), "to", max(matches$similarity), "\n")

# Create the linked dataset
linked_tariff_data <- data.frame(
  tariff_company = unique_companies$org_name[matches$tariff_index],
  crsp_permco = unique_crsp$PERMCO[matches$crsp_index],
  crsp_name = unique_crsp$standardized_name[matches$crsp_index],
  similarity_score = matches$similarity
)

# Show the first few matches
head(linked_tariff_data)

# Create a vector of PERMCO values to exclude
permcos_to_exclude <- c(42430,
                        56773,
                        57567,
                        56071,
                        57916,
                        3348,
                        20010,
                        59491,
                        12874,
                        11473,
                        35057,
                        5708,
                        45301,
                        57779,
                        53528,
                        6104,
                        59405,
                        57617,
                        58623,
                        54605,
                        59954,
                        7062,
                        56157,
                        58529,
                        55754,
                        20805,
                        56495,
                        59494,
                        4905,
                        12935,
                        59852,
                        54673,
                        925,
                        58690,
                        397,
                        20805,
                        59162,
                        56157,
                        6760,
                        53932,
                        26268,
                        59852,
                        59491,
                        56043,
                        13913,
                        727,
                        59804,
                        38389,
                        56823,
                        54511,
                        44602,
                        65,
                        57220,
                        42430,
                        20018,
                        2612,
                        2706,
                        47220,
                        2664,
                        59852,
                        21161,
                        5660,
                        56700,
                        3236,
                        56646,
                        57405,
                        59949,
                        21696,
                        21890,
                        20201,
                        727,
                        20232,
                        57595,
                        53972,
                        20232,
                        12874,
                        59405,
                        57375,
                        5708,
                        26268,
                        29768,
                        45857,
                        20653,
                        12032,
                        21696,
                        55719,
                        59494,
                        55867,
                        59491,
                        53528,
                        59405,
                        13487,
                        17489,
                        58145,
                        55943,
                        12583,
                        58296,
                        58676,
                        2183,
                        58282,
                        59405,
                        14293
                        
  # Add the PERMCO values you want to remove here
  # For example:
  # 12345,  # W W International (incorrect match for 24-7 International)
  # 67890,  # Another incorrect match
  # ...
)

# Filter out the problematic matches
linked_tariff_data_filtered <- linked_tariff_data %>%
  filter(!crsp_permco %in% permcos_to_exclude)

# Verify the filtering worked
cat("Before filtering:", nrow(linked_tariff_data), "matches\n")
cat("After filtering:", nrow(linked_tariff_data_filtered), "matches\n")

# Save the filtered dataset
write.csv(linked_tariff_data_filtered, "linked_tariff_data_filtered.csv", row.names = FALSE)



# Big one now -- time to get the before and after
install.packages("lubridate")
# Load necessary libraries
library(dplyr)
library(lubridate)

# Step 1: Create a function to get stock prices around a specific date for a given PERMCO
get_surrounding_prices <- function(permco, event_date, stock_data) {
  # Convert to Date object if it's not already
  event_date <- as.Date(event_date)
  
  # Get day before (or closest trading day before)
  day_before <- stock_data %>%
    filter(permco == permco, 
           date < event_date) %>%
    arrange(desc(date)) %>%
    slice(1)
  
  # Get day after (or closest trading day after)
  day_after <- stock_data %>%
    filter(permco == permco, 
           date > event_date) %>%
    arrange(date) %>%
    slice(1)
  
  # Return both prices
  return(list(
    before = if(nrow(day_before) > 0) day_before$PRC else NA,
    before_date = if(nrow(day_before) > 0) day_before$date else NA,
    after = if(nrow(day_after) > 0) day_after$PRC else NA,
    after_date = if(nrow(day_after) > 0) day_after$date else NA
  ))
}

# Step 2: Join tariff_exemptions with linked_tariff_data_filtered to get the matched PERMCOs
tariff_with_permco <- tariff_exemptions %>%
  inner_join(linked_tariff_data_filtered, by = c("org_name" = "tariff_company"))

# Step 3: Create a new dataframe with the stock prices
stock_price_changes <- data.frame(
  exemption_id = character(),
  org_name = character(),
  close_date = Date(),
  permco = numeric(),
  before_date = Date(),
  before_price = numeric(),
  after_date = Date(),
  after_price = numeric(),
  price_change = numeric(),
  price_change_pct = numeric(),
  stringsAsFactors = FALSE
)

# Step 4: For each tariff decision, find the stock prices
for (i in 1:nrow(tariff_with_permco)) {
  # Get current row
  current <- tariff_with_permco[i, ]
  
  # Get the exemption ID (assuming it's available, otherwise use row number)
  exemption_id <- if("id" %in% names(tariff_with_permco)) current$id else i
  
  # Get stock prices around decision date
  prices <- get_surrounding_prices(
    permco = current$crsp_permco,
    event_date = current$close_date,
    stock_data = stock_tariff_data
  )
  
  # Calculate price change
  price_change <- prices$after - prices$before
  price_change_pct <- if(!is.na(prices$before) && prices$before != 0) {
    (price_change / abs(prices$before)) * 100
  } else {
    NA
  }
  
  # Add to the results dataframe
  stock_price_changes <- rbind(stock_price_changes, data.frame(
    exemption_id = exemption_id,
    org_name = current$org_name,
    close_date = current$close_date,
    permco = current$crsp_permco,
    before_date = prices$before_date,
    before_price = prices$before,
    after_date = prices$after_date,
    after_price = prices$after,
    price_change = price_change,
    price_change_pct = price_change_pct,
    stringsAsFactors = FALSE
  ))
}

# Step 5: Summarize the results
summary_stats <- stock_price_changes %>%
  summarize(
    total_events = n(),
    events_with_price_data = sum(!is.na(price_change)),
    avg_price_change = mean(price_change, na.rm = TRUE),
    median_price_change = median(price_change, na.rm = TRUE),
    avg_pct_change = mean(price_change_pct, na.rm = TRUE),
    median_pct_change = median(price_change_pct, na.rm = TRUE)
  )

print(summary_stats)

# Step 6: Save the results
write.csv(stock_price_changes, "tariff_stock_price_changes.csv", row.names = FALSE)

if(i %% 100 == 0) {
  cat("Processed", i, "of", nrow(tariff_with_permco), "records\n")
}

# Load necessary libraries
library(dplyr)
library(ggplot2)

# Join stock_price_changes with tariff_exemptions to get decision information
price_changes_with_decision <- stock_price_changes %>%
  left_join(tariff_exemptions %>% select(org_name, close_date, decision), 
            by = c("org_name", "close_date"),
            relationship = "many-to-many")  # Handle the many-to-many relationship

# Calculate average percentage change for granted exemptions
granted_avg <- price_changes_with_decision %>%
  filter(decision == "Granted") %>%
  summarize(
    count = n(),
    avg_pct_change = mean(price_change_pct, na.rm = TRUE),
    median_pct_change = median(price_change_pct, na.rm = TRUE),
    sd_pct_change = sd(price_change_pct, na.rm = TRUE)
  )

# Calculate average percentage change for denied exemptions
denied_avg <- price_changes_with_decision %>%
  filter(decision == "Denied") %>%
  summarize(
    count = n(),
    avg_pct_change = mean(price_change_pct, na.rm = TRUE),
    median_pct_change = median(price_change_pct, na.rm = TRUE),
    sd_pct_change = sd(price_change_pct, na.rm = TRUE)
  )

# Print results
cat("Average percentage change after granted exemptions:", 
    round(granted_avg$avg_pct_change, 2), "% (n =", granted_avg$count, ")\n")

cat("Average percentage change after denied exemptions:", 
    round(denied_avg$avg_pct_change, 2), "% (n =", denied_avg$count, ")\n")

# Create a simple bar chart comparing the two
comparison_data <- data.frame(
  Decision = c("Granted", "Denied"),
  AvgPctChange = c(granted_avg$avg_pct_change, denied_avg$avg_pct_change)
)

ggplot(comparison_data, aes(x = Decision, y = AvgPctChange, fill = Decision)) +
  geom_bar(stat = "identity", width = 0.6) +
  geom_text(aes(label = paste0(round(AvgPctChange, 2), "%")), 
            vjust = ifelse(comparison_data$AvgPctChange >= 0, -0.5, 1.5)) +
  labs(
    title = "Average Stock Price Change After Tariff Exemption Decisions",
    y = "Average Percentage Change (%)",
    x = ""
  ) +
  scale_fill_manual(values = c("Granted" = "green3", "Denied" = "tomato")) +
  theme_minimal() +
  theme(legend.position = "none")

# Save the plot
ggsave("exemption_decision_comparison.png", width = 8, height = 6)

# Load necessary libraries
library(dplyr)
library(ggplot2)
library(stringr)

# First, join stock_price_changes with complete tariff_exemptions data including HTSUS codes
price_changes_complete <- stock_price_changes %>%
  left_join(tariff_exemptions %>% select(org_name, close_date, decision, htsus), 
            by = c("org_name", "close_date"),
            relationship = "many-to-many")

# Extract first 6 digits from HTSUS code
price_changes_complete <- price_changes_complete %>%
  mutate(htsus_6digit = str_sub(htsus, 1, 6))

# Identify date-decision-htsus6 combinations that appear more than once
duplicate_combinations <- price_changes_complete %>%
  group_by(close_date, decision, htsus_6digit) %>%
  summarize(count = n(), .groups = "drop") %>%
  filter(count > 1)

# Filter out the duplicate combinations
unique_decisions <- price_changes_complete %>%
  anti_join(duplicate_combinations, by = c("close_date", "decision", "htsus_6digit"))

# Calculate average percentage change for granted exemptions (robustness check)
granted_avg_robust <- unique_decisions %>%
  filter(decision == "Granted") %>%
  summarize(
    count = n(),
    avg_pct_change = mean(price_change_pct, na.rm = TRUE),
    median_pct_change = median(price_change_pct, na.rm = TRUE),
    sd_pct_change = sd(price_change_pct, na.rm = TRUE)
  )

# Calculate average percentage change for denied exemptions (robustness check)
denied_avg_robust <- unique_decisions %>%
  filter(decision == "Denied") %>%
  summarize(
    count = n(),
    avg_pct_change = mean(price_change_pct, na.rm = TRUE),
    median_pct_change = median(price_change_pct, na.rm = TRUE),
    sd_pct_change = sd(price_change_pct, na.rm = TRUE)
  )

# Print results of robustness check
cat("ROBUSTNESS CHECK: Excluding same-day, same-decision, same HTSUS-6 cases\n\n")
cat("Average percentage change after granted exemptions (robust):", 
    round(granted_avg_robust$avg_pct_change, 2), "% (n =", granted_avg_robust$count, ")\n")

cat("Average percentage change after denied exemptions (robust):", 
    round(denied_avg_robust$avg_pct_change, 2), "% (n =", denied_avg_robust$count, ")\n")

# Create a comparison chart with both original and robust results
comparison_data <- data.frame(
  Decision = rep(c("Granted", "Denied"), 2),
  Type = c(rep("All Cases", 2), rep("Robust", 2)),
  AvgPctChange = c(granted_avg$avg_pct_change, denied_avg$avg_pct_change,
                   granted_avg_robust$avg_pct_change, denied_avg_robust$avg_pct_change),
  Count = c(granted_avg$count, denied_avg$count, 
            granted_avg_robust$count, denied_avg_robust$count)
)

ggplot(comparison_data, aes(x = Decision, y = AvgPctChange, fill = Type)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.7), width = 0.6) +
  geom_text(aes(label = paste0(round(AvgPctChange, 2), "%\n(n=", Count, ")")), 
            position = position_dodge(width = 0.7),
            vjust = ifelse(comparison_data$AvgPctChange >= 0, -0.5, 1.5)) +
  labs(
    title = "Average Stock Price Change After Tariff Exemption Decisions",
    subtitle = "Comparing all cases vs. excluding same day-decision-product category cases",
    y = "Average Percentage Change (%)",
    x = ""
  ) +
  scale_fill_manual(values = c("All Cases" = "darkblue", "Robust" = "skyblue")) +
  theme_minimal()

# Save the plot
ggsave("exemption_decision_robust_comparison.png", width = 10, height = 7)


# Load necessary libraries

library(dplyr)
library(ggplot2)
library(stringr)

# First, join stock_price_changes with complete tariff_exemptions data including HTSUS codes
price_changes_complete <- stock_price_changes %>%
  left_join(tariff_exemptions %>% select(org_name, close_date, decision, htsus), 
            by = c("org_name", "close_date"),
            relationship = "many-to-many")

# Extract first 8 digits from HTSUS code
price_changes_complete <- price_changes_complete %>%
  mutate(htsus_8digit = str_sub(htsus, 1, 8))

# Calculate original averages (all cases)
granted_avg <- price_changes_complete %>%
  filter(decision == "Granted") %>%
  summarize(
    count = n(),
    avg_pct_change = mean(price_change_pct, na.rm = TRUE),
    median_pct_change = median(price_change_pct, na.rm = TRUE),
    sd_pct_change = sd(price_change_pct, na.rm = TRUE)
  )

denied_avg <- price_changes_complete %>%
  filter(decision == "Denied") %>%
  summarize(
    count = n(),
    avg_pct_change = mean(price_change_pct, na.rm = TRUE),
    median_pct_change = median(price_change_pct, na.rm = TRUE),
    sd_pct_change = sd(price_change_pct, na.rm = TRUE)
  )

# Print results
cat("ALL CASES:\n")
cat("Average percentage change after granted exemptions:", 
    round(granted_avg$avg_pct_change, 2), "% (n =", granted_avg$count, ")\n")
cat("Average percentage change after denied exemptions:", 
    round(denied_avg$avg_pct_change, 2), "% (n =", denied_avg$count, ")\n\n")

# Identify date-decision-htsus8 combinations that appear more than once
duplicate_combinations <- price_changes_complete %>%
  group_by(close_date, decision, htsus_8digit) %>%
  summarize(count = n(), .groups = "drop") %>%
  filter(count > 1)

# Filter out the duplicate combinations
unique_decisions <- price_changes_complete %>%
  anti_join(duplicate_combinations, by = c("close_date", "decision", "htsus_8digit"))

# Calculate average percentage change for granted exemptions (robustness check)
granted_avg_robust <- unique_decisions %>%
  filter(decision == "Granted") %>%
  summarize(
    count = n(),
    avg_pct_change = mean(price_change_pct, na.rm = TRUE),
    median_pct_change = median(price_change_pct, na.rm = TRUE),
    sd_pct_change = sd(price_change_pct, na.rm = TRUE)
  )

# Calculate average percentage change for denied exemptions (robustness check)
denied_avg_robust <- unique_decisions %>%
  filter(decision == "Denied") %>%
  summarize(
    count = n(),
    avg_pct_change = mean(price_change_pct, na.rm = TRUE),
    median_pct_change = median(price_change_pct, na.rm = TRUE),
    sd_pct_change = sd(price_change_pct, na.rm = TRUE)
  )

# Print results of robustness check
cat("ROBUSTNESS CHECK (8-digit HTSUS):\n")
cat("Average percentage change after granted exemptions:", 
    round(granted_avg_robust$avg_pct_change, 2), "% (n =", granted_avg_robust$count, ")\n")
cat("Average percentage change after denied exemptions:", 
    round(denied_avg_robust$avg_pct_change, 2), "% (n =", denied_avg_robust$count, ")\n")

# Create a comparison chart with both original and robust results
comparison_data <- data.frame(
  Decision = rep(c("Granted", "Denied"), 2),
  Type = c(rep("All Cases", 2), rep("8-digit HTSUS Filter", 2)),
  AvgPctChange = c(granted_avg$avg_pct_change, denied_avg$avg_pct_change,
                   granted_avg_robust$avg_pct_change, denied_avg_robust$avg_pct_change),
  Count = c(granted_avg$count, denied_avg$count, 
            granted_avg_robust$count, denied_avg_robust$count)
)

ggplot(comparison_data, aes(x = Decision, y = AvgPctChange, fill = Type)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.7), width = 0.6) +
  geom_text(aes(label = paste0(round(AvgPctChange, 2), "%\n(n=", Count, ")")), 
            position = position_dodge(width = 0.7),
            vjust = ifelse(comparison_data$AvgPctChange >= 0, -0.3, 1.3),
            size = 3) +  # Added size parameter to make text smaller
  labs(
    title = "Stock Price Change After Tariff Decision",
    y = "Average % Change",
    x = ""
  ) +
  scale_fill_manual(values = c("All Cases" = "darkblue", "8-digit HTSUS Filter" = "skyblue")) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 12),
    plot.subtitle = element_text(size = 10),
    legend.position = "bottom",  # Move legend to bottom to save vertical space
    axis.text = element_text(size = 9),  # Make axis text smaller
    plot.margin = margin(t = 10, r = 10, b = 10, l = 10)  # Add some margin
  ) +
  coord_cartesian(clip = "off")  # Allow plotting outside the plot panel

# Save the plot with adjusted dimensions - try a wider format
ggsave("exemption_decision_robust_comparison.png", width = 10, height = 5)
# Load necessary libraries
library(dplyr)
library(ggplot2)
library(stringr)

# First, join stock_price_changes with complete tariff_exemptions data including HTSUS codes
price_changes_complete <- stock_price_changes %>%
  left_join(tariff_exemptions %>% select(org_name, close_date, decision, htsus), 
            by = c("org_name", "close_date"),
            relationship = "many-to-many")

# Use the full 10-digit HTSUS code
price_changes_complete <- price_changes_complete %>%
  mutate(htsus_10digit = htsus)  # Using the full code

# Calculate original averages (all cases)
granted_avg <- price_changes_complete %>%
  filter(decision == "Granted") %>%
  summarize(
    count = n(),
    avg_pct_change = mean(price_change_pct, na.rm = TRUE),
    median_pct_change = median(price_change_pct, na.rm = TRUE),
    sd_pct_change = sd(price_change_pct, na.rm = TRUE)
  )

denied_avg <- price_changes_complete %>%
  filter(decision == "Denied") %>%
  summarize(
    count = n(),
    avg_pct_change = mean(price_change_pct, na.rm = TRUE),
    median_pct_change = median(price_change_pct, na.rm = TRUE),
    sd_pct_change = sd(price_change_pct, na.rm = TRUE)
  )

# Print results
cat("ALL CASES:\n")
cat("Average percentage change after granted exemptions:", 
    round(granted_avg$avg_pct_change, 2), "% (n =", granted_avg$count, ")\n")
cat("Average percentage change after denied exemptions:", 
    round(denied_avg$avg_pct_change, 2), "% (n =", denied_avg$count, ")\n\n")

# Identify date-decision-htsus10 combinations that appear more than once
duplicate_combinations <- price_changes_complete %>%
  group_by(close_date, decision, htsus_10digit) %>%
  summarize(count = n(), .groups = "drop") %>%
  filter(count > 1)

# Filter out the duplicate combinations
unique_decisions <- price_changes_complete %>%
  anti_join(duplicate_combinations, by = c("close_date", "decision", "htsus_10digit"))

# Calculate average percentage change for granted exemptions (robustness check)
granted_avg_robust <- unique_decisions %>%
  filter(decision == "Granted") %>%
  summarize(
    count = n(),
    avg_pct_change = mean(price_change_pct, na.rm = TRUE),
    median_pct_change = median(price_change_pct, na.rm = TRUE),
    sd_pct_change = sd(price_change_pct, na.rm = TRUE)
  )

# Calculate average percentage change for denied exemptions (robustness check)
denied_avg_robust <- unique_decisions %>%
  filter(decision == "Denied") %>%
  summarize(
    count = n(),
    avg_pct_change = mean(price_change_pct, na.rm = TRUE),
    median_pct_change = median(price_change_pct, na.rm = TRUE),
    sd_pct_change = sd(price_change_pct, na.rm = TRUE)
  )

# Print results of robustness check
cat("ROBUSTNESS CHECK (10-digit HTSUS):\n")
cat("Average percentage change after granted exemptions:", 
    round(granted_avg_robust$avg_pct_change, 2), "% (n =", granted_avg_robust$count, ")\n")
cat("Average percentage change after denied exemptions:", 
    round(denied_avg_robust$avg_pct_change, 2), "% (n =", denied_avg_robust$count, ")\n")

# Create a comparison chart with both original and robust results
comparison_data <- data.frame(
  Decision = rep(c("Granted", "Denied"), 2),
  Type = c(rep("All Cases", 2), rep("10-digit HTSUS Filter", 2)),
  AvgPctChange = c(granted_avg$avg_pct_change, denied_avg$avg_pct_change,
                   granted_avg_robust$avg_pct_change, denied_avg_robust$avg_pct_change),
  Count = c(granted_avg$count, denied_avg$count, 
            granted_avg_robust$count, denied_avg_robust$count)
)

ggplot(comparison_data, aes(x = Decision, y = AvgPctChange, fill = Type)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.7), width = 0.6) +
  geom_text(aes(label = paste0(round(AvgPctChange, 2), "%\n(n=", Count, ")")), 
            position = position_dodge(width = 0.7),
            vjust = ifelse(comparison_data$AvgPctChange >= 0, -0.5, 1.5)) +
  labs(
    title = "Stock Price Change After Tariff Exemption Decisions",
    subtitle = "All cases vs. filtered by 10-digit HTSUS",
    y = "Average % Change",
    x = ""
  ) +
  scale_fill_manual(values = c("All Cases" = "darkblue", "10-digit HTSUS Filter" = "skyblue")) +
  theme_minimal() +
  theme(plot.title = element_text(size = 12),
        plot.subtitle = element_text(size = 10))

# Save the plot
ggsave("exemption_decision_robust_comparison_10digit.png", width = 8, height = 6)

# Load necessary libraries
library(dplyr)
library(ggplot2)
library(stringr)

# First, join stock_price_changes with complete tariff_exemptions data including HTSUS codes
price_changes_complete <- stock_price_changes %>%
  left_join(tariff_exemptions %>% select(org_name, close_date, decision, htsus), 
            by = c("org_name", "close_date"),
            relationship = "many-to-many")

# Calculate original averages (all cases)
granted_avg <- price_changes_complete %>%
  filter(decision == "Granted") %>%
  summarize(
    count = n(),
    avg_pct_change = mean(price_change_pct, na.rm = TRUE),
    median_pct_change = median(price_change_pct, na.rm = TRUE),
    sd_pct_change = sd(price_change_pct, na.rm = TRUE)
  )

denied_avg <- price_changes_complete %>%
  filter(decision == "Denied") %>%
  summarize(
    count = n(),
    avg_pct_change = mean(price_change_pct, na.rm = TRUE),
    median_pct_change = median(price_change_pct, na.rm = TRUE),
    sd_pct_change = sd(price_change_pct, na.rm = TRUE)
  )

# Print results
cat("ALL CASES:\n")
cat("Average percentage change after granted exemptions:", 
    round(granted_avg$avg_pct_change, 2), "% (n =", granted_avg$count, ")\n")
cat("Average percentage change after denied exemptions:", 
    round(denied_avg$avg_pct_change, 2), "% (n =", denied_avg$count, ")\n\n")

# Identify date-htsus combinations that involve multiple different companies
# This is different from our previous check - we're looking for different companies, not just duplicate entries
multi_company_days <- price_changes_complete %>%
  group_by(close_date, htsus) %>%
  summarize(
    company_count = n_distinct(org_name),
    .groups = "drop"
  ) %>%
  filter(company_count > 1)

# Filter out cases where multiple different companies received decisions for the same product on the same day
unique_company_decisions <- price_changes_complete %>%
  anti_join(multi_company_days, by = c("close_date", "htsus"))

# Calculate average percentage change for granted exemptions (robustness check)
granted_avg_robust <- unique_company_decisions %>%
  filter(decision == "Granted") %>%
  summarize(
    count = n(),
    avg_pct_change = mean(price_change_pct, na.rm = TRUE),
    median_pct_change = median(price_change_pct, na.rm = TRUE),
    sd_pct_change = sd(price_change_pct, na.rm = TRUE)
  )

# Calculate average percentage change for denied exemptions (robustness check)
denied_avg_robust <- unique_company_decisions %>%
  filter(decision == "Denied") %>%
  summarize(
    count = n(),
    avg_pct_change = mean(price_change_pct, na.rm = TRUE),
    median_pct_change = median(price_change_pct, na.rm = TRUE),
    sd_pct_change = sd(price_change_pct, na.rm = TRUE)
  )

# Print results of robustness check
cat("ROBUSTNESS CHECK (Excluding multi-company same-day same-product decisions):\n")
cat("Average percentage change after granted exemptions:", 
    round(granted_avg_robust$avg_pct_change, 2), "% (n =", granted_avg_robust$count, ")\n")
cat("Average percentage change after denied exemptions:", 
    round(denied_avg_robust$avg_pct_change, 2), "% (n =", denied_avg_robust$count, ")\n")

# Create a comparison chart with both original and robust results
comparison_data <- data.frame(
  Decision = rep(c("Granted", "Denied"), 2),
  Type = c(rep("All Cases", 2), rep("Single-Company Filter", 2)),
  AvgPctChange = c(granted_avg$avg_pct_change, denied_avg$avg_pct_change,
                   granted_avg_robust$avg_pct_change, denied_avg_robust$avg_pct_change),
  Count = c(granted_avg$count, denied_avg$count, 
            granted_avg_robust$count, denied_avg_robust$count)
)

ggplot(comparison_data, aes(x = Decision, y = AvgPctChange, fill = Type)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.7), width = 0.6) +
  geom_text(aes(label = paste0(round(AvgPctChange, 2), "%\n(n=", Count, ")")), 
            position = position_dodge(width = 0.7),
            vjust = ifelse(comparison_data$AvgPctChange >= 0, -0.5, 1.5)) +
  labs(
    title = "Stock Price Change After Tariff Exemption Decisions",
    subtitle = "All cases vs. excluding multi-company same-day decisions",
    y = "Average % Change",
    x = ""
  ) +
  scale_fill_manual(values = c("All Cases" = "darkblue", "Single-Company Filter" = "skyblue")) +
  theme_minimal() +
  theme(plot.title = element_text(size = 12),
        plot.subtitle = element_text(size = 10))

# Save the plot
ggsave("exemption_decision_robust_comparison_unique_company.png", width = 8, height = 6)


# Update -- including confidence intervals and updated N

# Load necessary libraries
library(dplyr)
library(ggplot2)
library(stringr)

# First, join stock_price_changes with tariff exemptions data
price_changes_complete <- stock_price_changes %>%
  left_join(tariff_exemptions %>% select(org_name, close_date, decision, htsus), 
            by = c("org_name", "close_date"),
            relationship = "many-to-many")

# Aggregate to company-day level 
# For companies with multiple decisions on the same day, we'll track the decision types
company_day_decisions <- price_changes_complete %>%
  group_by(org_name, close_date, permco, before_date, before_price, after_date, after_price, price_change, price_change_pct) %>%
  summarize(
    granted_count = sum(decision == "Granted", na.rm = TRUE),
    denied_count = sum(decision == "Denied", na.rm = TRUE),
    decision_count = n(),
    # Determine the overall decision for the day (Granted, Denied, or Mixed)
    overall_decision = case_when(
      granted_count > 0 && denied_count == 0 ~ "Granted",
      denied_count > 0 && granted_count == 0 ~ "Denied",
      granted_count > 0 && denied_count > 0 ~ "Mixed",
      TRUE ~ NA_character_
    ),
    .groups = "drop"
  )

# Calculate statistics for each decision type
decision_stats <- company_day_decisions %>%
  filter(!is.na(overall_decision) & overall_decision != "Mixed") %>% # Exclude mixed decisions for clarity
  group_by(overall_decision) %>%
  summarize(
    count = n(),
    avg_pct_change = mean(price_change_pct, na.rm = TRUE),
    median_pct_change = median(price_change_pct, na.rm = TRUE),
    sd_pct_change = sd(price_change_pct, na.rm = TRUE),
    se_pct_change = sd_pct_change / sqrt(count),
    ci_lower = avg_pct_change - 1.96 * se_pct_change,
    ci_upper = avg_pct_change + 1.96 * se_pct_change,
    .groups = "drop"
  )

# Print results
cat("COMPANY-DAY ANALYSIS (each company counted once per day):\n")
print(decision_stats)

# Create a comparison chart with confidence intervals
ggplot(decision_stats, aes(x = overall_decision, y = avg_pct_change, fill = overall_decision)) +
  geom_bar(stat = "identity", width = 0.6) +
  geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper), width = 0.2) +
  geom_text(aes(label = paste0(round(avg_pct_change, 2), "%\n(n=", count, ")")), 
            vjust = ifelse(decision_stats$avg_pct_change >= 0, -0.5, 1.5)) +
  labs(
    title = "Average Stock Price Change After Tariff Exemption Decisions",
    subtitle = "Each company counted once per decision day, with 95% confidence intervals",
    y = "Average % Change",
    x = ""
  ) +
  scale_fill_manual(values = c("Granted" = "green3", "Denied" = "tomato")) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 12),
    plot.subtitle = element_text(size = 10),
    legend.position = "none"
  )

# Save the plot
ggsave("company_day_decision_analysis.png", width = 8, height = 6)

# Optional: Also examine the "Mixed" decision days
mixed_decision_stats <- company_day_decisions %>%
  filter(overall_decision == "Mixed") %>%
  summarize(
    count = n(),
    avg_pct_change = mean(price_change_pct, na.rm = TRUE),
    median_pct_change = median(price_change_pct, na.rm = TRUE)
  )

cat("\nCOMPANIES WITH MIXED DECISIONS ON THE SAME DAY:\n")
print(mixed_decision_stats)

# Load necessary libraries
library(dplyr)
library(ggplot2)
library(stringr)

# First, join stock_price_changes with tariff exemptions data
price_changes_complete <- stock_price_changes %>%
  left_join(tariff_exemptions %>% select(org_name, close_date, decision, htsus), 
            by = c("org_name", "close_date"),
            relationship = "many-to-many")

# Aggregate to company-day level with updated decision classification
company_day_decisions <- price_changes_complete %>%
  group_by(org_name, close_date, permco, before_date, before_price, after_date, after_price, price_change, price_change_pct) %>%
  summarize(
    granted_count = sum(decision == "Granted", na.rm = TRUE),
    denied_count = sum(decision == "Denied", na.rm = TRUE),
    decision_count = n(),
    # Updated classification: If ANY granted, classify as "Granted"; only if ALL denied, classify as "Denied"
    overall_decision = case_when(
      granted_count > 0 ~ "Granted",
      denied_count > 0 ~ "Denied",
      TRUE ~ NA_character_
    ),
    .groups = "drop"
  )

# Calculate statistics for each decision type
decision_stats <- company_day_decisions %>%
  filter(!is.na(overall_decision)) %>%
  group_by(overall_decision) %>%
  summarize(
    count = n(),
    avg_pct_change = mean(price_change_pct, na.rm = TRUE),
    median_pct_change = median(price_change_pct, na.rm = TRUE),
    sd_pct_change = sd(price_change_pct, na.rm = TRUE),
    se_pct_change = sd_pct_change / sqrt(count),
    ci_lower = avg_pct_change - 1.96 * se_pct_change,
    ci_upper = avg_pct_change + 1.96 * se_pct_change,
    .groups = "drop"
  )

# Print results
cat("COMPANY-DAY ANALYSIS (revised classification):\n")
cat("'Granted' = Any product exemption granted that day\n")
cat("'Denied' = All product exemptions denied that day\n\n")
print(decision_stats)

# Create a comparison chart with confidence intervals
ggplot(decision_stats, aes(x = overall_decision, y = avg_pct_change, fill = overall_decision)) +
  geom_bar(stat = "identity", width = 0.6) +
  geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper), width = 0.2) +
  geom_text(aes(label = paste0(round(avg_pct_change, 2), "%\n(n=", count, ")")), 
            vjust = ifelse(decision_stats$avg_pct_change >= 0, -0.5, 1.5)) +
  labs(
    title = "Stock Price Change After Tariff Exemption Decisions",
    subtitle = "Each company counted once per day; 'Granted' if ANY exemption granted",
    y = "Average % Change",
    x = ""
  ) +
  scale_fill_manual(values = c("Granted" = "green3", "Denied" = "tomato")) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 12),
    plot.subtitle = element_text(size = 10),
    legend.position = "none"
  )

# Save the plot
ggsave("company_day_revised_classification.png", width = 8, height = 6)

# Additional analysis: How many products on average per company-day?
product_counts <- company_day_decisions %>%
  group_by(overall_decision) %>%
  summarize(
    avg_products = mean(decision_count),
    max_products = max(decision_count),
    min_products = min(decision_count)
  )

cat("\nAVERAGE NUMBER OF PRODUCTS PER COMPANY-DAY:\n")
print(product_counts)

# Load necessary libraries
library(dplyr)
library(ggplot2)
library(stringr)

# First, join stock_price_changes with tariff exemptions data
price_changes_complete <- stock_price_changes %>%
  left_join(tariff_exemptions %>% select(org_name, close_date, decision, htsus), 
            by = c("org_name", "close_date"),
            relationship = "many-to-many")

# Aggregate to company-day level with proportion-based weighting
company_day_decisions <- price_changes_complete %>%
  group_by(org_name, close_date, permco, before_date, before_price, after_date, after_price, price_change, price_change_pct) %>%
  summarize(
    granted_count = sum(decision == "Granted", na.rm = TRUE),
    denied_count = sum(decision == "Denied", na.rm = TRUE),
    decision_count = n(),
    # Calculate proportion of granted decisions
    granted_proportion = granted_count / decision_count,
    # Classify based on majority rule
    overall_decision = case_when(
      granted_count > denied_count ~ "Granted",
      denied_count > granted_count ~ "Denied",
      granted_count == denied_count ~ "Tied",
      TRUE ~ NA_character_
    ),
    .groups = "drop"
  )

# Compute weighted averages
granted_weighted_avg <- company_day_decisions %>%
  filter(overall_decision == "Granted") %>%
  summarize(
    count = n(),
    # Standard (unweighted) statistics
    avg_pct_change = mean(price_change_pct, na.rm = TRUE),
    # Weighted calculations
    weighted_avg = sum(price_change_pct * granted_proportion, na.rm = TRUE) / sum(granted_proportion, na.rm = TRUE),
    weighted_var = sum(granted_proportion * (price_change_pct - weighted_avg)^2, na.rm = TRUE) / sum(granted_proportion, na.rm = TRUE),
    weighted_sd = sqrt(weighted_var),
    weighted_se = weighted_sd / sqrt(count),
    weighted_ci_lower = weighted_avg - 1.96 * weighted_se,
    weighted_ci_upper = weighted_avg + 1.96 * weighted_se,
    # Average proportion of granted decisions
    avg_granted_proportion = mean(granted_proportion)
  )

denied_weighted_avg <- company_day_decisions %>%
  filter(overall_decision == "Denied") %>%
  summarize(
    count = n(),
    # Standard (unweighted) statistics
    avg_pct_change = mean(price_change_pct, na.rm = TRUE),
    # Weighted calculations (using denied proportion)
    weighted_avg = sum(price_change_pct * (1-granted_proportion), na.rm = TRUE) / sum(1-granted_proportion, na.rm = TRUE),
    weighted_var = sum((1-granted_proportion) * (price_change_pct - weighted_avg)^2, na.rm = TRUE) / sum(1-granted_proportion, na.rm = TRUE),
    weighted_sd = sqrt(weighted_var),
    weighted_se = weighted_sd / sqrt(count),
    weighted_ci_lower = weighted_avg - 1.96 * weighted_se,
    weighted_ci_upper = weighted_avg + 1.96 * weighted_se,
    # Average proportion of denied decisions
    avg_denied_proportion = mean(1-granted_proportion)
  )

# Print results
cat("WEIGHTED COMPANY-DAY ANALYSIS:\n")
cat("'Granted' days weighted by proportion of granted decisions:\n")
print(granted_weighted_avg)

cat("\n'Denied' days weighted by proportion of denied decisions:\n")
print(denied_weighted_avg)

# Combine for plotting
weighted_comparison <- data.frame(
  Decision = c("Granted", "Denied"),
  WeightedAvg = c(granted_weighted_avg$weighted_avg, denied_weighted_avg$weighted_avg),
  CI_Lower = c(granted_weighted_avg$weighted_ci_lower, denied_weighted_avg$weighted_ci_lower),
  CI_Upper = c(granted_weighted_avg$weighted_ci_upper, denied_weighted_avg$weighted_ci_upper),
  Count = c(granted_weighted_avg$count, denied_weighted_avg$count),
  AvgProportion = c(granted_weighted_avg$avg_granted_proportion, denied_weighted_avg$avg_denied_proportion)
)

# Create a comparison chart with confidence intervals
ggplot(weighted_comparison, aes(x = Decision, y = WeightedAvg, fill = Decision)) +
  geom_bar(stat = "identity", width = 0.6) +
  geom_errorbar(aes(ymin = CI_Lower, ymax = CI_Upper), width = 0.2) +
  geom_text(aes(label = paste0(round(WeightedAvg, 2), "%\n(n=", Count, ")\nAvg. prop: ", 
                               round(AvgProportion * 100), "%")), 
            vjust = ifelse(weighted_comparison$WeightedAvg >= 0, -0.5, 1.5)) +
  labs(
    title = "Weighted Stock Price Change After Tariff Exemption Decisions",
    subtitle = "Days weighted by proportion of granted/denied decisions",
    y = "Weighted Average % Change",
    x = ""
  ) +
  scale_fill_manual(values = c("Granted" = "green3", "Denied" = "tomato")) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 12),
    plot.subtitle = element_text(size = 10),
    legend.position = "none"
  )

# Save the plot
ggsave("company_day_weighted_analysis.png", width = 8, height = 6)

# Load necessary libraries
library(dplyr)
library(ggplot2)
library(stringr)

# First, join stock_price_changes with tariff exemptions data
price_changes_complete <- stock_price_changes %>%
  left_join(tariff_exemptions %>% select(org_name, close_date, decision, htsus), 
            by = c("org_name", "close_date"),
            relationship = "many-to-many")

# Aggregate to company-day level with net decision calculations
company_day_decisions <- price_changes_complete %>%
  group_by(org_name, close_date, permco, before_date, before_price, after_date, after_price, price_change, price_change_pct) %>%
  summarize(
    granted_count = sum(decision == "Granted", na.rm = TRUE),
    denied_count = sum(decision == "Denied", na.rm = TRUE),
    decision_count = n(),
    # Calculate net decisions
    net_granted = granted_count - denied_count,
    net_denied = denied_count - granted_count,
    # Calculate net proportion
    net_proportion = abs(net_granted) / decision_count,
    # Determine overall direction
    net_direction = case_when(
      net_granted > 0 ~ "Net Granted",
      net_granted < 0 ~ "Net Denied",
      TRUE ~ "Neutral"
    ),
    .groups = "drop"
  ) %>%
  filter(!is.na(price_change_pct)) # Remove entries with missing price data

# Calculate extrapolated effects
extrapolated_effects <- company_day_decisions %>%
  filter(net_direction != "Neutral") %>% # Exclude cases where granted = denied
  mutate(
    # Extrapolate to full effect
    extrapolated_change = price_change_pct / net_proportion
  )

# Calculate statistics for each direction
granted_stats <- extrapolated_effects %>%
  filter(net_direction == "Net Granted") %>%
  summarize(
    count = n(),
    avg_extrapolated = mean(extrapolated_change, na.rm = TRUE),
    median_extrapolated = median(extrapolated_change, na.rm = TRUE),
    sd_extrapolated = sd(extrapolated_change, na.rm = TRUE),
    se_extrapolated = sd_extrapolated / sqrt(count),
    ci_lower = avg_extrapolated - 1.96 * se_extrapolated,
    ci_upper = avg_extrapolated + 1.96 * se_extrapolated,
    avg_net_proportion = mean(net_proportion),
    avg_net_count = mean(net_granted)
  )

denied_stats <- extrapolated_effects %>%
  filter(net_direction == "Net Denied") %>%
  summarize(
    count = n(),
    avg_extrapolated = mean(extrapolated_change, na.rm = TRUE),
    median_extrapolated = median(extrapolated_change, na.rm = TRUE),
    sd_extrapolated = sd(extrapolated_change, na.rm = TRUE),
    se_extrapolated = sd_extrapolated / sqrt(count),
    ci_lower = avg_extrapolated - 1.96 * se_extrapolated,
    ci_upper = avg_extrapolated + 1.96 * se_extrapolated,
    avg_net_proportion = mean(net_proportion),
    avg_net_count = mean(abs(net_granted))
  )

# Print results
cat("EXTRAPOLATED EFFECT ANALYSIS:\n")
cat("Assuming granted and denied decisions have opposite and canceling effects\n\n")

cat("NET GRANTED EFFECT (extrapolated to 100%):\n")
cat("Extrapolated average:", round(granted_stats$avg_extrapolated, 2), "%\n")
cat("95% CI: [", round(granted_stats$ci_lower, 2), ",", round(granted_stats$ci_upper, 2), "]\n")
cat("Based on", granted_stats$count, "company-days with avg. net proportion of", 
    round(granted_stats$avg_net_proportion * 100), "% (avg. net granted:", round(granted_stats$avg_net_count, 1), ")\n\n")

cat("NET DENIED EFFECT (extrapolated to 100%):\n")
cat("Extrapolated average:", round(denied_stats$avg_extrapolated, 2), "%\n")
cat("95% CI: [", round(denied_stats$ci_lower, 2), ",", round(denied_stats$ci_upper, 2), "]\n")
cat("Based on", denied_stats$count, "company-days with avg. net proportion of", 
    round(denied_stats$avg_net_proportion * 100), "% (avg. net denied:", round(denied_stats$avg_net_count, 1), ")\n")

# Combine for plotting
extrapolated_results <- data.frame(
  Decision = c("Net Granted", "Net Denied"),
  ExtrapAvg = c(granted_stats$avg_extrapolated, denied_stats$avg_extrapolated),
  CI_Lower = c(granted_stats$ci_lower, denied_stats$ci_lower),
  CI_Upper = c(granted_stats$ci_upper, denied_stats$ci_upper),
  Count = c(granted_stats$count, denied_stats$count),
  AvgNetProp = c(granted_stats$avg_net_proportion, denied_stats$avg_net_proportion)
)

# Create comparison chart
ggplot(extrapolated_results, aes(x = Decision, y = ExtrapAvg, fill = Decision)) +
  geom_bar(stat = "identity", width = 0.6) +
  geom_errorbar(aes(ymin = CI_Lower, ymax = CI_Upper), width = 0.2) +
  geom_text(aes(label = paste0(round(ExtrapAvg, 2), "%\n(n=", Count, ")\nAvg. net: ", 
                               round(AvgNetProp * 100), "%")), 
            vjust = ifelse(extrapolated_results$ExtrapAvg >= 0, -0.5, 1.5)) +
  labs(
    title = "Extrapolated Stock Price Change After Tariff Exemption Decisions",
    subtitle = "Effects extrapolated assuming granted and denied decisions cancel each other",
    y = "Extrapolated Average % Change",
    x = ""
  ) +
  scale_fill_manual(values = c("Net Granted" = "green3", "Net Denied" = "tomato")) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 12),
    plot.subtitle = element_text(size = 10),
    legend.position = "none"
  )

# Save the plot
ggsave("extrapolated_effect_analysis.png", width = 8, height = 6)


# Load necessary libraries
library(dplyr)
library(ggplot2)
library(stringr)

# First, join stock_price_changes with tariff exemptions data
price_changes_complete <- stock_price_changes %>%
  left_join(tariff_exemptions %>% select(org_name, close_date, decision, htsus), 
            by = c("org_name", "close_date"),
            relationship = "many-to-many")

# Aggregate to company-day level
company_day_decisions <- price_changes_complete %>%
  group_by(org_name, close_date, permco, before_date, before_price, after_date, after_price, price_change, price_change_pct) %>%
  summarize(
    granted_count = sum(decision == "Granted", na.rm = TRUE),
    denied_count = sum(decision == "Denied", na.rm = TRUE),
    decision_count = n(),
    # Determine if decisions are uniform
    is_all_granted = granted_count > 0 && denied_count == 0,
    is_all_denied = denied_count > 0 && granted_count == 0,
    is_mixed = granted_count > 0 && denied_count > 0,
    .groups = "drop"
  ) %>%
  filter(!is.na(price_change_pct)) # Remove entries with missing price data

# Filter to only include company-days where all decisions are the same
uniform_decisions <- company_day_decisions %>%
  filter(is_all_granted | is_all_denied) %>%
  mutate(decision_type = ifelse(is_all_granted, "Granted", "Denied"))

# Calculate statistics for each decision type
decision_stats <- uniform_decisions %>%
  group_by(decision_type) %>%
  summarize(
    count = n(),
    avg_pct_change = mean(price_change_pct, na.rm = TRUE),
    median_pct_change = median(price_change_pct, na.rm = TRUE),
    sd_pct_change = sd(price_change_pct, na.rm = TRUE),
    se_pct_change = sd_pct_change / sqrt(count),
    ci_lower = avg_pct_change - 1.96 * se_pct_change,
    ci_upper = avg_pct_change + 1.96 * se_pct_change,
    avg_decision_count = mean(decision_count),
    .groups = "drop"
  )

# Print results
cat("COMPANY-DAY ANALYSIS (UNIFORM DECISIONS ONLY):\n")
cat("Only including company-days where all decisions are the same type\n\n")

cat("GRANTED DECISIONS:\n")
granted_stats <- decision_stats %>% filter(decision_type == "Granted")
cat("Average percentage change:", round(granted_stats$avg_pct_change, 2), "%\n")
cat("95% CI: [", round(granted_stats$ci_lower, 2), ",", round(granted_stats$ci_upper, 2), "]\n")
cat("Based on", granted_stats$count, "company-days (avg.", round(granted_stats$avg_decision_count, 1), "products per day)\n\n")

cat("DENIED DECISIONS:\n")
denied_stats <- decision_stats %>% filter(decision_type == "Denied")
cat("Average percentage change:", round(denied_stats$avg_pct_change, 2), "%\n")
cat("95% CI: [", round(denied_stats$ci_lower, 2), ",", round(denied_stats$ci_upper, 2), "]\n")
cat("Based on", denied_stats$count, "company-days (avg.", round(denied_stats$avg_decision_count, 1), "products per day)\n\n")

cat("MIXED DECISIONS (EXCLUDED):\n")
mixed_count <- sum(company_day_decisions$is_mixed)
cat("Excluded", mixed_count, "company-days with mixed decisions\n")

# Create comparison chart
ggplot(decision_stats, aes(x = decision_type, y = avg_pct_change, fill = decision_type)) +
  geom_bar(stat = "identity", width = 0.6) +
  geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper), width = 0.2) +
  geom_text(aes(label = paste0(round(avg_pct_change, 2), "%\n(n=", count, ")\nAvg. products: ", 
                               round(avg_decision_count, 1))), 
            vjust = ifelse(decision_stats$avg_pct_change >= 0, -0.5, 1.5)) +
  labs(
    title = "Stock Price Change After Tariff Exemption Decisions",
    subtitle = "Only including company-days with uniform decision types",
    y = "Average % Change",
    x = ""
  ) +
  scale_fill_manual(values = c("Granted" = "green3", "Denied" = "tomato")) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 12),
    plot.subtitle = element_text(size = 10),
    legend.position = "none"
  ) +
  scale_x_discrete(labels = c("All Granted", "All Denied"))

# Save the plot
ggsave("uniform_decisions_analysis.png", width = 8, height = 6)

# Check the total number of unique company-day combinations in our original data
total_company_days <- price_changes_complete %>%
  group_by(org_name, close_date) %>%
  summarize(count = n(), .groups = "drop") %>%
  nrow()

# Check how many have uniform decisions
uniform_company_days <- company_day_decisions %>%
  filter(is_all_granted | is_all_denied) %>%
  nrow()

# Check how many have mixed decisions
mixed_company_days <- company_day_decisions %>%
  filter(is_mixed) %>%
  nrow()

cat("Total unique company-day combinations:", total_company_days, "\n")
cat("Company-days with uniform decisions:", uniform_company_days, "\n")
cat("Company-days with mixed decisions:", mixed_company_days, "\n")
cat("Company-days with all granted:", sum(company_day_decisions$is_all_granted), "\n")
cat("Company-days with all denied:", sum(company_day_decisions$is_all_denied), "\n")