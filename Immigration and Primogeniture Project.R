# Step 1: Use file.choose() to select your CSV file interactively
file_path <- file.choose()

# Step 2: Print the selected path (optional)
print(file_path)

# Step 3: Read the CSV file
# Using base R
dataset <- read.csv(file_path)

# Alternatively, for large files, use data.table package
# install.packages("data.table")  # Uncomment if not installed
# library(data.table)
# dataset <- fread(file_path)

# Step 4: Examine the data
# View first few rows
head(dataset)

# Get dimensions (rows and columns)
dim(dataset)

# Get column names
names(dataset)

# Get summary statistics
summary(dataset)

# Step 5: Check for any issues with the data
# Look for missing values
colSums(is.na(dataset))

# Step 6: Basic analysis examples
# If you want to see the structure of the data
str(dataset)

# Create a simple histogram for a numeric column (replace "column_name" with an actual column)
# hist(dataset$column_name)

# Create a table for a categorical variable
# table(dataset$categorical_column)

# Load required libraries
library(dplyr)

# Assuming your original IPUMS dataset is named 'ipums_data'
# Create a new dataset with only people born in specific places
# Replace the numbers in the c() function with your specific BPLD codes

filtered_dataset <- dataset %>%
  filter(BPLD %in% c(45300, 45301, 45302, 45305, 45311, 45312, 45313, 45314, 45315, 45316, 45317, 45318, 45319, 45320, 45321, 45322, 45323, 45324, 45325, 45326, 45327, 45328, 45329, 45330, 45331, 43532, 45333, 45341, 45342, 45344, 45345, 45346, 45347, 45348, 45349, 45350, 45351, 45352, 45353, 45360, 45361, 45362)) # Replace with your actual BPLD codes of interest

# Create two groupings

# Inheritance Rules Classification Using BPLD Codes
# For 1880 US Census Analysis

# Function to classify regions based on inheritance rules
classify_inheritance_rules <- function(bpld_code) {
  # Create a lookup table for BPLD codes and inheritance classification
  # 1 = Equal sharing among heirs
  # 0 = Primogeniture (firstborn inherits everything)
  
  # IPUMS BPLD codes vary by specific regions
  # You'll need to modify this with the specific BPLD codes from your data
  inheritance_rules <- list(
    # Equal sharing regions (1)
    "45311" = 1,  # Baden
    "45317" = 1,  # Hessen
    "45322" = 1,  # Rheinland
    "45325" = 1,  # Sigmaringen
    "45326" = 1,  # Schwarzburg
    "45328" = 1,  # Wurttemberg
    "45331" = 1,  # Frankfurt
    "45332" = 1,  # Saarland
    "45347" = 1,  # Thuringian States
    "45348" = 1,  # Sachsen Meiningen
    "45349" = 1,  # Sachsen Weimar Eisenach
    
    
    # Primogeniture regions (0)
    "45301" = 0,  # Berlin
    "45312" = 0,  # Bavaria
    "45313" = 0,  # Braunschweig
    "45314" = 0,  # Bremen 
    "45315" = 0,  # Hamburg
    "45316" = 0,  # Hanover
    "45319" = 0,  # Lippe
    "45320" = 0,  # Lubeck
    "45321" = 0,  # Oldenburg
    "45323" = 0,  # Schaumberg-Lippe
    "45324" = 0,  # Schleswig
    "45327" = 0,  # Westphalia
    "45329" = 0,  # Waldeck
    "45330" = 0,  # Wittenberg
    "45344" = 0,  # Kingdom of Saxony
    "45345" = 0,  # Mecklenburg
    "45346" = 0,  # Saxony
    "45351" = 0,  # Schwerin
    "45360" = 0,  #Prussia nec(?)
    "45361" = 0,  # Hohenzollern
    "45362" = 0   # Niedersachsen
  )
  
  # Return the inheritance classification if the BPLD code exists
  if (!is.null(inheritance_rules[[as.character(bpld_code)]])) {
    return(inheritance_rules[[as.character(bpld_code)]])
  } else {
    # Return NA for unknown regions
    message(paste("Warning: Unknown BPLD code", bpld_code))
    return(NA)
  }
}

# Apply the classification function to create the equal_sharing variable
filtered_dataset$equal_sharing <- sapply(filtered_dataset$BPLD, classify_inheritance_rules)

# Run regression with the correct dataset name
model <- lm(OCCSCORE ~ equal_sharing, data = filtered_dataset)

# Print model summary
summary(model)

# Create simple plot with the correct dataset name
plot(filtered_dataset$equal_sharing, filtered_dataset$OCCSCORE, 
     xlab = "Inheritance Rules", 
     ylab = "Occupational Score",
     pch = 20,
     col = "darkgray")

# Add regression line
abline(model, col = "red", lwd = 2)

# Filter the dataset to only include valid FARM values (1=non-farm, 2=farm)
# and valid inheritance rule classifications
valid_data <- filtered_dataset %>%
  filter(FARM %in% c(1, 2)) %>%
  filter(!is.na(equal_sharing))

# Convert FARM to a binary variable where 1=farm (originally 2)
valid_data$is_farm <- ifelse(valid_data$FARM == 2, 1, 0)

# Run logistic regression (since is_farm is binary)
farm_model <- glm(is_farm ~ equal_sharing, 
                  family = binomial(link = "logit"), 
                  data = valid_data)

# Print model summary
summary(farm_model)

# Create plot
# For logistic regression, we can plot the predicted probabilities
pred_probs <- predict(farm_model, type = "response")

# Use different colors for actual farm status
plot(valid_data$equal_sharing, valid_data$is_farm,
     xlab = "Inheritance Rules ",
     ylab = "Farm Status ",
     pch = 20,
     col = ifelse(valid_data$is_farm == 1, "darkgreen", "darkblue"))

# Add legend


# Add fitted probability curve
# Create sequence of x values
x_seq <- seq(0, 1, length.out = 100)
# Create new data frame for prediction
newdata <- data.frame(equal_sharing = x_seq)
# Predict probabilities
y_seq <- predict(farm_model, newdata = newdata, type = "response")
# Add line to plot
lines(x_seq, y_seq, col = "red", lwd = 2)

#Create father's birthplace data

# First, apply the classification function to create the father_equal_sharing variable
# Note: There's a parameter name mismatch in your function (bpld_code vs pld_code)
# Let's fix that and apply it correctly

# Fixed classify function for father's birthplace
classify_father_inheritance <- function(fbpld_code) {
  # Same inheritance rules list as before
  inheritance_rules <- list(
    # Equal sharing regions (1)
    "45311" = 1,  # Baden
    "45317" = 1,  # Hessen
    "45322" = 1,  # Rheinland
    "45325" = 1,  # Sigmaringen
    "45326" = 1,  # Schwarzburg
    "45328" = 1,  # Wurttemberg
    "45331" = 1,  # Frankfurt
    "45332" = 1,  # Saarland
    "45347" = 1,  # Thuringian States
    "45348" = 1,  # Sachsen Meiningen
    "45349" = 1,  # Sachsen Weimar Eisenach
    
    # Primogeniture regions (0)
    "45301" = 0,  # Berlin
    "45312" = 0,  # Bavaria
    "45313" = 0,  # Braunschweig
    "45314" = 0,  # Bremen 
    "45315" = 0,  # Hamburg
    "45316" = 0,  # Hanover
    "45319" = 0,  # Lippe
    "45320" = 0,  # Lubeck
    "45321" = 0,  # Oldenburg
    "45323" = 0,  # Schaumberg-Lippe
    "45324" = 0,  # Schleswig
    "45327" = 0,  # Westphalia
    "45329" = 0,  # Waldeck
    "45330" = 0,  # Wittenberg
    "45344" = 0,  # Kingdom of Saxony
    "45345" = 0,  # Mecklenburg
    "45346" = 0,  # Saxony
    "45351" = 0,  # Schwerin
    "45360" = 0,  # Prussia nec
    "45361" = 0,  # Hohenzollern
    "45362" = 0   # Niedersachsen
  )
  
  # Return the inheritance classification if the FBPLD code exists
  if (!is.null(inheritance_rules[[as.character(fbpld_code)]])) {
    return(inheritance_rules[[as.character(fbpld_code)]])
  } else {
    # Return NA for unknown regions
    return(NA)
  }
}

# Apply the classification function to create father_equal_sharing
filtered_dataset_father$father_equal_sharing <- sapply(filtered_dataset_father$FBPLD, classify_father_inheritance)

# Run the regression of OCCSCORE on father's inheritance rules
father_model <- lm(OCCSCORE ~ father_equal_sharing, data = filtered_dataset_father)

# Print model summary
summary(father_model)

# Create simple plot
plot(filtered_dataset_father$father_equal_sharing, filtered_dataset_father$OCCSCORE, 
     xlab = "Father's Inheritance Rules (0=Primogeniture, 1=Equal Sharing)", 
     ylab = "Occupational Score",
     pch = 20,
     col = "darkgray")

# Add regression line
abline(father_model, col = "red", lwd = 2)

#testing for male-female proportion
# Filter out invalid SEX values (9) from the dataset
valid_sex_data <- filtered_dataset %>%
  filter(SEX %in% c(1, 2)) %>%
  filter(!is.na(equal_sharing))

# Create binary male indicator (1 = male, 0 = female)
valid_sex_data$is_male <- ifelse(valid_sex_data$SEX == 1, 1, 0)

# Calculate percentage male by inheritance type
sex_by_inheritance <- valid_sex_data %>%
  group_by(equal_sharing) %>%
  summarize(
    total_count = n(),
    male_count = sum(is_male),
    percent_male = mean(is_male) * 100
  )

# Print results
print(sex_by_inheritance)

# Create a bar plot comparing percentage male
barplot(sex_by_inheritance$percent_male, 
        names.arg = c("Primogeniture", "Equal Sharing")[sex_by_inheritance$equal_sharing + 1],
        col = c("lightblue", "darkblue"),
        ylab = "Percentage Male",
        main = "Percentage Male by Inheritance System",
        ylim = c(0, 100))

# Add percentage labels on the bars
text(x = 1:length(sex_by_inheritance$percent_male), 
     y = sex_by_inheritance$percent_male + 5,
     labels = paste0(round(sex_by_inheritance$percent_male, 1), "%"),
     xpd = TRUE)

# Optional: Statistical test for difference in proportions
prop_test <- prop.test(sex_by_inheritance$male_count, sex_by_inheritance$total_count)
print(prop_test)

# Filter out invalid SEX values (9) from the dataset using father's birthplace
valid_sex_data_father <- filtered_dataset_father %>%
  filter(SEX %in% c(1, 2)) %>%
  filter(!is.na(father_equal_sharing))

# Create binary male indicator (1 = male, 0 = female)
valid_sex_data_father$is_male <- ifelse(valid_sex_data_father$SEX == 1, 1, 0)

# Calculate percentage male by father's inheritance type
sex_by_father_inheritance <- valid_sex_data_father %>%
  group_by(father_equal_sharing) %>%
  summarize(
    total_count = n(),
    male_count = sum(is_male),
    percent_male = mean(is_male) * 100
  )

# Print results
print(sex_by_father_inheritance)

# Create a bar plot comparing percentage male
barplot(sex_by_father_inheritance$percent_male, 
        names.arg = c("Primogeniture", "Equal Sharing")[sex_by_father_inheritance$father_equal_sharing + 1],
        col = c("lightblue", "darkblue"),
        ylab = "Percentage Male",
        main = "Father's Inheritance System",
        ylim = c(0, 100))

# Add percentage labels on the bars
text(x = 1:length(sex_by_father_inheritance$percent_male), 
     y = sex_by_father_inheritance$percent_male + 5,
     labels = paste0(round(sex_by_father_inheritance$percent_male, 1), "%"),
     xpd = TRUE)

# Optional: Statistical test for difference in proportions
father_prop_test <- prop.test(sex_by_father_inheritance$male_count, sex_by_father_inheritance$total_count)
print(father_prop_test)



