# Load necessary libraries
library(tidyverse)

# Import the dataset
data <- read_csv("hdro_indicators_zwe.csv")

# View the first few rows to understand its structure
head(data)

# Clean column names
colnames(data) <- colnames(data) %>%
  str_replace_all("#", "") %>%
  str_replace_all("\\+", "_") %>%
  str_trim()

# Check cleaned column names
colnames(data)
# Filter data for Adolescent Birth Rate (ABR)
abr_data <- data %>% filter(indicator_id == "abr")

# Filter data for CO2 emissions
co2_data <- data %>% filter(indicator_id == "co2_prod")


# Plot Adolescent Birth Rate
ggplot(abr_data, aes(x = year, y = value)) +
  geom_line(color = "blue") +
  geom_point() +
  labs(title = "Adolescent Birth Rate Over the Years",
       x = "Year",
       y = "Birth Rate (per 1,000 women ages 15-19)") +
  theme_minimal()

# Plot CO2 Emissions
ggplot(co2_data, aes(x = year, y = value)) +
  geom_line(color = "red") +
  geom_point() +
  labs(title = "CO2 Emissions Over the Years",
       x = "Year",
       y = "CO2 Emissions (tonnes per capita)") +
  theme_minimal()

#investigating relationship between ABR and CO2 if it exists 
# Merge ABR and CO2 datasets by year
merged_data <- abr_data %>%
  select(year, abr_value = value) %>%
  inner_join(co2_data %>% select(year, co2_value = value), by = "year")

# Calculate correlation
correlation <- cor(merged_data$abr_value, merged_data$co2_value)

# Display correlation result
print(paste("Correlation between ABR and CO2 emissions:", round(correlation, 4)))

# Summary of trends
summary_abr <- abr_data %>%
  summarise(min_rate = min(value, na.rm = TRUE), 
            max_rate = max(value, na.rm = TRUE), 
            avg_rate = mean(value, na.rm = TRUE))

summary_co2 <- co2_data %>%
  summarise(min_emission = min(value, na.rm = TRUE), 
            max_emission = max(value, na.rm = TRUE), 
            avg_emission = mean(value, na.rm = TRUE))

# Print summaries
print("Summary of Adolescent Birth Rate:")
print(summary_abr)

print("Summary of CO2 Emissions:")
print(summary_co2)

# Save cleaned data to a new CSV file
write_csv(data, "cleaned_hdro_indicators_zwe.csv")

# Save plots as images
ggsave("Adolescent_Birth_Rate_Zimbabwe.png", width = 10, height = 6)
ggsave("CO2_Emissions_Zimbabwe.png", width = 10, height = 6)
