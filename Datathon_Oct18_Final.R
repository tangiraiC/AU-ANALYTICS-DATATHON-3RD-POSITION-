# Datathon - Analytics Club
# October 18th

setwd("C:/Users/Gaby/OneDrive/Documentos/AU/Fall 2024/ITEC-610/Lab/Data");

### Libraries

library(tidyverse)
library(e1071)
library(DescTools)
library(pwr)
library(car)
library(dplyr)
library(summarytools)
library(ggplot2)
library(lsr)
library(stargazer)

### IMPORT DATASET

# Metro data
metro <- read.csv("metro data.csv")
glimpse(metro)

# Economics data
econ <- read.csv("Worlwide Economics Dataset.csv")
glimpse(econ)

# Missing Value
sum(is.na(metro))
sum(is.na(econ$GDP))
sum(is.na(country))

# Duplicates
sum(duplicated(metro))
sum(duplicated(country))

# Clean ridership column 
metro$annual_ridership = substr(metro$Annual.ridership..millions.,1,nchar(metro$Annual.ridership..millions.)-6)
metro$annual_ridership <- as.numeric(metro$annual_ridership)
metro$annual_ridership_2 <- ifelse(is.na(metro$annual_ridership), mean(metro$annual_ridership, na.rm = TRUE), metro$annual_ridership)

# Clean the 'Annual Ridership' column by extracting numeric values and years
metro$Annual_Ridership <- as.numeric(gsub("[^0-9.]", "", metro$Annual.ridership..millions.))
metro$Ridership_Year <- as.numeric(gsub(".*\\((\\d{4})\\).*", "\\1", metro$Annual.ridership..millions.))

# Remove rows with missing values
metro <- metro %>%
  filter(!is.na(Annual_Ridership) & !is.na(Ridership_Year))

format(metro$Annual_Ridership, scientific = FALSE, nsmall = 2)
format(metro$Ridership_Year, scientific = FALSE, nsmall = 2)

# Subset the data to include only years over 5 years
recent_years <- metro %>%
  filter(Ridership_Year >= (max(Ridership_Year) - 5))

# Summarize ridership trends by year
annual_ridership_trends <- recent_years %>%
  group_by(Ridership_Year) %>%
  summarize(Total_Ridership = sum(Annual_Ridership, na.rm = TRUE))

# Convert the Total Ridership to decimal format
annual_ridership_trends$Total_Ridership <- format(annual_ridership_trends$Total_Ridership, scientific = FALSE, nsmall = 2)

# Plot the annual ridership trends
ggplot(annual_ridership_trends, aes(x = Ridership_Year, y = Total_Ridership)) +
  geom_line(color = "blue", size = 1.5) +
  labs(title = "Metro System Annual Ridership Trends (Last 5 Years)", x = "Year", y = "Total Ridership (Millions)") +
  theme_minimal()


#TOP 10 METRO SYSTEMS
# Clean the 'Annual Ridership' column by extracting numeric values
metro$Annual_Ridership <- as.numeric(gsub("[^0-9.]", "", metro$Annual.ridership..millions.))
metro$Ridership_Year <- as.numeric(gsub(".*\\((\\d{4})\\).*", "\\1", metro$Annual.ridership..millions.))

# Remove rows with missing values
metro <- metro %>%
  filter(!is.na(Annual_Ridership))

# Identify the top 10 metro systems by ridership
top_10_metros <- metro %>%
  arrange(desc(Annual_Ridership)) %>%
  head(10)

# Plot the top 10 metro systems by ridership
ggplot(top_10_metros, aes(x = reorder(Name, Annual_Ridership), y = Annual_Ridership)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  coord_flip() +
  labs(title = "Top 10 Metro Systems by Ridership", x = "Metro System", y = "Annual Ridership (Millions)") +
  theme_minimal()

# View the top 10 metro systems
print(top_10_metros)


# METRO SYSTEM GROWTH OVER TIME
df <- read.csv("metro data.csv")
df_clean <- df %>%
  select(City, Service.opened, Last.expanded, System.length) %>%
  mutate(
    # Convert 'Service.opened' and 'Last.expanded' to numeric years
    Service.opened = as.numeric(gsub("[^0-9]", "", Service.opened)),
    Last.expanded = as.numeric(gsub("[^0-9]", "", Last.expanded)),
    
    System.length = as.numeric(gsub("[^0-9.]", "", System.length))
  )
df_clean$Last.expanded[is.na(df_clean$Last.expanded)] <- df_clean$Service.opened[is.na(df_clean$Last.expanded)]

df_clean <- df_clean %>% na.omit()

df_growth <- df_clean %>%
  arrange(Service.opened) %>%
  mutate(cumulative_length = cumsum(System.length))

ggplot(df_growth, aes(x = Service.opened, y = cumulative_length)) +
  geom_line(color = "blue", group = 1) +  # Ensures all data points are part of the same group
  geom_point() +
  labs(
    title = "Cumulative Metro System Growth Over Time",
    x = "Year",
    y = "Cumulative Metro Length (km)"
  ) +
  theme_minimal()

# DISTRIBUTION OF METRO LENGHTS

metro <- metro %>%
  separate(col = `System.length`, 
           into = c("System_length_km", " system length miles "), 
           sep = "km" )

metro$System_length_km <- gsub("[^0-9.]", "", metro$System_length_km)
metro$System_length_km <- as.numeric(metro$System_length_km)

metro$System_length_miles <- metro$System_length_km*0.621371

ggplot(metro, aes(x = System_length_km)) +
  geom_histogram(binwidth = 10, fill = "blue", color = "black", alpha = 0.7) +
  labs(title = "Distribution of Metro System Lengths Globally", 
       x = "Metro Length (km)", 
       y = "Frequency") +
  theme_minimal()

descr(metro$System_length_km)

# LENGTH VS RIDERSHIP

# Assuming the correct column is 'Annual ridership(millions)', split the column
metro$split_data <- strsplit(as.character(metro$Annual.ridership..millions.), " ")

# Extract ridership (without commas) and year
metro$Ridership <- as.numeric(gsub(",", "", sapply(metro$split_data, "[", 1)))
metro$Year <- gsub("[()]", "", sapply(metro$split_data, "[", 2))

metro <- na.omit(metro)

# Create a scatter plot to visualize the relationship
ggplot(metro, aes(x = System_length_miles, y = Ridership)) +
  geom_point(color = "blue", size = 3, alpha = 0.6) +
  labs(title = "Relationship Between System Length and Annual Ridership",
       x = "System Length (Miles)",
       y = "Annual Ridership (+000)") +
  theme_minimal()


# Clean ridership column 
metro$annual_ridership = substr(metro$Annual.ridership..millions.,1,nchar(metro$Annual.ridership..millions.)-6)
metro$annual_ridership <- as.numeric(metro$annual_ridership)
metro$annual_ridership_2 <- ifelse(is.na(metro$annual_ridership), mean(metro$annual_ridership, na.rm = TRUE), metro$annual_ridership)


### % Countries with metro system

freq(metro$Country.region)
distinct(metro, Country.region) # there are 62 countries with at least one metro 

### Developed vs. developing countries

metro <- rename(metro, Country = Country.region)
metro_grouped <- metro |> count(Country)

# Determine who is developed
econ$GDP_PPC <- econ$GDP/econ$Population
econ$Developed <- ifelse(econ$GDP_PPC > 13.2, 1, 0)

# Include a "has_metro" dichotomous variable
econ_has_metro <- left_join(econ, metro_grouped, by = "Country")
econ_has_metro$n <- ifelse(is.na(econ_has_metro$n), 0, econ_has_metro$n)
econ_has_metro$has_metro <- ifelse(econ_has_metro$n == 0, 0, 1)

# Make graph
summary_data <- econ_has_metro %>%
  group_by(Developed, has_metro) %>%
  summarise(count = n()) %>%
  ungroup()

summary_data2 <- summary_data %>%
  group_by(Developed) %>%
  mutate(proportion = count / sum(count)) %>%
  ungroup()

ggplot(summary_data2, aes(x = factor(Developed), y = proportion, fill = factor(has_metro))) +
  geom_bar(stat = "identity", position = "fill") +  # Use stat = "identity" to plot the proportions
  scale_fill_manual(values = c("0" = "lightgray", "1" = "blue"),
                    labels = c("No Metro", "Has Metro")) +
  labs(title = "Proportion of Countries with Metro Systems by Development Status",
       x = "Development Status",
       y = "Proportion of Countries",
       fill = "Metro Status") +
  scale_x_discrete(labels = c("0" = "Developing", "1" = "Developed")) +
  theme_minimal() +  # Keeps the minimal theme
  theme(
    panel.grid.major = element_blank(),  # Remove major grid lines
    panel.grid.minor = element_blank(),  # Remove minor grid lines
    panel.background = element_blank(),  # Remove background
    axis.line = element_line(colour = "grey")  # Optional: add axis lines back
  )

### Developed vs Developing ridership

metro_group2 <- metro %>%
  group_by(Country) %>%
  summarise(total_stations = sum(Stations),
            total_rider = sum(annual_ridership_2)) %>%
  ungroup()

metro_gdp <- left_join(metro_group2, econ, by = "Country")
metro_gdp$Developed <- ifelse(is.na(metro_gdp$Developed), 0, metro_gdp$Developed)
metro_gdp_grouped <- metro_gdp |> group_by(Developed) |> summarise(total_rider = sum(total_rider))
metro_gdp_grouped$total_rider_2 <- metro_gdp_grouped$total_rider/1000
metro_gdp_grouped$Developed_2 <- ifelse(metro_gdp_grouped$Developed == 1, "Developed", "Developing")


ggplot(metro_gdp_grouped, aes(x = Developed_2, y = total_rider_2)) +
  geom_bar(stat = "identity", fill = "blue") +  # stat="identity" for manual y values
  labs(title = "Annual Ridership by Developed/Developing Countries", x = "Development Status", y = "Annual Ridership (Billions)") +
  theme_minimal() +
  theme(
    panel.grid.major = element_blank(),  # Remove major grid lines
    panel.grid.minor = element_blank(),  # Remove minor grid lines
    panel.background = element_blank(),  # Remove background
    axis.line = element_line(colour = "grey")  # Optional: add axis lines back
  )


### Regression

metro_reg <- lm(metro_gdp$total_rider ~ metro_gdp$total_stations + metro_gdp$GDP + metro_gdp$Gov..Budget + metro_gdp$Population + metro_gdp$Jobless.rate, data = metro_gdp)
summary(metro_reg)

stargazer(metro_reg, type = "html", out = "datathon.html")

vif(metro_reg)

0.9238/(1-0.9238)  

pwr.f2.test(u = 5,
            v = 55,
            f2 = 12.1,
            sig.level = 0.05,
            power = NULL)
