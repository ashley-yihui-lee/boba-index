library(tidyverse)
library(data.table)

setwd('~/desktop/boba-index')

# Load boba prices
df <- fread("boba-price.csv")

# No LA
df <- df[city != "Los Angeles"]

# Ensure columns are numeric
df[, local_price := as.numeric(local_price)]

# Convert local prices to USD
df[, dollar_price := local_price / dollar_ex]

# Get New York boba price in USD
ny_price <- df[city == "New York", dollar_price]

# Calculate the Implied Exchange Rate: how much local currency equals $1 USD 
df[, implied_exchange_rate := (local_price / ny_price)]

# Calculate percentage difference
df[, exchange_rate_diff := ((implied_exchange_rate - dollar_ex) / dollar_ex) * 100]

# Round for readability
df[, exchange_rate_diff := round(exchange_rate_diff, 2)]

# Save the output
fwrite(df, "boba-exchange-rates.csv")

# Print the first few rows
print(df)
library(ggplot2)
library(showtext)  # Load showtext for custom fonts

# Add Lora font
library(ggplot2)
library(showtext)  # Load showtext for custom fonts

# Add Lora font
font_add_google("Lora", "lora")  
showtext_auto()  # Enable showtext for ggplot

ggplot(df, aes(x = exchange_rate_diff, y = 0, color = ifelse(exchange_rate_diff > 0, "above", "below"))) +
  geom_point(size = 22, alpha = 0.7) +  
  geom_vline(xintercept = 0,color = "black", size =1) +  # Reference line at 0
  scale_color_manual(values = c("above" = "#f39800", "below" = "#762f07")) +  # Two-color mapping
    labs(
    x = NULL,
    y = NULL
    ) +
  theme_minimal() +
  theme(
    text = element_text(family = "lora", size=30),  # Apply Lora font
    axis.text.y = element_blank(),  # Remove y-axis labels
    axis.ticks.y = element_blank(),  # Remove y-axis ticks
    legend.position = "none",  # Remove legend
    panel.grid.major.y = element_blank(),  # Remove major horizontal grid lines
    panel.grid.minor.y = element_blank(),  # Remove minor horizontal grid lines
    aspect.ratio = 1/5  
  )

showtext_opts(dpi = 300)
ggsave("boba-index-laptop.png", width = 20, height = 10, dpi = 300)
