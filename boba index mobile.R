library(tidyverse)
library(data.table)
library(ggplot2)
library(showtext)
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

# Add Lora font
font_add_google("Lora", "lora")  
showtext_auto()  # Enable showtext for ggplot

ggplot(df, aes(x = 0, y = exchange_rate_diff, color = ifelse(exchange_rate_diff > 0, "above", "below"))) +  
  geom_point(size = 12, alpha = 0.7) +  
  geom_hline(yintercept = 0, color = "black", size = 1) +  # Reference line at 0
  scale_color_manual(values = c("above" = "#f39800", "below" = "#762f07")) +  # Two-color mapping
  labs(
    x = NULL,
    y = NULL,
    color = "Exchange Rate Difference (%)"
  ) +
  theme_minimal() +
  theme(
    text = element_text(family = "lora", color = "black", size = 15),  
    axis.text.x = element_blank(),
    axis.text.y = element_text(size = 15),
    legend.position = "none",  # Remove legend
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    aspect.ratio = 5/1  
  )


showtext_opts(dpi = 300)
ggsave("boba-index-mobile.png", width = 6, height = 12, dpi = 300, units = "in")
