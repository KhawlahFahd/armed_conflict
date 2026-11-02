library(tidyverse)
library(here)

# Read the prepared dataset
conflict <- read_csv(
  here("data", "processed", "final_data.csv")
)

# Identify countries whose maternal mortality increased
# between 2000 and 2017
increased_countries <- conflict |>
  filter(year %in% c(2000, 2017)) |>
  select(iso, year, maternal_mortality) |>
  pivot_wider(
    names_from = year,
    values_from = maternal_mortality,
    names_prefix = "mmr_"
  ) |>
  filter(
    !is.na(mmr_2000),
    !is.na(mmr_2017),
    mmr_2017 > mmr_2000
  ) |>
  pull(iso)

# Select the full 2000–2017 trajectories
plot_data <- conflict |>
  filter(
    iso %in% increased_countries,
    between(year, 2000, 2017)
  )

# Figure 1: Maternal mortality by country
figure1 <- ggplot(
  plot_data,
  aes(
    x = year,
    y = maternal_mortality,
    color = iso,
    group = iso
  )
) +
  geom_line(linewidth = 0.9, na.rm = TRUE) +
  labs(
    title = "Maternal Mortality by Country, 2000–2017",
    subtitle = "Countries with higher maternal mortality in 2017 than in 2000",
    x = "Year",
    y = "Maternal deaths per 100,000 live births",
    color = "Country"
  ) +
  theme_minimal()

# Display Figure 1
figure1

