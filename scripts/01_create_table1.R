library(tidyverse)
library(here)
library(gtsummary)

# Read the final dataset
conflict <- read_csv(
  here("data", "processed", "final_data.csv")
)

# Check the data
glimpse(conflict)

# Total observations
nrow(conflict)

# Count observations by armed conflict
conflict |>
  count(armed_conflict, .drop = FALSE)

# Check missing values
conflict |>
  summarise(across(everything(), ~sum(is.na(.))))

# Prepare variables for Table 1
table1_data <- conflict |>
  mutate(
    armed_conflict = factor(
      armed_conflict,
      levels = c(0, 1),
      labels = c("No conflict", "Armed conflict")
    ),
    oecd = factor(
      oecd,
      levels = c(0, 1),
      labels = c("No", "Yes")
    ),
    earthquake = factor(
      earthquake,
      levels = c(0, 1),
      labels = c("No", "Yes")
    ),
    drought = factor(
      drought,
      levels = c(0, 1),
      labels = c("No", "Yes")
    )
  )

# Create Table 1
table1 <- table1_data |>
  select(
    armed_conflict,
    maternal_mortality,
    infant_mortality,
    under5_mortality,
    neonatal_mortality,
    gdp_1000,
    oecd,
    pop_dens,
    urban,
    age_dep,
    male_edu,
    temp,
    rainfall_1000,
    earthquake,
    drought
  ) |>
  tbl_summary(
    by = armed_conflict,
    type = list(
      all_continuous() ~ "continuous",
      c(oecd, earthquake, drought) ~ "dichotomous"
    ),
    value = list(
      oecd ~ "Yes",
      earthquake ~ "Yes",
      drought ~ "Yes"
    ),
    statistic = list(
      all_continuous() ~ "{mean} ({sd})",
      all_dichotomous() ~ "{n} ({p}%)"
    ),
    digits = all_continuous() ~ 1,
    missing = "always",
    missing_text = "Missing",
    missing_stat = "{N_miss}",
    label = list(
      maternal_mortality ~ "Maternal mortality",
      infant_mortality ~ "Infant mortality",
      under5_mortality ~ "Under-five mortality",
      neonatal_mortality ~ "Neonatal mortality",
      gdp_1000 ~ "GDP per capita (thousands)",
      oecd ~ "OECD membership",
      pop_dens ~ "Population density",
      urban ~ "Urban population",
      age_dep ~ "Age dependency ratio",
      male_edu ~ "Male education",
      temp ~ "Temperature",
      rainfall_1000 ~ "Rainfall (thousands)",
      earthquake ~ "Earthquake",
      drought ~ "Drought"
    )
  ) |>
  add_overall(last = FALSE) |>
  modify_header(label ~ "**Characteristic**") |>
  bold_labels()

# Display Table 1
table1
