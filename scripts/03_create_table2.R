
library(here)
library(plm)
library(dplyr)
library(gtsummary)
finaldata <- read.csv(here("data", "processed", "final_data.csv"))


lmmod <- lm(maternal_mortality ~ -1 + armed_conflict + gdp_1000 + oecd + pop_dens + urban +
              age_dep + male_edu + temp + rainfall_1000 + earthquake + drought +
              iso, 
            data = finaldata)


plmmod <- plm(maternal_mortality ~ armed_conflict + gdp_1000 + oecd + pop_dens + urban +
              age_dep + male_edu + temp + rainfall_1000 + earthquake + drought,
              index = c("iso"), model = "within", data = finaldata)

summary(lmmod)
summary(plmmod)


#fixed effects model
lmmod <- lm(maternal_mortality ~ -1 + armed_conflict + gdp_1000 + oecd + pop_dens + urban +
              age_dep + male_edu + temp + rainfall_1000 + earthquake + drought +
              iso + as.factor(year), 
            data = finaldata)

plmmod <- plm(maternal_mortality ~ armed_conflict + gdp_1000 + oecd + pop_dens + urban +
              age_dep + male_edu + temp + rainfall_1000 + earthquake + drought,
              index = c("iso", "year"),
              effect = "twoways",
              model = "within",
              data = finaldata)


summary(lmmod)
summary(plmmod)

#log-transfrom GDP per capita variable 

finaldata <- finaldata|> 
    mutate(log_gdp = log(gdp_1000))


# fitting the four models
# setting up predictores (using log_gdp)
preds <- as.formula(" ~ armed_conflict + log_gdp + oecd + pop_dens + urban +age_dep + male_edu + temp + rainfall_1000 + earthquake + drought")


# define function to use for two-way fixed effects
fit_model <- function(outcome) {
    formula <- update.formula(
        preds, 
        paste(outcome, "~ .")
    )
    plm(
        formula, 
        index =c("iso", "year"),
        effect = "twoways", 
        model = "within",
        data = finaldata
    )
}

# fitting the models
maternal_mortalitymod <- fit_model("maternal_mortality")
un5mormod <- fit_model("under5_mortality")
infmormod <- fit_model("infant_mortality")
neomormod <- fit_model("neonatal_mortality")

# Create Table 2
table2 <- tbl_merge(
  tbls = list(
    tbl_regression(maternal_mortalitymod),
    tbl_regression(un5mormod),
    tbl_regression(infmormod),
    tbl_regression(neomormod)
  ),
  tab_spanner = c(
    "**Maternal mortality**",
    "**Under-5 mortality**",
    "**Infant mortality**",
    "**Neonatal mortality**"
  )
) |>
  modify_caption(
    "**Table 2. Association Between Armed Conflict and Mortality Outcomes**"
  ) |>
  bold_labels()

# Display Table 2
table2
