library(tidyverse)

data_filtered = read.csv("data_clean.csv", check.names = FALSE, na.strings = "", fileEncoding = "UTF-8")

data_filtered |> 
  summarise(
    n_phd = sum(stage == "PhD student", na.rm = TRUE),
    n_postdoc = sum(stage == "Postdoctoral researcher", na.rm = TRUE),
    n_other = sum(stage == "Other", na.rm = TRUE)
  )

data_filtered |> 
  summarise(
    n_natural_sciences = sum(discipline_1 == "Natural sciences", na.rm = TRUE),
  )

data_filtered |> 
  summarise(
    n_aware_very = sum(awareness == "Very familiar", na.rm = TRUE),
  )

data_filtered |> 
  filter(!is.na(opportunity)) |>
  summarise(
    n_opportunity_strongly_agree = sum(opportunity == "Strongly agree", na.rm = TRUE),
    percent_opportunity_strongly_agree = n_opportunity_strongly_agree / n() * 100,
  )

data_filtered |> 
  filter(!is.na(capability)) |>
  summarise(
    n_capability_strongly_agree = sum(capability == "Perform research tasks independently, producing higher-quality results than competent human researchers.", na.rm = TRUE),
    percent_capability_strongly_agree = n_capability_strongly_agree / n() * 100,
  )

