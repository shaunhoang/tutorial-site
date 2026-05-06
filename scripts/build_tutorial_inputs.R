# Rebuild the SAE-ready tutorial checkpoint files from raw tutorial data.

library(dplyr)
library(readr)
library(stringr)
library(tidyr)

source(file.path("scripts", "setup_data.R"))

project_root <- tutorial_project_root()
raw_dir <- file.path(project_root, "data", "raw")
clean_dir <- file.path(project_root, "data", "clean")
download_dir <- file.path(project_root, "downloads", "sae-ready-inputs")

dir.create(clean_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(download_dir, recursive = TRUE, showWarnings = FALSE)
standardise_raw_data_layout(raw_dir)

clean_nsw <- function(x) {
  ifelse(x %in% c(-99, -98, -9, -8, -7), NA, x)
}

nsw_raw <- read_csv(
  file.path(raw_dir, "nsw_2223_msoa_la.csv"),
  show_col_types = FALSE
) |>
  rename(unit_id = CaseNo)

survey_domains <- nsw_raw |>
  transmute(
    unit_id,
    la_domain_id = la_code,
    msoa_domain_id = msoa_code
  )

adult_domain_sizes_msoa <- read_csv(
  file.path(raw_dir, "census2021-ts007-msoa.csv"),
  show_col_types = FALSE
) |>
  filter(str_starts(`geography code`, "W")) |>
  transmute(
    domain_id = `geography code`,
    domain_name = geography,
    domain_size =
      `Age: Total; measures: Value` -
      `Age: Aged 4 years and under; measures: Value` -
      `Age: Aged 5 to 9 years; measures: Value` -
      `Age: Aged 10 to 15 years; measures: Value`
  )

ltla_raw <- file.path(raw_dir, "census2021-ts007-ltla.csv")
if (file.exists(ltla_raw)) {
  population_domain_sizes_la <- read_csv(ltla_raw, show_col_types = FALSE) |>
    filter(str_starts(`geography code`, "W")) |>
    transmute(
      domain_id = `geography code`,
      domain_name = geography,
      domain_size = `Age: Total; measures: Value`
    )
} else {
  population_domain_sizes_la <- read_csv(
    file.path(clean_dir, "domain_sizes_population_la.csv"),
    show_col_types = FALSE
  )
}

survey_indicators <- nsw_raw |>
  transmute(
    unit_id,
    commute_distance = as.numeric(clean_nsw(TravWkDist)),
    poor_bus_satisfaction = case_when(
      Bus12M == 2 ~ 0L,
      clean_nsw(BusOverSat) %in% c(4, 5) ~ 1L,
      is.na(clean_nsw(BusOverSat)) ~ NA_integer_,
      TRUE ~ 0L
    ),
    walk_weekly = if_else(clean_nsw(AtFrqWlk10) %in% 1:3, 1L, 0L)
  )

survey_weights <- nsw_raw |>
  transmute(
    unit_id,
    SampleAdultWeight,
    WalesAdultWeight,
    SampleTravelWeight
  ) |>
  left_join(select(survey_domains, unit_id, la_domain_id), by = "unit_id") |>
  left_join(
    select(population_domain_sizes_la, la_domain_id = domain_id, domain_size),
    by = "la_domain_id"
  ) |>
  group_by(la_domain_id) |>
  mutate(
    WalesTravelWeight = SampleTravelWeight * (domain_size / sum(SampleTravelWeight, na.rm = TRUE))
  ) |>
  ungroup() |>
  select(unit_id, SampleAdultWeight, WalesAdultWeight, WalesTravelWeight)

survey_inputs <- survey_domains |>
  left_join(survey_indicators, by = "unit_id") |>
  left_join(survey_weights, by = "unit_id")

indicator_map <- tibble::tribble(
  ~indicator,              ~id_col,          ~weight,
  "commute_distance",      "msoa_domain_id", "SampleAdultWeight",
  "poor_bus_satisfaction", "msoa_domain_id", "WalesAdultWeight",
  "walk_weekly",           "la_domain_id",   "WalesTravelWeight"
)

write_csv(adult_domain_sizes_msoa, file.path(clean_dir, "domain_sizes_adult_msoa.csv"))
write_csv(population_domain_sizes_la, file.path(clean_dir, "domain_sizes_population_la.csv"))

for (i in seq_len(nrow(indicator_map))) {
  spec <- indicator_map[i, ]

  indicator_file <- survey_inputs |>
    transmute(
      unit_id,
      domain_id = .data[[spec$id_col]],
      indicator = .data[[spec$indicator]],
      weight = .data[[spec$weight]]
    )

  write_csv(
    indicator_file,
    file.path(clean_dir, str_c("ind_weight_", spec$indicator, ".csv"))
  )
}

checkpoint_files <- c(
  "domain_sizes_adult_msoa.csv",
  "domain_sizes_population_la.csv",
  str_c("ind_weight_", indicator_map$indicator, ".csv")
)

file.copy(
  file.path(clean_dir, checkpoint_files),
  file.path(download_dir, checkpoint_files),
  overwrite = TRUE
)

writeLines(
  c(
    "file,description",
    "domain_sizes_adult_msoa.csv,Adult population aged 16+ by MSOA",
    "domain_sizes_population_la.csv,Total resident population by Local Authority",
    "ind_weight_commute_distance.csv,Survey indicator and weight file for commute distance by MSOA",
    "ind_weight_poor_bus_satisfaction.csv,Survey indicator and weight file for poor bus satisfaction by MSOA",
    "ind_weight_walk_weekly.csv,Survey indicator and weight file for weekly walking by Local Authority"
  ),
  file.path(download_dir, "manifest.csv")
)

message("Wrote SAE-ready tutorial inputs to ", clean_dir)
message("Copied downloadable checkpoints to ", download_dir)
