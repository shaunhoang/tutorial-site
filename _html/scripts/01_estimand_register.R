# Template for creating an SAE estimand register.

library(tibble)

estimand_register <- tribble(
  ~indicator, ~outcome_rule, ~target_population, ~domain, ~domain_size,
  ~weight, ~routing, ~interpretation,
  "commute_multimodal",
  "row sum of TravWkModeAll1:TravWkModeAll8 >= 2",
  "adults_16plus",
  "local_authority",
  "pop_16plus",
  "WalesAdultWeight",
  "routed travel-to-work item",
  "Share of adults who usually use two or more modes to travel to work",
  "poor_bus_satisfaction",
  "BusOverSat %in% c(4, 5)",
  "adults_16plus",
  "local_authority",
  "pop_16plus",
  "WalesAdultWeight",
  "asked in the public transport satisfaction section",
  "Share of adults dissatisfied with bus services",
  "walk_weekly",
  "AtFrqWlk10 %in% c(1, 2, 3)",
  "adults_16plus",
  "local_authority",
  "pop_16plus",
  "WalesTravelWeight",
  "active travel module",
  "Share of adults who walk for transport at least weekly"
)

estimand_register
