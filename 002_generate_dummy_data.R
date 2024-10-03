library(geojsonsf)
library(sf)
library(janitor)
library(here)
library(dplyr)

postal_2021_reduced <- st_read(here::here("shapefiles", "POA_2021_AUST_GDA2020.json")) |> janitor::clean_names()

numeric_postcodes <- postal_2021_reduced |>
  filter(poa_code21 == poa_name21) |>
  select(-aus_code21, -aus_name21, -poa_name21)

postcodes <-   postal_2021_reduced |> 
  select(poa_code21) |> 
  st_drop_geometry() 

postcodes$color <- sample(colors(), nrow(postcodes), replace = TRUE)

postcodes_fake_dataset <-
  data.frame(
    poa_code21 = sample(postcodes$poa_code21, 3*nrow(postcodes), replace = TRUE),
    letters = sample(c("a", "b", "c"), 3*nrow(postcodes), replace = TRUE),
    number = sample(1:1000, 3*nrow(postcodes), replace = TRUE)
  ) |> left_join(postcodes, by = "poa_code21")