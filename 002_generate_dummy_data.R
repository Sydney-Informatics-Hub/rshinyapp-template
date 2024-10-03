library(geojsonsf)
library(sf)
library(janitor)
library(here)
library(dplyr)
library(qs)
library(stringr)

# Read the shapefile
postal_2021_reduced <- st_read(here::here("shapefiles", "POA_2021_AUST_GDA2020.json")) %>% janitor::clean_names()

# Filter for numeric postcodes and select relevant columns
numeric_postcodes <- postal_2021_reduced %>%
  filter(poa_code21 == poa_name21, str_starts(poa_code21, "2")) %>%
  select(poa_code21, geometry)  %>%
  filter(!st_is_empty(geometry))

# Create a dataframe of postcodes with colors
postcodes <- numeric_postcodes %>% 
  st_drop_geometry() %>%
  mutate(color = sample(colors(), n(), replace = TRUE))

postcodes$color <- sample(colors(), nrow(postcodes), replace = TRUE)

# Generate the fake dataset
set.seed(123)  # for reproducibility
postcodes_fake_dataset <- bind_rows(replicate(3, {
  postcodes %>%
    mutate(
      letters = sample(c("a", "b", "c"), n(), replace = TRUE),
      number = sample(1:1000, n(), replace = TRUE))
}, simplify = FALSE)) %>%
  left_join(numeric_postcodes, by = "poa_code21")

# Convert to sf object
postcodes_fake_dataset <- st_as_sf(postcodes_fake_dataset)

qsave(postcodes_fake_dataset, file="input_file.qs")
