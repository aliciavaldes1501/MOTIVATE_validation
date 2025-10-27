#### Data prep ReSurvey points PNPE ###
data_V2025.2 <- read_tsv(
  "C:/Data/MOTIVATE/MOTIVATE_data/Data/EVA and ReSurvey Data/2025 data - V2025.2/Core data/200_Motivate20250415_notJUICE_header_with_precise_coordinates.csv")

RS_PNPE <- data_V2025.2 %>% 
  filter(`ReSurvey project` == "PICOS") %>%
  select(PlotObservationID, `ReSurvey site`, `ReSurvey plot`, 
         `ReSurvey observation`, `Date of recording`, Longitude, Latitude, 
         `Expert system`) %>% 
  arrange(`ReSurvey plot`, `ReSurvey observation`) %>%
  mutate(date = dmy(`Date of recording`), year = as.integer(year(date))) %>%
  mutate(
    # Clean 'Expert System' column by removing "!" and replacing "~" with NA
    EUNIS = case_when(
      `Expert system` == "~" ~ NA_character_,  # Replace "~" with NA
      TRUE ~ `Expert system`)
  ) %>%
  # Add columns for the different EUNIS levels
  mutate(
    # EUNIS levels
    EUNIS_1 = substr(EUNIS, 1, 1),
    EUNIS_2 = ifelse(nchar(EUNIS) >= 2, substr(EUNIS, 1, 2), NA_character_), 
    EUNIS_3 = ifelse(nchar(EUNIS) >= 3, substr(EUNIS, 1, 3), NA_character_)
    ) %>%
  select(- `Expert system`, - `Date of recording`) %>%
  pivot_wider()

print(RS_PNPE, width = Inf, n = 50)

# Export to shapefile

st_write(st_as_sf(RS_PNPE, coords = c("Longitude", "Latitude"), crs = 4326),
         here("data", "shp", "RS_PNPE.shp"))


