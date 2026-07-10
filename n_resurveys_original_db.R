# N resurveys from original db

PlotObservationIDs <- data_validated_clean %>% distinct(PlotObservationID) %>% pull(PlotObservationID)
data_V2025.2_sel <- data_V2025.2 %>%
  mutate(plotID = paste(RS_CODE, `ReSurvey site`, `ReSurvey plot`, sep = "-")) %>%
  # Keep only observations in the validation data
  filter(PlotObservationID %in% PlotObservationIDs) %>%
  mutate(year = as.integer(year(dmy(`Date of recording`)))) %>%
  group_by(plotID) %>%
  mutate(
    n_resurveys = n_distinct(year, na.rm = TRUE),   # count unique years
    first_year  = min(year, na.rm = TRUE),          # earliest year
    last_year   = max(year, na.rm = TRUE)           # latest year
  ) %>%
  ungroup()

plot_resurveys <-full_join(
  # Full dataset
  full_dataset_months  %>%
    group_by(plotID) %>%
    mutate(
      n_resurveys = n_distinct(year, na.rm = TRUE),   # count unique years
      first_year  = min(year, na.rm = TRUE),          # earliest year
      last_year   = max(year, na.rm = TRUE)           # latest year
    ) %>%
    ungroup() %>%
    distinct(plotID, n_resurveys, first_year, last_year) %>%
    rename(n_resurveys_full = n_resurveys, first_year_full = first_year, 
           last_year_full = last_year),
  # Original data
  data_V2025.2_sel %>%
    distinct(plotID, n_resurveys, first_year, last_year) %>%
    rename(n_resurveys_orig = n_resurveys, first_year_orig = first_year, 
           last_year_orig = last_year)
) %>%
  mutate(prop_resurveys_in_full = n_resurveys_full / n_resurveys_orig)

plot_resurveys %>% filter(n_resurveys_orig != n_resurveys_full)
plot_resurveys %>% filter(first_year_orig != first_year_full)
plot_resurveys %>% filter(last_year_orig != last_year_full)

PlotObservationIDs_full_dataset <- full_dataset_months %>% pull(PlotObservationID)
data_validated_clean <- data_validated_clean %>%
  mutate(in_full_dataset = ifelse(
    PlotObservationID %in% PlotObservationIDs_full_dataset, TRUE, FALSE))

data_validated_clean %>%
  count(plotID, in_full_dataset) %>%
  pivot_wider(
    names_from = in_full_dataset,
    values_from = n,
    values_fill = 0,
    names_prefix = "n_"
  ) %>%
  filter(n_FALSE != 0 & n_TRUE != 0) %>%
  nrow()