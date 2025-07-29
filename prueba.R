# Bea's filter
trial2 <- read_csv("C:/Data/MOTIVATE/MOTIVATE_RS_data/Landsat/Bands/all/ALPALPS_0_Landsat_RawBands.csv") %>%
  rename(PlotObservationID = PltObID)
# New filter
trial2 <- read_csv("C:/Data/MOTIVATE/MOTIVATE_RS_data/Landsat/Bands_new/ALPALPS_0_Landsat_RawBands.csv") %>%
  rename(PlotObservationID = PltObID)
# No filter
trial3 <- read_csv("C:/Data/MOTIVATE/MOTIVATE_RS_data/Landsat/Bands_unfiltered/ALPALPS_0_Landsat_RawBands.csv") %>%
  rename(PlotObservationID = PltObID)
  
trial2 %>% dplyr::filter(year != year(date))
trial2 %>% dplyr::filter(year != year(date))
trial3 %>% dplyr::filter(year != year(date))

################### trial1 ######################

# Summarize the band values conditionally
band_summary1 <- trial1 %>%
  group_by(PlotObservationID, date) %>%
  summarise(
    n_images = n_distinct(image_id),
    Blue = if (n_images > 1) mean(Blue, na.rm = TRUE) else first(Blue),
    Green  = if (n_images > 1) mean(Green,  na.rm = TRUE) else first(Green),
    NIR  = if (n_images > 1) mean(NIR,  na.rm = TRUE) else first(NIR),
    Red  = if (n_images > 1) mean(Red,  na.rm = TRUE) else first(Red),
    SWIR1 = if (n_images > 1) mean(SWIR1,  na.rm = TRUE) else first(SWIR1),
    .groups = "drop"
  ) 

# Calculate how many different days for each PlotObservationID
n_days1 <- band_summary1 %>%
  group_by(PlotObservationID) %>%
  summarise(n_days = n_distinct(date))

# Join back to original data
trial1_updated <- trial1 %>%
  # Remove old band values
  select(-Blue, -Green, -NIR, -Red, -SWIR1) %>%
  # Join band_summary
  left_join(band_summary1, by = c("PlotObservationID", "date")) %>%
  # Keep one row per group
  distinct(PlotObservationID, date, .keep_all = TRUE) %>%
  # Remove unwanted columns
  select(-`system:index`, -image_id, -.geo, -time_utc, -timestamp) %>%
  # Join
  left_join(n_days1)

# Calculate indices
trial1_indices <- trial1_updated %>%
  # Set PlotObservationID as factor
  mutate(PlotObservationID = factor(PlotObservationID)) %>%
  # Rename the bands
  rename(blue = Blue, green = Green, red = Red, NIR = NIR, SWIR = SWIR1) %>%
  # Scale the bands
  mutate(blue = blue / 10000, green = green / 10000, red = red / 10000,
         NIR = NIR / 10000, SWIR = SWIR / 10000) %>%
  # Create column that combines the day of the month and the time
  mutate(
    date = as.POSIXct(date),
    # Normalize the dates to a fixed year (2000)
    # so that seasonal patterns across different years can be compared visually
    day_month = as.POSIXct(format(date, "2000-%m-%d"))) %>%
  # Create column with DOY
  mutate(DOY = yday(date)) %>%
  # Calculate NDVI
  mutate(NDVI = (NIR - red) / (NIR + red),
         EVI = (NIR - red) * 2.5 / (NIR + 6 * red - 7.5 * blue + 1),
         SAVI = (NIR - red) * 1.5 / (NIR + red + 0.5),
         NDMI = (NIR - SWIR) / (NIR + SWIR),
         NDWI = (green - NIR) / (green + NIR))

################### trial2 ######################

# Summarize the band values conditionally
band_summary2 <- trial2 %>%
  group_by(PlotObservationID, date) %>%
  summarise(
    n_images = n_distinct(image_id),
    Blue = if (n_images > 1) mean(Blue, na.rm = TRUE) else first(Blue),
    Green  = if (n_images > 1) mean(Green,  na.rm = TRUE) else first(Green),
    NIR  = if (n_images > 1) mean(NIR,  na.rm = TRUE) else first(NIR),
    Red  = if (n_images > 1) mean(Red,  na.rm = TRUE) else first(Red),
    SWIR1 = if (n_images > 1) mean(SWIR1,  na.rm = TRUE) else first(SWIR1),
    .groups = "drop"
  ) 

# Calculate how many different days for each PlotObservationID
n_days2 <- band_summary2 %>%
  group_by(PlotObservationID) %>%
  summarise(n_days = n_distinct(date))

# Join back to original data
trial2_updated <- trial2 %>%
  # Remove old band values
  select(-Blue, -Green, -NIR, -Red, -SWIR1) %>%
  # Join band_summary
  left_join(band_summary2, by = c("PlotObservationID", "date")) %>%
  # Keep one row per group
  distinct(PlotObservationID, date, .keep_all = TRUE) %>%
  # Remove unwanted columns
  select(-`system:index`, -image_id, -.geo, -time_utc, -timestamp) %>%
  # Join
  left_join(n_days2)

# Calculate indices
trial2_indices <- trial2_updated %>%
  # Set PlotObservationID as factor
  mutate(PlotObservationID = factor(PlotObservationID)) %>%
  # Rename the bands
  rename(blue = Blue, green = Green, red = Red, NIR = NIR, SWIR = SWIR1) %>%
  # Scale the bands
  mutate(blue = blue / 10000, green = green / 10000, red = red / 10000,
         NIR = NIR / 10000, SWIR = SWIR / 10000) %>%
  # Create column that combines the day of the month and the time
  mutate(
    date = as.POSIXct(date),
    # Normalize the dates to a fixed year (2000)
    # so that seasonal patterns across different years can be compared visually
    day_month = as.POSIXct(format(date, "2000-%m-%d"))) %>%
  # Create column with DOY
  mutate(DOY = yday(date)) %>%
  # Calculate NDVI
  mutate(NDVI = (NIR - red) / (NIR + red),
         EVI = (NIR - red) * 2.5 / (NIR + 6 * red - 7.5 * blue + 1),
         SAVI = (NIR - red) * 1.5 / (NIR + red + 0.5),
         NDMI = (NIR - SWIR) / (NIR + SWIR),
         NDWI = (green - NIR) / (green + NIR))

################### trial3 ######################

# Summarize the band values conditionally
band_summary3 <- trial3 %>%
  group_by(PlotObservationID, date) %>%
  summarise(
    n_images = n_distinct(image_id),
    Blue = if (n_images > 1) mean(Blue, na.rm = TRUE) else first(Blue),
    Green  = if (n_images > 1) mean(Green,  na.rm = TRUE) else first(Green),
    NIR  = if (n_images > 1) mean(NIR,  na.rm = TRUE) else first(NIR),
    Red  = if (n_images > 1) mean(Red,  na.rm = TRUE) else first(Red),
    SWIR1 = if (n_images > 1) mean(SWIR1,  na.rm = TRUE) else first(SWIR1),
    .groups = "drop"
  ) 

# Calculate how many different days for each PlotObservationID
n_days3 <- band_summary3 %>%
  group_by(PlotObservationID) %>%
  summarise(n_days = n_distinct(date))

# Join back to original data
trial3_updated <- trial3 %>%
  # Remove old band values
  select(-Blue, -Green, -NIR, -Red, -SWIR1) %>%
  # Join band_summary
  left_join(band_summary3, by = c("PlotObservationID", "date")) %>%
  # Keep one row per group
  distinct(PlotObservationID, date, .keep_all = TRUE) %>%
  # Remove unwanted columns
  select(-`system:index`, -image_id, -.geo, -time_utc, -timestamp) %>%
  # Join
  left_join(n_days3)

# Calculate indices
trial3_indices <- trial3_updated %>%
  # Set PlotObservationID as factor
  mutate(PlotObservationID = factor(PlotObservationID)) %>%
  # Rename the bands
  rename(blue = Blue, green = Green, red = Red, NIR = NIR, SWIR = SWIR1) %>%
  # Scale the bands
  mutate(blue = blue / 10000, green = green / 10000, red = red / 10000,
         NIR = NIR / 10000, SWIR = SWIR / 10000) %>%
  # Create column that combines the day of the month and the time
  mutate(
    date = as.POSIXct(date),
    # Normalize the dates to a fixed year (2000)
    # so that seasonal patterns across different years can be compared visually
    day_month = as.POSIXct(format(date, "2000-%m-%d"))) %>%
  # Create column with DOY
  mutate(DOY = yday(date)) %>%
  # Calculate NDVI
  mutate(NDVI = (NIR - red) / (NIR + red),
         EVI = (NIR - red) * 2.5 / (NIR + 6 * red - 7.5 * blue + 1),
         SAVI = (NIR - red) * 1.5 / (NIR + red + 0.5),
         NDMI = (NIR - SWIR) / (NIR + SWIR),
         NDWI = (green - NIR) / (green + NIR))

############################################################################

# Number of different dates

trial1_indices %>% distinct(PlotObservationID, date) %>% 
  count(PlotObservationID) %>%
  ggplot(aes(x = n)) + geom_histogram(color = "black", fill = "white")

trial2_indices %>% distinct(PlotObservationID, date) %>% 
  count(PlotObservationID) %>%
  ggplot(aes(x = n)) + geom_histogram(color = "black", fill = "white")

trial3_indices %>% distinct(PlotObservationID, date) %>% 
  count(PlotObservationID) %>%
  ggplot(aes(x = n)) + geom_histogram(color = "black", fill = "white")

# trial1 and trial2 similar, trial3 different

# Number of different months

trial1_indices %>% distinct(PlotObservationID, date) %>% 
  mutate(month = month(date)) %>%
  distinct(PlotObservationID, month) %>%
  count(PlotObservationID) %>%
  ggplot(aes(x = n)) + geom_histogram(color = "black", fill = "white")

trial2_indices %>% distinct(PlotObservationID, date) %>% 
  mutate(month = month(date)) %>%
  distinct(PlotObservationID, month) %>%
  count(PlotObservationID) %>%
  ggplot(aes(x = n)) + geom_histogram(color = "black", fill = "white")

trial3_indices %>% distinct(PlotObservationID, date) %>% 
  mutate(month = month(date)) %>%
  distinct(PlotObservationID, month) %>%
  count(PlotObservationID) %>%
  ggplot(aes(x = n)) + geom_histogram(color = "black", fill = "white")

# trial1 and trial2 similar, trial3 different
