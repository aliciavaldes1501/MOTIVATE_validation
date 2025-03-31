# NDVI

percentiles <- db_resurv_RS_short %>%
  filter(EUNISa_1 %in% c("T", "R", "S", "Q")) %>%
  group_by(EUNISa_1) %>%
  summarize(percentile_20 = quantile(NDVI, probs = 0.20, na.rm = T))

db_resurv_RS_short <- db_resurv_RS_short %>%
  filter(EUNISa_1 %in% c("T", "R", "S", "Q")) %>%
  left_join(percentiles, by = "EUNISa_1") %>%
  mutate(color_category = case_when(
    NDVI < percentile_20 ~ "below_20th",
    NDVI >= percentile_20 ~ "above_20th"
  ))

ggplot(data = db_resurv_RS_short %>%
         mutate(wrong = as.factor(case_when(
           EUNISa_1 == "T" & NDVI < 0.8 ~ "wrong",
           EUNISa_1 == "R" & NDVI < 0.3 ~ "wrong",
           EUNISa_1 == "S" & NDVI < 0.6 ~ "wrong",
           EUNISa_1 == "Q" & NDVI < 0.2 ~ "wrong",
           TRUE ~ "not_wrong"
         ))),
       aes(x = EUNISa_1_descr, y = NDVI)) +
  geom_flat_violin(position = position_nudge(x = 0.2, y = 0), alpha = 0.8,
                   fill = "lightblue") +
  geom_point(aes(color = color_category),
             position = position_jitter(width = 0.15), size = 1, alpha = 0.25) +
  geom_boxplot(width = 0.2, outlier.shape = NA, alpha = 0.5) +
  stat_summary(fun.y = mean, geom = "point", shape = 20, size = 1) +
  stat_summary(fun.data = function(x) data.frame(y = max(x) + 0.1, label = length(x)),
               geom = "text", aes(label = ..label..), vjust = 0.5) +
  labs(y = "Maximum NDVI in July of the sampling year", x = "EUNIS level 1") +
  guides(fill = FALSE, color = FALSE) +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 15)) +
  scale_color_manual(values = c("below_20th" = "grey", "above_20th" = "lightblue")) +
  theme_bw() + coord_flip()
ggsave(
  here("output", "figures", "NDVI_small.tiff"),
  width = 15, height = 11, units = "cm", dpi = 300)

# NDVI bioregions

ggplot(data = db_resurv_RS_short %>% filter(!is.na(biogeo)),
       aes(x = EUNISa_1_descr, y = NDVI)) +
  geom_flat_violin(position = position_nudge(x = 0.2, y = 0), alpha = 0.8,
                   fill = "lightblue") +
  geom_point(color = "lightblue",
             position = position_jitter(width = 0.15), size = 1, alpha = 0.25) +
  geom_boxplot(width = 0.2, outlier.shape = NA, alpha = 0.5) +
  stat_summary(fun.y = mean, geom = "point", shape = 20, size = 1) +
  stat_summary(fun.data = function(x) data.frame(y = max(x) + 0.1, label = length(x)),
               geom = "text", aes(label = ..label..), vjust = 0.5) +
  labs(y = "Maximum NDVI in July of the sampling year", x = "EUNIS level 1") +
  guides(fill = FALSE, color = FALSE) +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 15)) +
  theme_bw() + coord_flip() + facet_wrap(~ biogeo)
ggsave(
  here("output", "figures", "NDVI_biogeo.tiff"),
  width = 20, height = 15, units = "cm", dpi = 300)

# Canopy height categories

ggplot(db_resurv_RS_short %>%
         # Keep only forests, grasslands, shrublands and wetlands
         filter(EUNISa_1 %in% c("T", "R", "S", "Q")) %>%
         mutate(CH_cat =
                  factor(
                    case_when(canopy_height == 0 ~ "0 m",
                              canopy_height > 0 & canopy_height <= 1 ~ "0-1 m",
                              canopy_height > 1 & canopy_height <=2 ~ "1-2 m",
                              canopy_height > 2 & canopy_height <=5 ~ "2-5 m",
                              canopy_height > 5 & canopy_height <=8 ~ "5-8 m",
                              canopy_height > 8 ~ "> 8 m",
                              is.na(canopy_height) ~ NA_character_),
                    levels = c(
                      "0 m", "0-1 m", "1-2 m", "2-5 m", "5-8 m", "> 8 m"))),
       aes(x = EUNISa_1_descr, fill = CH_cat)) +
  geom_bar() + theme_bw() + coord_flip() +
  scale_y_continuous(labels = label_number()) +
  scale_fill_viridis_d(direction = -1) +
  labs(x = "EUNIS level 1", fill = "Canopy height") +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 15)) +
  theme(legend.position = c(0.8, 0.75),
        legend.direction = "vertical")
ggsave(
  here("output", "figures", "CH_cats_habs_small.tiff"),
  width = 13, height = 11, units = "cm", dpi = 300)

# Number of plots per EUNIS 1

db_resurv %>% filter(!is.na(EUNISa_1)) %>%
  group_by(RS_CODE, `ReSurvey site`, `ReSurvey plot`) %>%
  count(EUNIS_assignation) %>%
  ungroup() %>%
  count(EUNIS_assignation)

EUNIS1_plot <- db_resurv %>% filter(!is.na(EUNISa_1)) %>%
  group_by(RS_CODE, `ReSurvey site`, `ReSurvey plot`) %>%
  summarise(EUNIS1 = if_else(n_distinct(EUNISa_1_descr) > 1, "Habitat change",
                             unique(EUNISa_1_descr)[1]),
            EUNIS_assignation = if_else(n_distinct(EUNIS_assignation) > 1,
                                        "Change", unique(EUNIS_assignation)[1]))

# Define the specific order for EUNIS1
specific_order <- c("Marine habitats", "Inland waters", "Coastal habitats",
                    "Inland habitats with no or little soil", "Wetlands",
                    "Vegetated man-made habitats",
                    "Heathlands, scrub and tundra", 
                    "Forests and other wooded land", "Habitat change",
                    "Grasslands")

# Convert EUNIS1 to a factor with the specified order
EUNIS1_plot <- EUNIS1_plot %>%
  mutate(EUNIS1 = factor(EUNIS1, levels = specific_order))

# Calculate total counts for each EUNIS1 category
total_counts <- EUNIS1_plot %>%
  ungroup() %>%
  count(EUNIS1)

ggplot() +
  geom_bar(data = EUNIS1_plot, 
           aes(x = fct_rev(EUNIS1), fill = EUNIS_assignation),
           position = position_stack(), width = 0.7) +
  geom_text(data = total_counts,
            aes(x = fct_rev(EUNIS1), y = n, label = n), hjust = -0.3) +
  labs(x = "EUNIS habitat group", y = "Number of plots") +
  coord_flip() + theme_classic() +
  theme(legend.position = c(0.9, 0.9)) +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 15)) +
  scale_y_continuous(limits = c(0, 32000))

ggsave(
  here("output", "figures", "EUNIS1_plot.tiff"),
  width = 11, height = 11, units = "cm", dpi = 300)

# Plots NDVI Cordillera / Differential GPS / all ReSurvey

points_halle <- points_halle %>%
  mutate(point_type = if_else(point_type =="Cordillera", "Cordillera (submeter GPS)", point_type)) %>%
  mutate(point_type = factor(point_type,
                             levels = c("ReSurvey",
                                        "ReSurvey differential GPS",
                                        "Cordillera (submeter GPS)"
                                        )))

ggplot(data = points_halle %>%
         mutate(EUNIS_descr = case_when(
           EUNIS == "T" ~ "Forests and other wooded land",
           EUNIS == "S" ~ "Heathlands, scrub and tundra",
           EUNIS == "R" ~ "Grasslands",
           EUNIS == "Q" ~ "Wetlands"
         )),
       aes(x = EUNIS_descr, y = NDVI, fill = EUNIS)) +
  geom_flat_violin(position = position_nudge(x = 0.2, y = 0), alpha = 0.8) +
  geom_point(aes(y = NDVI, color = EUNIS),
             position = position_jitter(width = 0.15), size = 0.5,
             alpha = 0.5) +
  geom_boxplot(width = 0.2, outlier.shape = NA, alpha = 0.5) +
  stat_summary(fun.y=mean, geom="point", shape = 20, size = 1) +
  stat_summary(fun.data = function(x) data.frame(y = max(x) + 0.1,
                                                 label = length(x)),
               geom = "text", aes(label = ..label..), vjust = 0.5) +
  guides(fill = FALSE, color = FALSE) + theme_bw() + coord_flip() +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 15)) +
  facet_wrap(~ point_type) +
  labs(x = "EUNIS level 1", y = "Maximum NDVI in July") +
  theme(panel.spacing = unit(1, "lines"))
ggsave(here("output", "figures", "NDVI_points_halle.tiff"),
       width = 38, height = 9, units = "cm", dpi = 300)

# Distribution of canopy height for each habitat

db_resurv_RS %>% filter(EUNISa_1 == "T") %>%
  ggplot(aes(x = canopy_height)) + geom_density(fill = "lightblue")

db_resurv_RS %>%
  filter(EUNISa_1 == "T" | EUNISa_1 == "R" | EUNISa_1 == "S" | EUNISa_1 == "Q") %>%
  mutate(color_category = case_when(
    EUNISa_1 == "T" & canopy_height < 8 ~ "grey",
    EUNISa_1 == "S" & canopy_height > 5 ~ "grey",
    EUNISa_1 == "R" & canopy_height > 2 ~ "grey",
    EUNISa_1 == "Q" & canopy_height > 2 ~ "grey",
    TRUE ~ EUNISa_1
  )) %>%
  ggplot(aes(x = EUNISa_1_descr, y = canopy_height, color = color_category)) +
  geom_jitter(size = 0.1, width = 0.25, alpha = 0.5) +
  theme_bw() +
  labs(x = "EUNIS level 1", y ="Canopy height (m)") + 
  guides(fill = FALSE, color = FALSE) + 
  coord_flip() +
  scale_y_continuous(breaks = c(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 20, 30)) +
  scale_color_manual(values = c("Q" = "#F8766D", "R" = "#7CAE00",
                                "S" = "#00BFC4", "T" = "#C77CFF",
                                "grey" = "grey")) +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 15))
ggsave(
  here("output", "figures", "CH_points_habs.tiff"),
  width = 16, height = 11, units = "cm", dpi = 300)

# Peak DOY

ggplot(data = db_resurv_RS_short %>%
         # Keep only forests, grasslands, shrublands and wetlands
         filter(EUNISa_1 %in% c("T", "R", "S", "Q") & S2_phen_data == T),
       aes(x = EUNISa_1_descr, y = Peak_DOY, fill = EUNISa_1_descr)) +
  geom_flat_violin(position = position_nudge(x = 0.2, y = 0), alpha = 0.8) +
  geom_point(aes(y = Peak_DOY, color = EUNISa_1_descr),
             position = position_jitter(width = 0.15), size = 1, alpha = 0.25) +
  geom_boxplot(width = 0.2, outlier.shape = NA, alpha = 0.5) +
  stat_summary(fun.y=mean, geom="point", shape = 20, size = 1) +
  stat_summary(fun.data = function(x) data.frame(y = max(x) + 0.1,
                                                 label = length(x)),
               geom = "text", aes(label = ..label..), vjust = 0.5) +
  labs(y = "Date of the maximum NDVI (DOY)", x = "EUNIS level 1") +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 15)) +
  guides(fill = FALSE, color = FALSE) +
  theme_bw() + coord_flip()
ggsave(
  here("output", "figures", "Peak_DOY.tiff"),
  width = 15, height = 11, units = "cm", dpi = 300)

ggplot(data = db_resurv_RS_short %>%
         # Keep only forests, grasslands, shrublands and wetlands
         filter(EUNISa_1 %in% c("T", "R", "S", "Q") & S2_phen_data == T),
       aes(x = EUNISa_1_descr, y = Peak_DOY, fill = EUNISa_1_descr)) +
  geom_flat_violin(position = position_nudge(x = 0.2, y = 0), alpha = 0.8) +
  geom_point(aes(y = Peak_DOY, color = EUNISa_1_descr),
             position = position_jitter(width = 0.15), size = 1, alpha = 0.25) +
  geom_boxplot(width = 0.2, outlier.shape = NA, alpha = 0.5) +
  stat_summary(fun.y=mean, geom="point", shape = 20, size = 1) +
  stat_summary(fun.data = function(x) data.frame(y = max(x) + 0.1,
                                                 label = length(x)),
               geom = "text", aes(label = ..label..), vjust = 0.5) +
  labs(y = "Date of the maximum NDVI (DOY)", x = "EUNIS level 1") +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 15)) +
  guides(fill = FALSE, color = FALSE) +
  theme_bw() + coord_flip() + facet_wrap(~ biogeo)
ggsave(
  here("output", "figures", "Peak_DOY_biogeo.tiff"),
  width = 25, height = 14, units = "cm", dpi = 300)

