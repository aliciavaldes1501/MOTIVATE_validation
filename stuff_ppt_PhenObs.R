db_Europa_allobs <- read_csv(
  here("data", "clean", "db_Europa_allobs.csv"))

# Number of plots per EUNIS 1

EUNIS1_plot <- db_Europa_allobs %>% filter(!is.na(EUNISa_1)) %>%
  group_by(RS_CODE, `ReSurvey site`, `ReSurvey plot`) %>%
  summarise(EUNIS1 = if_else(n_distinct(EUNISa_1) > 1, "",
                             unique(EUNISa_1)[1]),
            EUNIS1_descr = if_else(n_distinct(EUNISa_1_descr) > 1, "Habitat change",
                             unique(EUNISa_1_descr)[1]))

# Define the specific order for EUNIS1
specific_order <- c("","Q",
                    "S", 
                    "R", 
                    "T")

# Convert EUNIS1 to a factor with the specified order
EUNIS1_plot <- EUNIS1_plot %>%
  mutate(EUNIS1 = factor(EUNIS1, levels = specific_order)) %>%
  mutate(label = paste0(EUNIS1, ": ", EUNIS1_descr))

# Calculate total counts for each EUNIS1 category
total_counts <- EUNIS1_plot %>%
  ungroup() %>%
  count(label)

ggplot() +
  geom_bar(data = EUNIS1_plot, 
           aes(x = fct_rev(label), fill = label),
           position = position_stack(), width = 0.7) +
  geom_text(data = total_counts,
            aes(x = fct_rev(label), y = n, label = n), hjust = -0.3) +
  labs(x = "EUNIS habitat group", y = "Number of plots") +
  coord_flip() + theme_classic() +
  theme(legend.position = "none") +
  scale_x_discrete(labels = c("Habitat change" = "Habitat change",
                              "Q: Wetlands" = "Wetlands",
                              "R: Grasslands" = "Grasslands",
                              "S: Heathlands, scrub and tundra" = "Shrublands",
                              "T: Forests and other wooded land" = "Forests")) +
  scale_y_continuous(limits = c(0, 38000)) +
  scale_fill_manual(values = c("grey", "#C77CFF", "#7CAE00", "#00BFC4", "#F8766D"))

ggsave(
  here("output", "figures", "EUNIS1_plot_IALE.tiff"),
  width = 12, height = 8, units = "cm", dpi = 300)

# R4: Alpine and subapline grasslands
# T1: Broadleaved deciduous forests
# S4: Temperate heathland
# Q1: Raised and blanket bogs

plot_data <- GAM_data_ids_q1 %>%
  dplyr::filter(PlotObservationID %in% c(1844888, # Q
                                         1841966, # R
                                         1846809, # S
                                         3643), # T
         index == "NDVI")

ggplot() +
  geom_point(data = plot_data,
             aes(x = DOY, y = value_orig, color = EUNISa_1), size = 1, alpha = 0.5) +
    geom_line(data = plot_data,
              aes(x = DOY, y = value, color = EUNISa_1), size = 1) +
  geom_vline(data = plot_data %>%
               distinct(PlotObservationID, EUNISa_1, index, pos),
             aes(xintercept = pos, color = EUNISa_1), 
             size = 1, alpha = 0.75, color = "darkgrey") +
  geom_vline(data = plot_data %>%
               distinct(PlotObservationID, EUNISa_1, index, sos_slope),
             aes(xintercept = sos_slope, color = EUNISa_1), 
             size = 0.5, linetype = "dashed", color = "darkslategrey", alpha = 0.75) +
  geom_vline(data = plot_data %>%
               distinct(PlotObservationID, EUNISa_1, index, eos_slope),
             aes(xintercept = eos_slope, color = EUNISa_1), 
             size = 0.5, linetype = "dashed", color = "darkslategrey", alpha = 0.75) +
  theme_minimal() +
  theme(legend.position = "none", legend.title = element_blank()) +
  ylab("NDVI") +
  scale_color_manual(values = c("#C77CFF", "#7CAE00", "#00BFC4", "#F8766D")) +
  facet_grid(rows = vars(EUNISa_1))

ggsave(
  here("output", "figures", "spectrophen_plot_PhenObs.tiff"),
  width = 20, height = 8, units = "cm", dpi = 300)

# Montly NDVI by habitat type
ggplot(data_monthly_EUNISa_1, 
       aes(x = month, y = mean_NDVI, color = EUNISa_1_descr, 
           group = EUNISa_1_descr)) +
  geom_point() +
  geom_line(aes(group = EUNISa_1_descr)) +
  scale_color_discrete(labels = c("Wetlands" = "Wetlands",
                              "Grasslands" = "Grasslands",
                              "Heathlands, scrub and tundra" = "Shrublands",
                              "Forests and other wooded land" = "Forests")) +
  labs(
    x = "Month",
    y = "NDVI",
    color = "Habitat (EUNIS1)"
  ) +
  theme_minimal()

ggplot(data_monthly_EUNISa_2 %>% 
         dplyr::filter(EUNISa_1 == "T" & !is.na(EUNISa_2)), 
       aes(x = month, y = mean_NDVI, linetype = EUNISa_2_descr,
           group = EUNISa_2_descr)) +
  geom_point(color = "#F8766D") +
  geom_line(color = "#F8766D") +
  labs(
    x = "Month",
    y = "NDVI",
    linetype = "Habitat (EUNIS2)"
  ) +
  theme_minimal()


