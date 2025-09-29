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
  scale_x_discrete(labels = function(x) str_wrap(x, width = 10)) +
  scale_y_continuous(limits = c(0, 32000)) +
  scale_fill_manual(values = c("grey", "#C77CFF", "#7CAE00", "#00BFC4", "#F8766D"))

ggsave(
  here("output", "figures", "EUNIS1_plot_IALE.tiff"),
  width = 12, height = 8, units = "cm", dpi = 300)

# R4: Alpine and subapline grasslands
# T1: Broadleaved deciduous forests
# S4: Temperate heathland
# Q1: Raised and blanket bogs

plot_data <- GAM_data_ids_q1 %>%
  dplyr::filter(PlotObservationID %in% c(317925, 381160, 6326, 67959),
         index == "NDVI")

ggplot() +
  geom_point(data = plot_data,
             aes(x = DOY, y = value_orig, color = EUNISa_1), size = 1, alpha = 0.5) +
    geom_line(data = plot_data,
              aes(x = DOY, y = value, color = EUNISa_1), size = 1) +
  geom_vline(data = plot_data %>%
               distinct(PlotObservationID, EUNISa_1, index, pos),
             aes(xintercept = pos, color = EUNISa_1), size = 1, alpha = 0.75) +
  geom_vline(data = data.frame(DOY = 75), aes(xintercept = DOY),
             size = 0.5, linetype = "dashed", color = "darkslategrey", alpha = 0.75) +
  geom_vline(data = data.frame(DOY = 285), aes(xintercept = DOY),
             size = 0.5, linetype = "dashed", color = "darkslategrey", alpha = 0.75) +
  theme_minimal() +
  theme(legend.position = "none", legend.title = element_blank()) +
  ylab("NDVI") +
  scale_color_manual(values = c("#C77CFF", "#7CAE00", "#00BFC4", "#F8766D")) +
  facet_grid(cols = vars(EUNISa_1))

ggsave(
  here("output", "figures", "spectrophen_plot_IALE.tiff"),
  width = 20, height = 8, units = "cm", dpi = 300)

