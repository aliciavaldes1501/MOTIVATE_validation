# Before running this code, I need to run notebook 16_S2_extraction

library(scales)
show_col(hue_pal()(5))

# EUNISa_1
ggplot(data_monthly_EUNISa_1 %>%
         mutate(EUNIS = paste(EUNISa_1, EUNISa_1_descr, sep = " - ")), 
       aes(x = month, y = mean_NDVI, color = EUNIS, 
           group = EUNISa_1_label)) +
  geom_point() +
  geom_line(aes(group = EUNISa_1)) +
  labs(
    title = "Monthly NDVI by Habitat Type",
    x = "Month",
    y = "NDVI",
    color = "Habitat (EUNIS1)"
  ) +
  theme_minimal() +
  scale_color_manual(values = c("#A3A500", "#00B0F6", "#00BF7D", "#F8766D"))
ggsave(
  here("output", "figures", "congresoBOS", "EUNIS1_NDVI.jpeg"),
  dpi = 300, width = 6, height = 2.5)

# EUNISa_2
# T
ggplot(data_monthly_EUNISa_2 %>% 
         dplyr::filter(EUNISa_1 == "T" & !is.na(EUNISa_2)) %>%
         mutate(EUNIS = paste(EUNISa_2, EUNISa_2_descr, sep = " - ")), 
       aes(x = month, y = mean_NDVI, linetype = EUNIS, group = EUNIS)) +
  geom_point(color = "#F8766D") +
  geom_line(color = "#F8766D") +
  labs(
    title = "Forests",
    x = "Month",
    y = "NDVI",
    linetype = "Habitat (EUNIS2)"
  ) +
  theme_minimal()
ggsave(
  here("output", "figures", "congresoBOS", "EUNIS2_T_NDVI.jpeg"),
  dpi = 300, width = 6, height = 2.5)

# R
ggplot(data_monthly_EUNISa_2 %>% 
         dplyr::filter(EUNISa_1 == "R" & !is.na(EUNISa_2)) %>%
         mutate(EUNIS = paste(EUNISa_2, EUNISa_2_descr, sep = " - ")), 
       aes(x = month, y = mean_NDVI, linetype = EUNIS, group = EUNIS)) +
  geom_point(color = "#00B0F6") +
  geom_line(color = "#00B0F6") +
  labs(
    title = "Grasslands",
    x = "Month",
    y = "NDVI",
    linetype = "Habitat (EUNIS2)"
  ) +
  theme_minimal()
ggsave(
  here("output", "figures", "congresoBOS", "EUNIS2_R_NDVI.jpeg"),
  dpi = 300, width = 7, height = 2.5)

# Max NDVI
distr_plot(final_RS_data, "NDVI_max", "Valor máximo anual de NDVI")
ggsave(
  here("output", "figures", "congresoBOS", "max_NDVI.jpeg"),
  dpi = 300, width = 5, height = 3)

## Diferencia peak-greening
distr_plot(final_RS_data %>%
             group_by(PlotObservationID, EUNISa_1, EUNISa_1_descr) %>%
             summarise(NDVI_diff_pos_march_value = mean(NDVI_diff_pos_march_value,
                                                        na.rm = T)),
           "NDVI_diff_pos_march_value", 
           "Diferencia en NDVI entre pico y comienzo de\ncrecimiento (media 2017-2024)")
ggsave(
  here("output", "figures", "congresoBOS", "mean_NDVI_diff_pos_march_value.jpeg"),
  dpi = 300, width = 4, height = 3)

