library(patchwork)

plot_data <- GAM_data_ids_q1 %>%
  dplyr::filter(PlotObservationID %in% c(3643), # T
                index == "NDVI")

plotC <- ggplot() +
  geom_point(data = plot_data,
             aes(x = DOY, y = value_orig), color = "grey", size = 1, alpha = 0.75) +
  geom_line(data = plot_data,
            aes(x = DOY, y = value), color = "#0072B2", size = 1) +
  # geom_vline(data = plot_data %>%
  #              distinct(PlotObservationID, EUNIS2, index, pos),
  #            aes(xintercept = pos, color = EUNIS2), 
  #            size = 1, linetype = "dashed", color = "#D55E00", alpha = 0.75) +
  # geom_vline(data = plot_data %>%
  #              distinct(PlotObservationID, EUNIS2, index, sos_slope),
  #            aes(xintercept = sos_slope, color = EUNIS2), 
  #            size = 1, linetype = "dashed", color = "#D55E00", alpha = 0.75) +
  # geom_vline(data = plot_data %>%
  #              distinct(PlotObservationID, EUNIS2, index, eos_slope),
  #            aes(xintercept = eos_slope, color = EUNIS2), 
  #            size = 1, linetype = "dashed", color = "#D55E00", alpha = 0.75) +
  # geom_point(data = plot_data %>%
  #              dplyr::filter(DOY == pos),
  #            aes(x = pos, y = value), 
  #            shape = 18, size = 5, color = "#D55E00") +
  # # geom_text(data = plot_data %>%
  # #             dplyr::filter(DOY == pos),
  # #           aes(x = pos, y = value, label = "Peak"), 
  # #           vjust = -1, hjust = 1, size = 3, color = "black") +
  # geom_point(data = plot_data %>%
  #              dplyr::filter(DOY == sos_slope),
  #            aes(x = sos_slope, y = value), 
  #            shape = 18, size = 5, color = "#D55E00") +
  # # geom_text(data = plot_data %>%
  # #             dplyr::filter(DOY == sos_slope),
  # #           aes(x = sos_slope, y = value, label = "Start"), 
  # #           vjust = -1, hjust = 1, size = 3, color = "black") +
  # geom_point(data = plot_data %>%
  #              dplyr::filter(DOY == eos_slope),
  #            aes(x = eos_slope, y = value), 
  #            shape = 18, size = 5, color = "#D55E00") +
  # # geom_text(data = plot_data %>%
  # #             dplyr::filter(DOY == eos_slope),
  # #           aes(x = eos_slope, y = value, label = "End"), 
  # #           vjust = -1, hjust = -0.25, size = 3, color = "black") +
  theme_classic() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank()
  ) +
  theme(legend.position = "none") +
  labs(x = "DOY", y = "Index value", color = "EUNIS level II") +
  scale_color_manual(values = c("darkgreen")) +
  theme(legend.title = element_blank())

plotC

