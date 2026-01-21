library(patchwork)

plotA <- ggplot(data_monthly_EUNISa_1 %>%
         mutate(EUNIS = paste(EUNISa_1, EUNISa_1_descr, sep = " - ")), 
       aes(x = month, y = mean_NDVI, color = EUNISa_1, 
           group = EUNISa_1)) +
  geom_point(size = 1.5) +
  geom_line(aes(group = EUNISa_1), size = 0.5) +
  labs(x = "Month", y = "NDVI", color = "EUNIS level I") +
  theme_minimal() +
  theme(legend.position = "top", legend.margin = margin(t = 1, b = 1),
        legend.key.width = unit(0.4, "cm"), legend.spacing.x = unit(0.01, "cm")) +
  scale_color_manual(values = c("deepskyblue3", "darkgoldenrod2", "coral4", 
                                "chartreuse4")) +
  scale_x_discrete(labels = function(x) substr(x, 1, 1)) +
  theme(legend.title = element_blank())

plotB <- ggplot(data_monthly_EUNISa_2 %>% 
         dplyr::filter(EUNISa_1 == "T" & !is.na(EUNISa_2)) %>%
         mutate(EUNIS = paste(EUNISa_2, EUNISa_2_descr, sep = " - ")), 
       aes(x = month, y = mean_NDVI, color = EUNISa_2, group = EUNISa_2)) +
  geom_point(size = 1.5) +
  geom_line(size = 0.5) +
  labs(x = "Month", y = "NDVI", color = "EUNIS level II") +
  theme_minimal() +
  theme(legend.position = "top", legend.margin = margin(t = 1, b = 1),
        legend.key.width = unit(0.4, "cm"), legend.spacing.x = unit(0.01, "cm")) +
  scale_color_manual(values = c("darkgreen", "darkolivegreen3", 
                                "aquamarine3")) +
  scale_x_discrete(labels = function(x) substr(x, 1, 1)) +
  theme(legend.title = element_blank()) +
  theme(axis.title.y = element_blank())

plot_data <- GAM_data_ids_q1 %>%
  dplyr::filter(PlotObservationID %in% c(3643), # T
                index == "NDVI")

plotC <- ggplot() +
  geom_point(data = plot_data,
             aes(x = DOY, y = value_orig, color = EUNISa_2), size = 1, alpha = 0.5) +
  geom_line(data = plot_data,
            aes(x = DOY, y = value, color = EUNISa_2), size = 0.5) +
  geom_vline(data = plot_data %>%
               distinct(PlotObservationID, EUNISa_2, index, pos),
             aes(xintercept = pos, color = EUNISa_2), 
             size = 0.5, linetype = "dashed", color = "darkslategrey") +
  geom_vline(data = plot_data %>%
               distinct(PlotObservationID, EUNISa_2, index, sos_slope),
             aes(xintercept = sos_slope, color = EUNISa_2), 
             size = 0.5, linetype = "dashed", color = "darkslategrey", alpha = 0.75) +
  geom_vline(data = plot_data %>%
               distinct(PlotObservationID, EUNISa_2, index, eos_slope),
             aes(xintercept = eos_slope, color = EUNISa_2), 
             size = 0.5, linetype = "dashed", color = "darkslategrey", alpha = 0.75) +
  geom_point(data = plot_data %>%
               dplyr::filter(DOY == pos),
             aes(x = pos, y = value), 
             shape = 18, size = 5, color = "darkgreen") +
  # geom_text(data = plot_data %>%
  #             dplyr::filter(DOY == pos),
  #           aes(x = pos, y = value, label = "Peak"), 
  #           vjust = -1, hjust = 1, size = 3, color = "black") +
  geom_point(data = plot_data %>%
               dplyr::filter(DOY == sos_slope),
             aes(x = sos_slope, y = value), 
             shape = 18, size = 5, color = "darkgreen") +
  # geom_text(data = plot_data %>%
  #             dplyr::filter(DOY == sos_slope),
  #           aes(x = sos_slope, y = value, label = "Start"), 
  #           vjust = -1, hjust = 1, size = 3, color = "black") +
  geom_point(data = plot_data %>%
               dplyr::filter(DOY == eos_slope),
             aes(x = eos_slope, y = value), 
             shape = 18, size = 5, color = "darkgreen") +
  # geom_text(data = plot_data %>%
  #             dplyr::filter(DOY == eos_slope),
  #           aes(x = eos_slope, y = value, label = "End"), 
  #           vjust = -1, hjust = -0.25, size = 3, color = "black") +
  theme_minimal() +
  theme(legend.position = "top", legend.margin = margin(t = 1, b = 1),
        legend.key.width = unit(0.4, "cm"), legend.spacing.x = unit(0.01, "cm")) +
  labs(x = "DOY", y = "NDVI", color = "EUNIS level II") +
  scale_color_manual(values = c("darkgreen")) +
  theme(legend.title = element_blank()) +
  theme(axis.title.y = element_blank())

figure <- plotA + theme(plot.tag.position  = c(0.1, 0.93)) +
  plotB + theme(plot.tag.position  = c(0.1, 0.93)) +
  plotC + theme(plot.tag.position  = c(0.1, 0.93))
  #plot_annotation(tag_levels = "A", tag_prefix = "", tag_suffix = ")")
figure
ggsave(
  here("output", "figures", "figure_HABITATS.jpeg"),
  dpi = 300, width = 6, height = 3)
