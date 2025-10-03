ggplot() +
  geom_point(data = plot_data,
             aes(x = DOY, y = value_orig, color = EUNISa_1), size = 1, alpha = 0.5) +
  geom_line(data = plot_data,
            aes(x = DOY, y = value, color = EUNISa_1), size = 1) +
  geom_vline(data = plot_data %>%
               distinct(PlotObservationID, EUNISa_1, index, pos),
             aes(xintercept = pos, color = EUNISa_1), 
             size = 1, alpha = 0.75, linetype = "dotted") +
  geom_vline(data = plot_data %>%
               distinct(PlotObservationID, EUNISa_1, index, sos_threshold),
             aes(xintercept = sos_threshold, color = EUNISa_1),
             size = 1, alpha = 0.75, linetype = "dashed") +
  geom_vline(data = plot_data %>%
               distinct(PlotObservationID, EUNISa_1, index, eos_threshold),
             aes(xintercept = eos_threshold, color = EUNISa_1), 
             size = 1, alpha = 0.75, linetype = "dashed") +
  theme_minimal() +
  theme(legend.position = "none", legend.title = element_blank()) +
  ylab("NDVI") +
  scale_color_manual(values = c("#C77CFF", "#7CAE00", "#00BFC4", "#F8766D")) +
  facet_grid(cols = vars(EUNISa_1))

distr_plot(data_validation %>% dplyr::filter(EVI_min > -0.5),
           c("NDVI_max"),
           c("Maximum value of NDVI"))
