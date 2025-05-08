library(tidyverse)
library(caret)
library(cluster)
library(factoextra)
library(ggplot2)

filtered_data <- db_resurv_RS_short_PLOT %>%
  filter(EUNISa_1 %in% c("T", "R", "S", "Q")) %>%
  filter(`Location method` == "Location with differential GPS") %>%
  select(starts_with("NDVI"), starts_with("NDMI"), starts_with("NDWI"),
         starts_with("SAVI"), starts_with("EVI"), canopy_height, EUNISa_1) %>%
  na.omit() %>%
  filter(EVI_max <= 1)

filtered_data <- db_resurv_RS_short_PLOT %>%
  filter(EUNISa_1 %in% c("T", "R", "S", "Q")) %>%
  filter(`Location method` == "Location with differential GPS") %>%
  select(NDVI_max, NDMI_max, NDWI_max, SAVI_max, EVI_max, canopy_height, EUNISa_1) %>%
  na.omit() %>%
  filter(EVI_max <= 1)

features <- filtered_data %>% select(where(is.numeric))

standardized_features <- scale(features)

pca <- prcomp(standardized_features, center = TRUE, scale. = TRUE)
pca
summary(pca)
pca_df <- as_tibble(pca$x) %>% 
  mutate(EUNISa_1 = filtered_data$EUNISa_1)

ggplot(pca_df, aes(x = PC1, y = PC2, color = EUNISa_1)) +
  geom_point() +
  labs(title = "PCA of Habitat Types") +
  theme_minimal()
biplot(pca)

set.seed(123)
kmeans_result <- kmeans(standardized_features, centers = 4)
filtered_data <- filtered_data %>% mutate(Cluster = kmeans_result$cluster)

pca_df <- pca_df %>% mutate(Cluster = as.factor(kmeans_result$cluster))

ggplot(pca_df, aes(x = PC1, y = PC2, color = Cluster)) +
  geom_point() +
  labs(title = "KMeans Clustering of Habitat Types") +
  theme_minimal()

habitat_to_cluster <- c('T' = 1, 'S' = 2, 'R' = 3, 'Q' = 4)
filtered_data <- filtered_data %>%
  mutate(Expected_Cluster = as.factor(habitat_to_cluster[EUNISa_1]))

misclassified_points <- filtered_data %>% filter(Cluster != Expected_Cluster)
print("Potential Misclassified Points:")
print(misclassified_points)
