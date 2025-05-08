# Models

# Convert EUNISa_1 to factor
db_resurv_RS_short <- db_resurv_RS_short %>%
  mutate(EUNISa_1 = as.factor(EUNISa_1))
# Set unit PAN for bioregion PAN
db_resurv_RS_short <- db_resurv_RS_short %>%
  mutate(unit = if_else(biogeo == "PAN", "PAN", unit))
# Add plot to include as random effect
db_resurv_RS_short_S2_phen <- db_resurv_RS_short %>%
  filter(S2_phen_data == T) %>%
  mutate(RS_site = `ReSurvey site`, RS_plot = `ReSurvey plot`) %>%
  lazy_dt() %>%
  group_by(RS_CODE, RS_site, RS_plot) %>%
  mutate(plot = .GRP) %>%
  as_tibble() %>%
  select(-RS_site, -RS_plot)

### HERE: Careful!

# We need to include plot as a random effect on this kind of models.
# For doing this, and probably the previous stuff too, 
# we need to ensure that there is only one observation of each plot per year.
# So far there are some plots with more than one observation per year.

db_resurv_RS_short_S2_phen %>%
  group_by(plot) %>%
  # Count the number of times each year appears
  count(year) %>%
  # Filter when a year appears more than once
  filter(n > 1)

# Develop a solution for this after Halle!

# So far not including this in models. Do we include plot as a random effect?
# Nested within unit or not? And year?
  
# Or use interannual means? Then we would have one value per plot -
# but what about plots where the habitat type changed?
# (we could tag them as "change").

lmer_SOS_DOY <- lmer(SOS_DOY ~ EUNISa_1 + (1|biogeo/unit),
                     data = db_resurv_RS_short_S2_phen %>%
                       # Keep only forests, grasslands, shrublands and wetlands
                       filter(EUNISa_1 %in% c("T", "R", "S", "Q")))
lmer_Peak_DOY <- lmer(Peak_DOY ~ EUNISa_1 + (1|biogeo/unit),
                      data = db_resurv_RS_short_S2_phen %>%
                        # Keep only forests, grasslands, shrublands and wetlands
                        filter(EUNISa_1 %in% c("T", "R", "S", "Q")))
lmer_EOS_DOY <- lmer(EOS_DOY ~ EUNISa_1 + (1|biogeo/unit),
                     data = db_resurv_RS_short_S2_phen %>%
                       # Keep only forests, grasslands, shrublands and wetlands
                       filter(EUNISa_1 %in% c("T", "R", "S", "Q")))

summary(lmer_SOS_DOY)
summary(lmer_Peak_DOY)
summary(lmer_EOS_DOY)
Anova(lmer_SOS_DOY)
Anova(lmer_Peak_DOY)
Anova(lmer_EOS_DOY)

plot(ggpredict(lm_SOS_DOY))
plot(ggpredict(lm_Peak_DOY))
plot(ggpredict(lm_EOS_DOY))

### HERE: ggplot means and error plot with SOS, Peak and EOS together

### HERE: correspondence analyses (CAs), 
# biplot showing the distribution of points, similar to PCA, 
# see if they are grouped by habiat type... (read paper sent by Bea)


