WKSHP.fake.data = read.csv("Synthetic_CoverCrop_SoilHealth_Data_v4.csv")



soil_dat <- read_csv("Synthetic_CoverCrop_SoilHealth_Data_v4.csv")%>%
  
  mutate(
    Site = as.factor(Site),
    Year = as.factor(Year),
    Block = as.factor(Block),
    Treatment = factor(Treatment, levels = c("Control", "SG_Low", "SG_High", "CC_Low", "CC_High")),
    Depth_cm = factor(Depth_cm, levels = c("0-15", "15-30"))
  )

# 2. Fit Mixed Model for Organic Matter (%)
# Factorial fixed effects: Treatment * Depth_cm * Site
# Random effect: Block nested within Site
om_mod <- lmer(Organic_Matter_pct ~ Treatment * Depth_cm + Site + (1|Block), data = soil_dat)

# 3. Extract emmeans partitioned by Site and Depth
om_emm <- emmeans(om_mod, ~ Treatment |  Site * Depth_cm)

# 4. Generate CLD letters (descending order so 'a' is highest mean)
om_cld <- cld(om_emm, alpha = 0.05, Letters = letters, adjust = "tukey", decreasing = TRUE)
om_cld

# 5. Merge CLD back to emmeans summary 
om_plot_data <- as.data.frame(om_emm) %>%
  left_join(
    as.data.frame(om_cld) %>% select(Treatment, Site, Depth_cm, .group),
    by = c("Treatment", "Site", "Depth_cm")
  ) %>%
  mutate(Letter = trimws(.group))

# 6. Generate Plot matching your aesthetic preferences
ggplot(om_plot_data, aes(x = Treatment, y = emmean, fill = Treatment)) +
  geom_bar(stat = "identity", color = "black", alpha = 1) +
  geom_errorbar(aes(ymin = emmean - SE, ymax = emmean + SE), width = 0.2) +
  geom_text(aes(label = Letter, y = emmean + SE), vjust = -0.5, size = 3.5) +
  facet_grid(Depth_cm ~ Site, scales = "free_y") +
  scale_fill_viridis_d(option = "mako") +
  labs(
    x = "Cover Crop Treatment",
    y = expression("Estimated Mean Soil Organic Matter ("*"%"*")"),
    caption = paste0(
      "Bars represent estimated marginal means.\n",
      "Error bars represent ±1 SE.\n",
      "Letters indicate significant differences (Tukey HSD, alpha = 0.05)."
    )
  ) +
  theme_bw(base_family = "serif") +
  theme(
    legend.position = "none",
    axis.text.x = element_text(angle = 45, hjust = 1),
    strip.text = element_text(face = "bold", size = 11)
  )
