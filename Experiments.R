#----LOADING PACKAGES----
library("ggeffects")
library("ggplot2")
library("lmerTest")
library("emmeans")
library("multcomp")
library("multcompView")
library("RColorBrewer")
library("tidyverse")
library("MASS")
library("readxl")
library("car")
library("kableExtra")
library("see")
library("knitr")
library("dplyr")
library("kableExtra")
library("viridis")
library("carData")
library("performance")
library("googlesheets4")
library("tibble")
library("webshot2")


#for lm ANOVA export car::Anova()

F24_CC_CNrat_ANOVA %>%
  tidy() %>%
  select(
    Source = term,
    `Sum Sq` = sumsq,
    Df = df,
    `F value` = statistic,
    `Pr(>F)` = p.value
  ) %>%
  mutate(`Pr(>F)` = format(`Pr(>F)`, scientific = FALSE, digits = 4)) %>%
  {
    cat(paste(colnames(.), collapse = "\t"), "\n")
    apply(., 1, function(x) cat(paste(x, collapse = "\t"), "\n"))
  }



# for lmer anova() ANOVA

F24_CC_CNrat_plot_data %>%
  select(Site, Treatment_Full, response, SE, Letter) %>%
  {
    cat(paste(colnames(.), collapse = "\t"), "\n")
    apply(., 1, function(x) cat(paste(x, collapse = "\t"), "\n"))
  }





# getting the emmeans, SEs and cld to copy to google sheets neatly

F24_CC_CNrat_plot_data %>%
  select(Site, Treatment_Full, response, SE, Letter) %>%
  {
    cat(paste(colnames(.), collapse = "\t"), "\n")
    apply(., 1, function(x) cat(paste(x, collapse = "\t"), "\n"))
  }





# 1. Data Wrangling & Mutate Deployment Labels
F24XS25_ER_three_phase <- F24XS25_ERXAGBM_dat %>%
  # Exclude 'No CC' and 'Oat + Radish' ONLY at Lamberton in Spring 2025
  filter(!(Site == "Lamberton" & Season == "S25" & Trt_type %in% c("No CC", "Oat + Radish"))) %>%
  mutate(
    Deployment_Label = case_when(
      (Season %in% c("F24", "Fall 2024") & Deployment == "D1") ~ "Fall Pre-Frost",
      (Season %in% c("S25", "Spring 2025") & Deployment == "D1") ~ "Spring Pre-Termination",
      (Season %in% c("S25", "Spring 2025") & Deployment == "D2") ~ "Spring Post-Termination",
      TRUE ~ as.character(Deployment)
    ),
    Deployment_Label = factor(
      Deployment_Label, 
      levels = c("Fall Pre-Frost", "Spring Pre-Termination", "Spring Post-Termination")
    )
  ) %>%
  droplevels()


view(F24XS25_ER_three_phase)

# 2. Fit Mixed Model & Obtain emmeans by Deployment
# (Assuming log-transformed response; replace with actual variable name if different)
Y1_3p_er_mod <- lmer(log(ER_gm2day) ~ Treatment_Full * Deployment_Label + (1|Site:Rep),
               data = F24XS25_ER_three_phase)

Y1_3p_er_emm <- emmeans(Y1_3p_er_mod, ~ Treatment_Full | Deployment_Label)

Y1_3p_er_cld <- cld(Y1_3p_er_emm, alpha = 0.05, Letters = letters, adjust = "tukey", decreasing = TRUE)

ER_3P_plot_data <- regrid(Y1_3p_er_emm, transform = "response") %>%
  as.data.frame() %>%
  left_join(
    as.data.frame(Y1_3p_er_cld) %>% select(Treatment_Full, Deployment_Label, .group), 
    by = c("Treatment_Full", "Deployment_Label")
  ) %>%
  mutate(Letter = trimws(.group))

# ==============================================================================
# 1. Direct Data Prep: Conditionally swap individual letters for 'n.s.'
# ==============================================================================
ER_3P_plot_data_clean <- ER_3P_plot_data %>%
  group_by(Deployment_Label) %>%
  mutate(
    # Check if panel has significant overall differences
    Is_Sig = n_distinct(Letter) > 1,
    
    # Keep bar letters if significant; replace ONLY the middle bar with 'n.s.' if non-significant
    Bar_Label = if_else(
      Is_Sig, 
      Letter, 
      if_else(row_number() == ceiling(n() / 2), "n.s.", "")
    )
  ) %>%
  ungroup()

# ==============================================================================
# 2. Simplified Single-Geom Plotting Pipeline
# ==============================================================================
ggplot(ER_3P_plot_data_clean, aes(x = Treatment_Full, y = response, fill = Treatment_Full)) +
  geom_bar(stat = "identity", color = "black", alpha = 0.8) +
  geom_errorbar(
    aes(ymin = pmax(0, response - SE), ymax = response + SE), 
    width = 0.2, 
    linewidth = 0.6
  ) +
  # Single geom_text layer handling both Tukey letters and the single centered "n.s."
  geom_text(
    aes(
      label = Bar_Label, 
      y = response + SE,
      fontface = if_else(Bar_Label == "n.s.", "bold", "plain")
    ), 
    vjust = -0.5, 
    size = 4, 
    family = "serif"
  ) +
  facet_wrap(~ Deployment_Label, scales = "fixed") +
  scale_fill_manual(values = color_map) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.15))) +
  labs(
    title = "Water-Borne Sediment Erosion Across Cover Crop Phases",
    x = "Cover Crop Treatment",
    y = expression(Water-Borne~Sediment~Erosion~(g~m^-2~day^-1)),
    caption = paste0(
      "Bars represent back-transformed estimated marginal means (emmeans).\n",
      "Error bars represent Delta-method approximated ±1 SE.\n",
      "Letters indicate significant differences within deployment timing (Tukey HSD, alpha = 0.05); n.s. = not significant."
    )
  ) +
  theme_bw(base_family = "serif") +
  theme(
    legend.position = "none",
    axis.text.x = element_text(angle = 45, hjust = 1, face = "bold", color = "black"),
    axis.text.y = element_text(color = "black"),
    axis.title = element_text(face = "bold"),
    strip.text = element_text(face = "bold", size = 11),
    panel.grid.minor = element_blank()
  )










#----Merging lysimeter and biomass emmeans for plot S25 lysimeter and S25 biomass----#

# ==============================================================================
# 1. Format & Align Means Dataframes Across Measurements
# ==============================================================================
# Extract biomass emmeans + compact letter display
bm_cld_df <- as.data.frame(S25_CCBM_cld)
bm_group_col <- intersect(c(".group", "group"), names(bm_cld_df))[1]

bm_clean <- as.data.frame(S25_CCBM_emm) %>%
  left_join(
    bm_cld_df %>% select(Treatment_Full, Site, Letter_bm_raw = all_of(bm_group_col)),
    by = c("Treatment_Full", "Site")
  ) %>%
  filter(
    Site %in% c("Saint Paul", "Waseca"),
    Treatment_Full %in% c("Rye 80", "Rye 10", "Fallow")
  ) %>%
  mutate(Letter_bm = trimws(Letter_bm_raw)) %>%
  select(Site, Treatment_Full, emmean_bm = emmean, SE_bm = SE, Letter_bm)

# Extract nitrate emmeans + compact letter display
lys_cld_df <- as.data.frame(lys_cld)
lys_group_col <- intersect(c(".group", "group"), names(lys_cld_df))[1]

lys_clean <- as.data.frame(lys_emm) %>%
  left_join(
    lys_cld_df %>% select(Treatment_Full, Site, Letter_no3_raw = all_of(lys_group_col)),
    by = c("Treatment_Full", "Site")
  ) %>%
  filter(
    Site %in% c("Saint Paul", "Waseca"),
    Treatment_Full %in% c("Rye 80", "Rye 10", "Fallow")
  ) %>%
  mutate(Letter_no3 = trimws(Letter_no3_raw)) %>%
  select(Site, Treatment_Full, emmean_no3 = emmean, SE_no3 = SE, Letter_no3)

# ==============================================================================
# 2. Merge Data & Insert Zero Values for Fallow Biomass
# ==============================================================================
dual_plot_data <- full_join(bm_clean, lys_clean, by = c("Site", "Treatment_Full")) %>%
  mutate(
    # Zero out biomass metrics for Fallow control
    emmean_bm = if_else(Treatment_Full == "Fallow" & is.na(emmean_bm), 0, emmean_bm),
    SE_bm     = if_else(Treatment_Full == "Fallow" & is.na(SE_bm), 0, SE_bm),
    Letter_bm = if_else(Treatment_Full == "Fallow" & is.na(Letter_bm), "", Letter_bm),
    Treatment_Full = factor(Treatment_Full, levels = c("Rye 80", "Rye 10", "Fallow"))
  )

view(dual_plot_data)
# Dynamic scale factor calculation
max_bm  <- max(dual_plot_data$emmean_bm + dual_plot_data$SE_bm, na.rm = TRUE)
max_no3 <- max(dual_plot_data$emmean_no3 + dual_plot_data$SE_no3, na.rm = TRUE)
scale_factor <- max_bm / max_no3

# ==============================================================================
# 3. Dual-Axis Plot Generation
# ==============================================================================
ggplot(dual_plot_data, aes(x = Treatment_Full)) +
  # Primary Axis: Biomass Bars & Errors
  geom_bar(
    aes(y = emmean_bm, fill = Treatment_Full),
    stat = "identity", color = "black", alpha = 0.7, width = 0.55
  ) +
  geom_errorbar(
    aes(ymin = pmax(0, emmean_bm - SE_bm), ymax = emmean_bm + SE_bm),
    width = 0.15, linewidth = 0.6
  ) +
  geom_text(
    aes(y = emmean_bm + SE_bm, label = Letter_bm),
    vjust = -0.5, size = 3.5, family = "serif"
  ) +
  
  # Secondary Axis: Nitrate Line, Points & Errors
  geom_line(
    aes(y = emmean_no3 * scale_factor, group = 1),
    color = "firebrick", linewidth = 1
  ) +
  geom_errorbar(
    aes(
      ymin = (emmean_no3 - SE_no3) * scale_factor,
      ymax = (emmean_no3 + SE_no3) * scale_factor
    ),
    color = "firebrick", width = 0.15, linewidth = 0.6
  ) +
  geom_point(
    aes(y = emmean_no3 * scale_factor),
    color = "firebrick", size = 3
  ) +
  geom_text(
    aes(y = (emmean_no3 + SE_no3) * scale_factor, label = Letter_no3),
    vjust = -0.6, color = "firebrick", fontface = "bold", size = 3.5, family = "serif"
  ) +
  
  # Faceting & Dual Y Scales
  facet_wrap(~ Site) +
  scale_fill_manual(values = color_map) +
  scale_y_continuous(
    name = expression("Estimated Mean Biomass ("*kg~ha^-1*")"),
    sec.axis = sec_axis(~ . / scale_factor, name = expression("Estimated Mean Porewater "*NO[3]*"-"*N~"(mg/L)")),
    expand = expansion(mult = c(0, 0.20))
  ) +
  
  # Aesthetics & Formatting
  labs(
    x = "Cover Crop Treatment",
    caption = paste0(
      "Bars and red points represent model estimated marginal means (emmeans) ± 1 SE.\n",
      "Black letters: Biomass comparisons | Red letters: Porewater NO3-N comparisons (Tukey HSD, alpha = 0.05)."
    )
  ) +
  theme_bw(base_family = "serif") +
  theme(
    legend.position = "none",
    axis.text.x = element_text(angle = 45, hjust = 1, face = "bold", color = "black"),
    axis.text.y = element_text(color = "black"),
    axis.title = element_text(face = "bold"),
    strip.text = element_text(face = "bold", size = 11),
    axis.title.y.right = element_text(color = "firebrick", face = "bold"),
    axis.text.y.right  = element_text(color = "firebrick"),
    panel.grid.minor = element_blank()
  )








# ==============================================================================
# 1. Format & Align Means Dataframes Across Measurements
# ==============================================================================
# Extract biomass emmeans + compact letter display
bm_cld_df <- as.data.frame(S25_CCBM_cld)
bm_group_col <- intersect(c(".group", "group"), names(bm_cld_df))[1]

bm_clean <- as.data.frame(S25_CCBM_emm) %>%
  left_join(
    bm_cld_df %>% select(Treatment_Full, Site, Letter_bm_raw = all_of(bm_group_col)),
    by = c("Treatment_Full", "Site")
  ) %>%
  filter(
    Site %in% c("Saint Paul", "Waseca"),
    Treatment_Full %in% c("Rye 80", "Rye 10", "Fallow")
  ) %>%
  mutate(Letter_bm = trimws(Letter_bm_raw)) %>%
  select(Site, Treatment_Full, emmean_bm = emmean, SE_bm = SE, Letter_bm)

# Extract regridded (back-transformed) nitrate emmeans directly from lys_plot_data
lys_clean <- lys_plot_data %>%
  filter(
    Site %in% c("Saint Paul", "Waseca"),
    Treatment_Full %in% c("Rye 80", "Rye 10", "Fallow")
  ) %>%
  select(Site, Treatment_Full, emmean_no3 = response, SE_no3 = SE, Letter_no3 = Letter)

# ==============================================================================
# 2. Merge Data & Insert Zero Values for Fallow Biomass
# ==============================================================================
dual_plot_data <- full_join(bm_clean, lys_clean, by = c("Site", "Treatment_Full")) %>%
  mutate(
    # Zero out biomass metrics for Fallow control
    emmean_bm = if_else(Treatment_Full == "Fallow" & is.na(emmean_bm), 0, emmean_bm),
    SE_bm     = if_else(Treatment_Full == "Fallow" & is.na(SE_bm), 0, SE_bm),
    Letter_bm = if_else(Treatment_Full == "Fallow" & is.na(Letter_bm), "", Letter_bm),
    Treatment_Full = factor(Treatment_Full, levels = c("Rye 80", "Rye 10", "Fallow"))
  )

# Dynamic scale factor calculation using back-transformed upper error bounds
max_bm  <- max(dual_plot_data$emmean_bm + dual_plot_data$SE_bm, na.rm = TRUE)
max_no3 <- max(dual_plot_data$emmean_no3 + dual_plot_data$SE_no3, na.rm = TRUE)
scale_factor <- max_bm / max_no3

# ==============================================================================
# 3. Dual-Axis Plot Generation
# ==============================================================================
ggplot(dual_plot_data, aes(x = Treatment_Full)) +
  # Primary Axis: Biomass Bars & Errors
  geom_bar(
    aes(y = emmean_bm, fill = Treatment_Full),
    stat = "identity", color = "black", alpha = 0.7, width = 0.55
  ) +
  geom_errorbar(
    aes(ymin = pmax(0, emmean_bm - SE_bm), ymax = emmean_bm + SE_bm),
    width = 0.15, linewidth = 0.6
  ) +
  geom_text(
    aes(y = emmean_bm + SE_bm, label = Letter_bm),
    vjust = -0.5, size = 3.5, family = "serif"
  ) +
  
  # Secondary Axis: Back-Transformed Nitrate Line, Points & Errors
  geom_line(
    aes(y = emmean_no3 * scale_factor, group = 1),
    color = "firebrick", linewidth = 1
  ) +
  geom_errorbar(
    aes(
      ymin = pmax(0, (emmean_no3 - SE_no3)) * scale_factor,
      ymax = (emmean_no3 + SE_no3) * scale_factor
    ),
    color = "firebrick", width = 0.15, linewidth = 0.6
  ) +
  geom_point(
    aes(y = emmean_no3 * scale_factor),
    color = "firebrick", size = 3
  ) +
  geom_text(
    aes(y = (emmean_no3 + SE_no3) * scale_factor, label = Letter_no3),
    vjust = -0.6, color = "firebrick", fontface = "bold", size = 3.5, family = "serif"
  ) +
  
  # Faceting & Dual Y Scales
  facet_wrap(~ Site) +
  scale_fill_manual(values = color_map) +
  scale_y_continuous(
    name = expression(Estimated~Mean~Biomass~(kg~ha^-1)),
    sec.axis = sec_axis(~ . / scale_factor, name = expression(Estimated~Mean~Porewater~NO[3]*"-"*N~(mg~L^-1))),
    expand = expansion(mult = c(0, 0.20))
  ) +
  
  # Aesthetics & Formatting
  labs(
    x = "Cover Crop Treatment",
    caption = paste0(
      "Bars and red points represent model back-transformed estimated marginal means (emmeans) ± 1 SE.\n",
      "Black letters: Biomass comparisons | Red letters: Porewater NO3-N comparisons (Tukey HSD, alpha = 0.05)."
    )
  ) +
  theme_bw(base_family = "serif") +
  theme(
    legend.position = "none",
    axis.text.x = element_text(angle = 45, hjust = 1, face = "bold", color = "black"),
    axis.text.y = element_text(color = "black"),
    axis.title = element_text(face = "bold"),
    strip.text = element_text(face = "bold", size = 11),
    axis.title.y.right = element_text(color = "firebrick", face = "bold"),
    axis.text.y.right  = element_text(color = "firebrick"),
    panel.grid.minor = element_blank()
  )














# ==============================================================================
# 1. Filter log-scale EMMs to Rye Treatments (+ Fallow), Run CLD & Regrid
# ==============================================================================
# Filter log EMMs to Rye levels (+ Fallow) and calculate subsetted Tukey CLD
F24_CCBM_cld_rye <- F24_CCBM_emm_log %>%
  as.emmGrid() %>%
  subset(Treatment_Full %in% c("Rye 10", "Rye 80", "Fallow")) %>%
  cld(alpha = 0.05, Letters = letters, adjust = "tukey", decreasing = TRUE)

# Back-transform EMMs to response scale and join subsetted Rye letters
F24_CCBM_plot_data_rye <- regrid(F24_CCBM_emm_log, transform = "response") %>%
  as.data.frame() %>%
  filter(Treatment_Full %in% c("Rye 10", "Rye 80", "Fallow")) %>%
  left_join(
    as.data.frame(F24_CCBM_cld_rye) %>% select(Treatment_Full, Site, .group),
    by = c("Treatment_Full", "Site")
  ) %>%
  mutate(Letter = trimws(.group))
F24_CCBM_plot_data_rye 
# ==============================================================================
# 2. Build Unified Dual-Axis Plotting Dataframe
# ==============================================================================
F24bm_S25swn_dual_plot_data <- full_join(
  # Primary Axis Data: Fall Biomass (Saint Paul & Waseca)
  F24_CCBM_plot_data_rye %>%
    filter(Site %in% c("Saint Paul", "Waseca"), Treatment_Full %in% c("Rye 80", "Rye 10", "Fallow")) %>%
    select(Site, Treatment_Full, emmean_bm = response, SE_bm = SE, Letter_bm = Letter),
  
  # Secondary Axis Data: Spring Lysimeter Nitrate (pre-regridded response scale)
  lys_plot_data %>%
    filter(Site %in% c("Saint Paul", "Waseca"), Treatment_Full %in% c("Rye 80", "Rye 10", "Fallow")) %>%
    select(Site, Treatment_Full, emmean_no3 = response, SE_no3 = SE, Letter_no3 = Letter),
  
  by = c("Site", "Treatment_Full")
) %>%
  mutate(
    # Insert zeros for unmeasured Fallow biomass control
    emmean_bm = if_else(Treatment_Full == "Fallow" & is.na(emmean_bm), 0, emmean_bm),
    SE_bm     = if_else(Treatment_Full == "Fallow" & is.na(SE_bm), 0, SE_bm),
    Letter_bm = if_else(Treatment_Full == "Fallow" & is.na(Letter_bm), "", Letter_bm),
    Treatment_Full = factor(Treatment_Full, levels = c("Rye 80", "Rye 10", "Fallow"))
  )
F24bm_S25swn_dual_plot_data
# Calculate dynamic axis scaling factor based on upper error bounds
F24bm_S25swn_scale_factor <- max(F24bm_S25swn_dual_plot_data$emmean_bm + F24bm_S25swn_dual_plot_data$SE_bm, na.rm = TRUE) /
  max(F24bm_S25swn_dual_plot_data$emmean_no3 + F24bm_S25swn_dual_plot_data$SE_no3, na.rm = TRUE)

# ==============================================================================
# 3. Dual-Axis Plot Generation
# ==============================================================================
ggplot(F24bm_S25swn_dual_plot_data, aes(x = Treatment_Full)) +
  # Primary Axis: Fall 2024 Biomass Bars & Symmetric Errors
  geom_bar(
    aes(y = emmean_bm, fill = Treatment_Full),
    stat = "identity", color = "black", alpha = 0.7, width = 0.55
  ) +
  geom_errorbar(
    aes(ymin = pmax(0, emmean_bm - SE_bm), ymax = emmean_bm + SE_bm),
    width = 0.15, linewidth = 0.6
  ) +
  geom_text(
    aes(y = emmean_bm + SE_bm, label = Letter_bm),
    vjust = -0.5, size = 3.5, family = "serif"
  ) +
  
  # Secondary Axis: Spring 2025 Porewater Nitrate Line, Points & Errors
  geom_line(
    aes(y = emmean_no3 * F24bm_S25swn_scale_factor, group = 1),
    color = "firebrick", linewidth = 1
  ) +
  geom_errorbar(
    aes(
      ymin = pmax(0, (emmean_no3 - SE_no3)) * F24bm_S25swn_scale_factor,
      ymax = (emmean_no3 + SE_no3) * F24bm_S25swn_scale_factor
    ),
    color = "firebrick", width = 0.15, linewidth = 0.6
  ) +
  geom_point(
    aes(y = emmean_no3 * F24bm_S25swn_scale_factor),
    color = "firebrick", size = 3
  ) +
  geom_text(
    aes(y = (emmean_no3 + SE_no3) * F24bm_S25swn_scale_factor, label = Letter_no3),
    vjust = -0.8, hjust = 1.2,  color = "firebrick", fontface = "bold", size = 4, family = "serif"
  ) +
  
  # Faceting & Secondary Y-Axis Transformation
  facet_wrap(~ Site) +
  scale_fill_manual(values = color_map) +
  scale_y_continuous(
    name = expression(Estimated~Mean~Fall~Biomass~(kg~ha^-1)),
    sec.axis = sec_axis(
      ~ . / F24bm_S25swn_scale_factor,
      name = expression(Estimated~Mean~Porewater~NO[3]*"-"*N~(mg~L^-1))
    ),
    expand = expansion(mult = c(0, 0.20))
  ) +
  
  # Aesthetics
  labs(
    x = "Treatment",
    caption = paste0(
      "Bars and red points represent back-transformed emmeans ± 1 SE.\n",
      "Black letters: Fall 2024 Biomass comparisons | Red letters: Spring 2025 Porewater NO3-N comparisons (Tukey HSD, alpha = 0.05)."
    )
  ) +
  theme_bw(base_family = "serif") +
  theme(
    legend.position = "none",
    axis.text.x = element_text(angle = 45, hjust = 1, face = "bold", color = "black"),
    axis.text.y = element_text(color = "black"),
    axis.title = element_text(face = "bold"),
    strip.text = element_text(face = "bold", size = 11),
    axis.title.y.right = element_text(color = "firebrick", face = "bold"),
    axis.text.y.right  = element_text(color = "firebrick"),
    panel.grid.minor = element_blank()
  )











