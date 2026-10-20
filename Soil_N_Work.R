
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
library("magick")
library("patchwork")
library("broom")



#---- COLOR PALETTE FOR TREATMENTS AND TREATMENT TYPE----
# Refined custom palette: balanced for lightness steps and high color-contrast
color_map <- c(
  "Fallow"  = "#4D4D4D",  # Medium-dark charcoal (high contrast against white, distinct from black)
  
  # Oats/Radish (Warm Oranges/Browns, stepping from light-vibrant to dark)
  "Oat/Radish 10/1" = "#FDBE85",  # Light warm orange
  "Oat/Radish 20/2" = "#FD8D3C",  # Medium orange
  "Oat/Radish 40/4" = "#E6550D",  # Deep rust orange
  "Oat/Radish 60/6" = "#A63603",  # Dark brown-orange
  
  # Cereal Rye (Cool Blues, stepping from light-vibrant to dark)
  
  "Rye 10" = "#BDD7E7",  # (Light blue)
  "Rye 20" = "#6BAED6",  # Medium blue
  "Rye 40" = "#3182BD",  # Deep royal blue
  "Rye 80" = "#08519C"   # Dark navy blue
)

# Option 1: Academic High-Contrast (Clean, vivid blue and amber/orange)
color_map_type = c(
  "No CC"        = "#636363", # Soft charcoal
  "Oat + Radish" = "#E6550D", # Vivid amber-orange
  "Cereal Rye"   = "#3182BD"  # Mid-tone steel blue
)

#----OBJECT REMOVAL, ENVIRONMENT CLEANUP----

rm(list = c("plot_site_nitrate"))



#---- Fall 2024 (F24) - Spring 2025 (S25)----


#----READING IN THE DATA----

#----F24 3-DEPTH SOIL N----

F24_3depth_soilN = read.csv("F24_Soil_N.csv")

S25_3depth_soilN = read.csv("S25_Soil_N.csv")

F25_3depth_soilN = read.csv("F25_Soil_N.csv")
  
S26_3depth_soilN = read.csv("S26_Soil_N.csv")


#----SETTING FACTOR LEVELS----

F24_3depth_soilN = F24_3depth_soilN %>% 
  mutate(
    Date = as.Date(Date, format = "%Y-%m-%d"),
    Site = factor(Site, levels = c("Saint Paul", "Waseca", "Lamberton")),
    Rep = factor(Rep),
    Depth_in = factor(Depth_in, levels = c("0-6", "6-12", "12-24")),
    Depth_cm = dplyr::recode(Depth_in,
                             "0-6"   = "0-15",
                             "6-12"  = "15-30",
                             "12-24" = "30-60"),
    Trt = factor(Trt, levels = c("C", "R1", "R2", "R3", "R4", "O1", "O2", "O3", "O4")),
    Trt_type = factor(Trt_type),
    Season_Full = case_when(
      Season == "F24" ~ "Fall 2024"
    ),
    Treatment_Full = case_when(
      Trt == "C"  ~ "Fallow",
      Trt == "R1" ~ "Rye 10",
      Trt == "R2" ~ "Rye 20",
      Trt == "R3" ~ "Rye 40",
      Trt == "R4" ~ "Rye 80",
      Trt == "O1" ~ "Oat/Radish 10/1",
      Trt == "O2" ~ "Oat/Radish 20/2",
      Trt == "O3" ~ "Oat/Radish 40/4",
      Trt == "O4" ~ "Oat/Radish 60/6",
      TRUE        ~ NA_character_ # This catches any codes that don't match
    ),
    Treatment_Full = factor(Treatment_Full, levels = c(
      "Fallow", 
      "Rye 10", "Rye 20", "Rye 40", "Rye 80", 
      "Oat/Radish 10/1", "Oat/Radish 20/2", "Oat/Radish 40/4", "Oat/Radish 60/6"
    )),
    Trt_type = factor(Trt_type, levels = c("No CC", "Cereal Rye", "Oat + Radish"))
  )

view(F24_3depth_soilN)



S25_3depth_soilN = S25_3depth_soilN %>% 
  mutate(
    Date = as.Date(Date, format = "%Y-%m-%d"),
    Site = factor(Site, levels = c("Saint Paul", "Waseca", "Lamberton")),
    Rep = factor(Rep),
    Depth_in = factor(Depth_in, levels = c("0-6", "6-12", "12-24")),
    Depth_cm = dplyr::recode(Depth_in,
                             "0-6"   = "0-15",
                             "6-12"  = "15-30",
                             "12-24" = "30-60"),
    Trt = factor(Trt, levels = c("C", "R1", "R2", "R3", "R4", "O1", "O2", "O3", "O4")),
    Trt_type = factor(Trt_type),
    Season_Full = case_when(
      Season == "S25" ~ "Spring 2025"
    ),
    Treatment_Full = case_when(
      Trt == "C"  ~ "Fallow",
      Trt == "R1" ~ "Rye 10",
      Trt == "R2" ~ "Rye 20",
      Trt == "R3" ~ "Rye 40",
      Trt == "R4" ~ "Rye 80",
      Trt == "O1" ~ "Oat/Radish 10/1",
      Trt == "O2" ~ "Oat/Radish 20/2",
      Trt == "O3" ~ "Oat/Radish 40/4",
      Trt == "O4" ~ "Oat/Radish 60/6",
      TRUE        ~ NA_character_ # This catches any codes that don't match
    ),
    Treatment_Full = factor(Treatment_Full, levels = c(
      "Fallow", 
      "Rye 10", "Rye 20", "Rye 40", "Rye 80", 
      "Oat/Radish 10/1", "Oat/Radish 20/2", "Oat/Radish 40/4", "Oat/Radish 60/6"
    )),
    Trt_type = factor(Trt_type, levels = c("No CC", "Cereal Rye", "Oat + Radish"))
  )





F25_3depth_soilN = F25_3depth_soilN %>% 
  mutate(
    Date = as.Date(Date, format = "%Y-%m-%d"),
    Site = factor(Site, levels = c("Saint Paul", "Waseca", "Lamberton")),
    Rep = factor(Rep),
    Depth_in = factor(Depth_in, levels = c("0-6", "6-12", "12-24")),
    Depth_cm = dplyr::recode(Depth_in,
                             "0-6"   = "0-15",
                             "6-12"  = "15-30",
                             "12-24" = "30-60"),
    Trt = factor(Trt, levels = c("C", "R1", "R2", "R3", "R4", "O1", "O2", "O3", "O4")),
    Trt_type = factor(Trt_type),
    Season_Full = case_when(
      Season == "F25" ~ "Fall 2025"
    ),
    Treatment_Full = case_when(
      Trt == "C"  ~ "Fallow",
      Trt == "R1" ~ "Rye 10",
      Trt == "R2" ~ "Rye 20",
      Trt == "R3" ~ "Rye 40",
      Trt == "R4" ~ "Rye 80",
      Trt == "O1" ~ "Oat/Radish 10/1",
      Trt == "O2" ~ "Oat/Radish 20/2",
      Trt == "O3" ~ "Oat/Radish 40/4",
      Trt == "O4" ~ "Oat/Radish 60/6",
      TRUE        ~ NA_character_ # This catches any codes that don't match
    ),
    Treatment_Full = factor(Treatment_Full, levels = c(
      "Fallow", 
      "Rye 10", "Rye 20", "Rye 40", "Rye 80", 
      "Oat/Radish 10/1", "Oat/Radish 20/2", "Oat/Radish 40/4", "Oat/Radish 60/6"
    )),
    Trt_type = factor(Trt_type, levels = c("No CC", "Cereal Rye", "Oat + Radish"))
  )






S26_3depth_soilN = S26_3depth_soilN %>% 
  mutate(
    Date = as.Date(Date, format = "%Y-%m-%d"),
    Site = factor(Site, levels = c("Saint Paul", "Waseca", "Lamberton")),
    Rep = factor(Rep),
    Depth_in = factor(Depth_in, levels = c("0-6", "6-12", "12-24")),
    Depth_cm = dplyr::recode(Depth_in,
                             "0-6"   = "0-15",
                             "6-12"  = "15-30",
                             "12-24" = "30-60"),
    Trt = factor(Trt, levels = c("C", "R1", "R2", "R3", "R4", "O1", "O2", "O3", "O4")),
    Trt_type = factor(Trt_type),
    Season_Full = case_when(
      Season == "S26" ~ "Spring 2026"
    ),
    Treatment_Full = case_when(
      Trt == "C"  ~ "Fallow",
      Trt == "R1" ~ "Rye 10",
      Trt == "R2" ~ "Rye 20",
      Trt == "R3" ~ "Rye 40",
      Trt == "R4" ~ "Rye 80",
      Trt == "O1" ~ "Oat/Radish 10/1",
      Trt == "O2" ~ "Oat/Radish 20/2",
      Trt == "O3" ~ "Oat/Radish 40/4",
      Trt == "O4" ~ "Oat/Radish 60/6",
      TRUE        ~ NA_character_ # This catches any codes that don't match
    ),
    Treatment_Full = factor(Treatment_Full, levels = c(
      "Fallow", 
      "Rye 10", "Rye 20", "Rye 40", "Rye 80", 
      "Oat/Radish 10/1", "Oat/Radish 20/2", "Oat/Radish 40/4", "Oat/Radish 60/6"
    )),
    Trt_type = factor(Trt_type, levels = c("No CC", "Cereal Rye", "Oat + Radish"))
  )


#----SUMMING THE DEPTHS----

F24_SoilN_depth_summed <- F24_3depth_soilN %>%
  group_by(Site, Season, Rep, Plot, Date, Trt, Treatment_Full, Trt_type ) %>%
  summarize(
    Total_N_kgha = sum(N_totindep_kgha, na.rm = TRUE),
    Total_NitN_kgha = sum(Nitrate_NitriteN_kgha, na.rm = TRUE),
    Total_Amm_kgha = sum(AmmoniumN_kgha, na.rm = TRUE),
    Total_N_lbac = sum(N_totindep_lbac, na.rm = TRUE),
    Total_NitN_lbac = sum(Nitrate_NitriteN_lbac, na.rm = TRUE),
    Total_Amm_lbac = sum(AmmoniumN_lbac, na.rm = TRUE),
    .groups = "drop")


S25_SoilN_depth_summed <- S25_3depth_soilN %>%
  group_by(Site, Season,Rep , Plot, Date, Trt, Treatment_Full, Trt_type ) %>%
  summarize(
    Total_N_kgha = sum(N_totindep_kgha, na.rm = TRUE),
    Total_NitN_kgha = sum(Nitrate_NitriteN_kgha, na.rm = TRUE),
    Total_Amm_kgha = sum(AmmoniumN_kgha, na.rm = TRUE),
    Total_N_lbac = sum(N_totindep_lbac, na.rm = TRUE),
    Total_NitN_lbac = sum(Nitrate_NitriteN_lbac, na.rm = TRUE),
    Total_Amm_lbac = sum(AmmoniumN_lbac, na.rm = TRUE),
    .groups = "drop")


F25_SoilN_depth_summed <- F25_3depth_soilN %>%
  group_by(Site, Season,Rep , Plot, Date, Trt, Treatment_Full, Trt_type ) %>%
  summarize(
    Total_N_kgha = sum(N_totindep_kgha, na.rm = TRUE),
    Total_NitN_kgha = sum(Nitrate_NitriteN_kgha, na.rm = TRUE),
    Total_Amm_kgha = sum(AmmoniumN_kgha, na.rm = TRUE),
    Total_N_lbac = sum(N_totindep_lbac, na.rm = TRUE),
    Total_NitN_lbac = sum(Nitrate_NitriteN_lbac, na.rm = TRUE),
    Total_Amm_lbac = sum(AmmoniumN_lbac, na.rm = TRUE),
    .groups = "drop")


S26_SoilN_depth_summed <- S26_3depth_soilN %>%
  group_by(Site, Season,Rep , Plot, Date, Trt, Treatment_Full, Trt_type ) %>%
  summarize(
    Total_N_kgha = sum(N_totindep_kgha, na.rm = TRUE),
    Total_NitN_kgha = sum(Nitrate_NitriteN_kgha, na.rm = TRUE),
    Total_Amm_kgha = sum(AmmoniumN_kgha, na.rm = TRUE),
    Total_N_lbac = sum(N_totindep_lbac, na.rm = TRUE),
    Total_NitN_lbac = sum(Nitrate_NitriteN_lbac, na.rm = TRUE),
    Total_Amm_lbac = sum(AmmoniumN_lbac, na.rm = TRUE),
    .groups = "drop")

#----CREATING F24-S25 SUMMED DATA WITH SEASONALLY SUBTRACTED N AND BIOMASS VALUES


# 1. Prepare and rename Fall datasets (parsing Date origins)
F24_SoiNXAGBM_dat <- inner_join(
  F24_SoilN_depth_summed %>% rename(Date_N_samp_F24 = Date), 
  F24_AGBM_CN %>% rename(Date_BM_harvest_F24 = Date), 
  by = c("Site", "Rep", "Plot", "Trt", "Treatment_Full")
) %>%
  rename(
    Total_N_kgha_F24 = Total_N_kgha,
    Total_N_lbac_F24 = Total_N_lbac,
    CC_AGBM_kgha_F24  = AGBM_kgha,
    CC_AGBM_lbac_F24 = AGBM_lbac,
    CC_AGBM_mgha_F24 = AGBM_mgha,
    BMN_kgha_F24 = N_kgha,
    BMN_lbac_F24 = N_lbac,
    CN_rat_F24 = CN_rat
  ) %>%
  select(
    Site, Rep, Plot, Trt, Treatment_Full, 
    Date_N_samp_F24, Date_BM_harvest_F24,
    Total_N_kgha_F24, Total_N_lbac_F24,
    CC_AGBM_kgha_F24, CC_AGBM_lbac_F24,CC_AGBM_mgha_F24, BMN_kgha_F24, BMN_lbac_F24, CN_rat_F24
  )

# 2. Prepare and rename Spring datasets (parsing Date origins)
S25_SoiNXAGBM_dat <- inner_join(
  S25_SoilN_depth_summed %>% rename(Date_N_samp_S25 = Date), 
  S25_AGBM_CN %>% rename(Date_BM_harvest_S25 = Date), 
  by = c("Site", "Rep", "Plot", "Trt", "Treatment_Full")
) %>%
  rename(
    Total_N_kgha_S25 = Total_N_kgha,
    Total_N_lbac_S25 = Total_N_lbac,
    CC_AGBM_kgha_S25  = AGBM_kgha,
    CC_AGBM_lbac_S25 = AGBM_lbac,
    CC_AGBM_mgha_S25 = AGBM_mgha,
    BMN_kgha_S25 = N_kgha,
    BMN_lbac_S25 = N_lbac,
    CN_rat_S25 = CN_rat
  ) %>%
  select(
    Site, Rep, Plot, Trt, Treatment_Full, 
    Date_N_samp_S25, Date_BM_harvest_S25,
    Total_N_kgha_S25, Total_N_lbac_S25,
    CC_AGBM_kgha_S25, CC_AGBM_lbac_S25,CC_AGBM_mgha_S25, BMN_kgha_S25, BMN_lbac_S25, CN_rat_S25
  )

# 3. Merge seasons, calculate seasonal differences and elapsed day intervals
F24XS25_SoiNXAGBM_diff_dat <- inner_join(
  F24_SoiNXAGBM_dat, S25_SoiNXAGBM_dat, 
  by = c("Site", "Rep", "Plot", "Trt", "Treatment_Full")
) %>%
  mutate(
    # Soil N and Biomass Deltas
    Soil_N_diff_kgha = Total_N_kgha_F24 - Total_N_kgha_S25,
    Soil_N_diff_lbac = Total_N_lbac_F24 - Total_N_lbac_S25,
    CC_BM_diff_kgha  = CC_AGBM_kgha_S25  - CC_AGBM_kgha_F24,
    CC_BM_diff_lbac  = CC_AGBM_lbac_S25  - CC_AGBM_lbac_F24,
    CC_BM_diff_mgha  = CC_AGBM_mgha_S25  - CC_AGBM_mgha_F24,
    CC_N_diff_kgha   = BMN_kgha_S25 - BMN_kgha_F24,
    CC_N_diff_lbac   = BMN_lbac_S25 - BMN_lbac_F24,
    CN_rat_perc_chan = ((CN_rat_S25 - CN_rat_F24) / CN_rat_F24) * 100,

    # Over-winter Temporal Interval (Spring minus Fall for soil cores and biomass)
    SoilN_interval_days  = as.numeric(difftime(as.Date(Date_N_samp_S25), as.Date(Date_N_samp_F24), units = "days")),
    BM_harvest_interval_days = as.numeric(difftime(as.Date(Date_BM_harvest_S25), as.Date(Date_BM_harvest_F24), units = "days"))
  )

F24XS25_SoiNXAGBM_diff_dat <- F24XS25_SoiNXAGBM_diff_dat %>%
  filter(!(Site == "Lamberton" & Trt %in% c("C", "O1", "O2", "O3", "O4")))

view(F24XS25_SoiNXAGBM_diff_dat)

#----N DIFF MOD FOR----

S25F24_BMN_Ndiff_MOD = lmer(Soil_N_diff_kgha ~ CC_N_diff_kgha + (1|Site/Rep), 
                                                               data = F24XS25_SoiNXAGBM_diff_dat)
anova(S25F24_BMN_Ndiff_MOD, type = 3)

ggplot(data = F24XS25_SoiNXAGBM_diff_dat, aes(x = CC_N_diff_kgha, y = Soil_N_diff_kgha ) ) +
  # Map point color to Treatment_Full
  geom_point(aes(color = Treatment_Full, size = CC_AGBM_kgha_S25), alpha = 0.8) +
  # Keep trendline global across all points
  geom_smooth(method = "lm", color = "black", se = TRUE, linewidth = 0.8) +
  # Apply your custom color palette to the color aesthetic
  scale_color_manual(values = color_map) +
  scale_size_continuous(
    name = expression(bold("Biomass (kg ha"^{-1}*")")),
    range = c(1, 8)
  ) +
  labs(
    x = expression("Spring 2025 - Fall 2024 CC Biomass N Difference (kg ha"^{-1}*")"),
    y = expression("Fall 2024 - Spring 2025 Soil N Difference (kg ha"^{-1}*")"),
    color = "Treatment",
    size = "S25 CC Biomass"
  ) +
  theme_bw(base_family = "serif") +
  theme(
    panel.grid.minor = element_blank(),
    axis.text = element_text(size = 10),
    axis.title = element_text(size = 11, face = "bold"),
    legend.title = element_text(face = "bold", size = 10),
    legend.position = "right"
  )


#----EXPLORING THE DATA----
# ==============================================================================
# 1. Generate Descriptive Statistics Dataframe
# ==============================================================================
F24_N_summary <- F24_3depth_soilN %>%
  group_by(Site, Depth_cm, Treatment_Full) %>%
  summarize(
    n    = sum(!is.na(Nitrate_NitriteN_kgha)),
    mean = mean(Nitrate_NitriteN_kgha, na.rm = TRUE),
    sd   = sd(Nitrate_NitriteN_kgha, na.rm = TRUE),
    # Standard Error of the Mean: SE = SD / sqrt(n)
    se   = sd / sqrt(n),
    # Coefficient of Variation: CV% = (SD / Mean) * 100
    cv   = (sd / mean) * 100,
    .groups = "drop"
  )

# ==============================================================================
# 2. Export as Styled Table
# ==============================================================================
F24_N_summary %>%
  mutate(
    # Format metrics for standard scientific display
    mean_se = sprintf("%.2f ± %.2f", mean, se),
    cv_pct  = sprintf("%.1f%%", cv)
  ) %>%
  select(Site, Depth_cm, Treatment_Full, n, mean_se, cv_pct) %>%
  kable(
    format = "html",
    caption = "Descriptive statistics for Fall 2024 soil nitrate-nitrogen concentrations (kg/ha) by site, depth, and cover crop treatment.",
    col.names = c("Site", "Depth (cm)", "Treatment", "n", "Mean ± SE", "CV (%)"),
    align = "llclcc"
  ) %>%
  kable_styling(
    bootstrap_options = c("striped", "hover", "condensed"),
    full_width = FALSE,
    position = "left"
  ) %>%
  collapse_rows(columns = 1:2, valign = "top")


#----IN PROGRESS: MAKING AN EXPORTABLE TABLE DIRECTLY TO MY COMPUTER----

# 1. Define the absolute path explicitly (ensuring quotes match perfectly)
output_dir <- "/Users/robertfowler/Documents/CC Outcomes/Y2_NRCS_new_analysis/NRCS_new_plots"

# 2. Safely check and create the directory if it doesn't exist
if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
}


F24_N_table <- F24_N_summary %>%
  mutate(
    mean_se = sprintf("%.2f ± %.2f", mean, se),
    cv_pct  = sprintf("%.1f%%", cv)
  ) %>%
  select(Site, Depth_cm, Treatment_Full, n, mean_se, cv_pct) %>%
  kable(
    format = "html",
    caption = "Descriptive statistics for Fall 2024 soil nitrate-nitrogen concentrations (kg/ha) by site, depth, and cover crop treatment.",
    col.names = c("Site", "Depth (cm)", "Treatment", "n", "Mean ± SE", "CV (%)"),
    align = "llclcc"
  ) %>%
  kable_styling(
    bootstrap_options = c("striped", "hover", "condensed"),
    full_width = FALSE,
    position = "left"
  ) %>%
  collapse_rows(columns = 1:2, valign = "top")

# 3. Export to the specific folder using file.path()
save_kable(
  F24_N_table, 
  file = file.path(output_dir, "F24_N_summary.png"), 
  zoom = 2
)

#----SOIL NITRATE EXPLORATORY BOXPLOT----
# ==============================================================================
# Refined Soil Nitrate Boxplot (Linear Scale, High-Contrast Facets)
# ==============================================================================
ggplot(F24_3depth_soilN, aes(x = Treatment_Full, y = Nitrate_NitriteN_kgha, fill = Treatment_Full)) +
  # Boxplot layer with clean lines and no underlying raw data points
  geom_boxplot(outlier.shape = 16, outlier.size = 1.5, alpha = 0.85, color = "black") +
  
  # Facet grid: Depth vertically (metric), Site horizontally
  facet_grid(Depth_cm ~ Site, scales = "free_y") +
  
  # Custom treatment color mapping
  scale_fill_manual(values = color_map) +
  
  # Y-axis label using corrected R mathematical expression syntax
  labs(
    x = "Cover Crop Treatment",
    y = expression(Nitrate/Nitrite-N~(kg~ha^-1))
  ) +
  
  # Set serif font family globally
  theme_bw(base_family = "serif") +
  
  # Visual adjustments for facet banners and labels
  theme(
    legend.position = "none",
    
    # Rotate and align long treatment names on the horizontal axis
    axis.text.x = element_text(angle = 45, hjust = 1, size = 9, color = "black"),
    axis.text.y = element_text(size = 9, color = "black"),
    axis.title  = element_text(face = "bold", size = 11, color = "black"),
    
    # Facet strips: Light grey background with a crisp, solid black border
    strip.background = element_rect(fill = "grey90", color = "black", linewidth = 0.7),
    strip.text       = element_text(face = "bold", size = 10, color = "black"),
    
    # Clean up background grid lines
    panel.grid.major = element_line(color = "grey95"),
    panel.grid.minor = element_blank()
  )


#----MODEL----

F24_TrtXDepXSi_MOD = lmer(Nitrate_NitriteN_kgha ~ Trt * Depth_cm * Site + (1|Site:Rep),
                              data = F24_3depth_soilN)

#----CHECKING MODEL----
# 1. Set up a 1-row, 2-column plotting grid
par(mfrow = c(1, 2), family = "serif")

# 2. Residuals vs. Fitted (Tests Homoskedasticity / Constant Variance)
plot(fitted(F24_TrtXDepXSi_MOD), residuals(F24_TrtXDepXSi_MOD),
     xlab = "Fitted Values", ylab = "Residuals",
     main = "Residuals vs. Fitted")
abline(h = 0, col = "red", lty = 2)

# 3. Normal Q-Q Plot (Tests Normality of Residuals)
qqnorm(residuals(F24_TrtXDepXSi_MOD), main = "Normal Q-Q Plot")
qqline(residuals(F24_TrtXDepXSi_MOD), col = "red", lty = 2)

# Reset plotting grid
par(mfrow = c(1, 1))

#----TRANSFORMING THE MODEL----
# 1. Fit the log-transformed model (using the cleaned data)
F24_TrtXDepXSi_logMOD <- lmer(
  log(Nitrate_NitriteN_kgha) ~ Treatment_Full * Depth_cm * Site + (1|Site:Rep),
  data = F24_3depth_soilN
)

# 2. Run diagnostics on the log-transformed model to verify correction
par(mfrow = c(1, 2), family = "serif")
plot(fitted(F24_TrtXDepXSi_logMOD), residuals(F24_TrtXDepXSi_logMOD),
     xlab = "Fitted Values", ylab = "Residuals", main = "Log-Model: Residuals vs. Fitted")
abline(h = 0, col = "red", lty = 2)

qqnorm(residuals(F24_TrtXDepXSi_logMOD), main = "Log-Model: Q-Q Plot")
qqline(residuals(F24_TrtXDepXSi_logMOD), col = "red", lty = 2)
par(mfrow = c(1, 1))


#----VARIANCE COMPONENTS----
# Extract and calculate the ICC
vc <- as.data.frame(VarCorr(F24_TrtXDepXSi_logMOD))
sigma2_block <- vc$vcov[vc$grp == "Site:Rep"]
sigma2_resid <- vc$vcov[vc$grp == "Residual"]

icc <- sigma2_block / (sigma2_block + sigma2_resid)
cat("Blocking accounted for", round(icc * 100, 1), "% of the total random variation.\n")

performance::icc(F24_TrtXDepXSi_logMOD)

#----MODEL SUMMARY AND ANOVA----
# 1. Generate the standard model parameter summary
summary(F24_TrtXDepXSi_logMOD)

#----EXPONENTIATE FOR TRANSFORMED MODELS

exp(fixef(F24_TrtXDepXSi_logMOD))


# 2. Run Type III ANOVA using Satterthwaite ddf
F24_TrtXDepXSi_logANOVA <- anova(F24_TrtXDepXSi_logMOD, type = 3, ddf = "Satterthwaite")
print(F24_TrtXDepXSi_logANOVA)




#----FORMATTED ANOVA AND EXPORT----


# ==============================================================================
# 1. Clean, Round, and Format ANOVA Table (with Safe HTML Entities)
# ==============================================================================
F24_SoilNit_anova <-F24_TrtXDepXSi_logANOVA %>%
  as.data.frame() %>%
  tibble::rownames_to_column("Source") %>%
  # Clean up the Source names
  mutate(
    Source = gsub("Depth_cm", "Depth", Source),
    Source = gsub("Treatment_Full|Trt", "Treatment", Source),
    Source = gsub(":", " × ", Source)
  ) %>%
  # Safely round the numerical columns
  mutate(
    `Sum Sq`  = round(`Sum Sq`, 2),
    `Mean Sq` = round(`Mean Sq`, 2),
    NumDF     = round(NumDF, 0),
    DenDF     = round(DenDF, 2),
    `F value` = round(`F value`, 2)
  ) %>%
  # Format p-values using &lt; instead of < to prevent HTML tag clipping
  mutate(
    `p-value` = case_when(
      is.na(`Pr(>F)`)   ~ "NS",
      `Pr(>F)` < 0.001  ~ "<b>&lt; 0.001</b>", # HTML-safe less-than
      `Pr(>F)` < 0.05   ~ sprintf("<b>%.4f</b>", `Pr(>F)`),
      TRUE              ~ sprintf("%.4f", `Pr(>F)`)
    )
  ) %>%
  # Keep only display-ready columns
  select(Source, `Sum Sq`, `Mean Sq`, NumDF, DenDF, `F value`, `p-value`)

# ==============================================================================
# 2. Export as High-Contrast HTML Table
# ==============================================================================
F24_SoilNit_anova %>%
  kable(
    format = "html",
    caption = "Type III Analysis of Variance with Satterthwaite's method for soil nitrate-N 
    (log-transformed kg ha<sup>-1</sup>) in fall 2024. Bold p-values indicate statistical significance (alpha = 0.05). 
    Data was analyzed as a RCBD with the form: lmer(log(Nitrate) ~ Treatment * Depth * Site + (1|Site:Rep)).",
    col.names = c("Source of Variation", "Sum Sq", "Mean Sq", "Num Df", "Den Df", "F value", "p-value"),
    align = "lcccccc",
    escape = FALSE # Crucial: parses <b> and &lt; correctly
  ) %>%
  kable_styling(
    bootstrap_options = c("striped", "hover", "condensed"),
    full_width = FALSE,
    position = "left"
  ) %>%
  row_spec(0, bold = TRUE, color = "black")

#----EXTRACTING EMMEANS AND CLD, BACK-TRANSFORMING----

# ==============================================================================
# 1. Back-Transform using regrid() for Symmetric Standard Errors (response ± SE)
# ==============================================================================
# First, estimate marginal means on the log scale
F24_SoilNit_emm_log <- emmeans(
  F24.TrtXDepXSi.met.LOG_MOD, 
  ~ Treatment_Full|Depth_cm * Site)

F24_SoilNit_emm_log %>% 
  summary(infer = TRUE, type = "response")

F24_SoilNit_emm_log %>% 
  summary(infer = TRUE)

F24_SoilNit_contrasts <- pairs(F24_SoilNit_emm_log, adjust = "tukey")
print(F24_SoilNit_contrasts)

# Perform Tukey comparisons and get CLD letters on the log scale (where assumptions hold)
F24_SoilNit_cld <- cld(
  F24_SoilNit_emm_log, 
  alpha = 0.05, 
  Letters = letters, 
  adjust = "tukey", 
  decreasing = TRUE)



# Convert the grid to the response scale using regrid() for symmetric SEs
# Then merge the letters back based on all indexing factors
F24_SoilNit_plot_data <- regrid(F24_SoilNit_emm_log, transform = "response") %>%
  as.data.frame() %>%
  left_join(
    as.data.frame(F24_SoilNit_cld) %>% 
      select(Treatment_Full, Depth_cm, Site, .group), 
    by = c("Treatment_Full", "Depth_cm", "Site")
  ) %>%
  mutate(Letter = trimws(.group))

print(F24_SoilNit_plot_data, n = 81)

#----CLUTTERED, FACET-WRAPPED PLOTS WITH CLD LETTERS CUT OFF----
ggplot(F24_SoilNit_plot_data, aes(x = Treatment_Full, y = response, fill = Treatment_Full)) +
  geom_bar(stat = "identity", color = "black", alpha = 0.7) +
  # Symmetrical Delta-method SE bars
  geom_errorbar(aes(ymin = response - SE, ymax = response + SE), width = 0.2) +
  # Letter positioning safely above the SE bar
  geom_text(aes(label = Letter, y = response + SE), vjust = -0.5, size = 3) +
  # 3x3 wrapped grid (Site x Depth) with independent y-axes
  facet_wrap(Site ~ Depth_cm, scales = "free_y", ncol = 3) + 
  scale_fill_viridis_d(option = "mako") +
  labs(
    y = expression("Estimated Mean Soil Nitrate-N (kg ha"^{-1}*")"), # Adjust variable name as needed
    x = "Cover Crop Treatment",
    caption = paste0(
      "Bars represent back-transformed emmeans.\n",
      "Error bars represent Delta-method approximated ±1 SE.\n",
      "Letters indicate significant differences within each site and depth combination (Tukey HSD, alpha = 0.05)."
    )
  ) +
  theme_bw(base_family = "serif") +
  theme(
    legend.position = "none", 
    axis.text.x = element_text(angle = 45, hjust = 1, size = 9),
    strip.text = element_text(face = "bold", size = 10)
  )

#----WEIRD NEW PIPELINE FOR SITE-SPECIFIC GRAPHS----



# ==============================================================================
# 1. Simplified Data Wrangling (No complex indexing or middle-bar math)
# ==============================================================================
F24_SoilNit_plot_data <- F24_SoilNit_plot_data %>%
  group_by(Site, Depth_cm) %>%
  mutate(
    # Count unique letters in this site-depth cluster
    Is_Sig = n_distinct(Letter) > 1,
    
    # Layer 1: Individual letters (blanked out if the group is non-significant)
    Bar_Letter = if_else(Is_Sig, Letter, ""),
    
    # Layer 2: Single "n.s." label (placed only on the first row of a non-sig cluster)
    Cluster_NS = if_else(!Is_Sig & row_number() == 1, "n.s.", ""),
    
    # Find the maximum height in this cluster to float the "n.s." label safely
    Cluster_Max_Y = max(response + SE)
  ) %>%
  ungroup()

# ==============================================================================
# 2. Simplified Plotting Function (No complex positioning for n.s.)
# ==============================================================================
F24_plot_site_nitrate <- function(site_name) {
  F24_SoilNit_plot_data %>%
    filter(Site == site_name) %>%
    
    ggplot(aes(x = Depth_cm, y = response, fill = Treatment_Full)) +
    # Clustered Bars and Error Bars
    geom_bar(stat = "identity", position = position_dodge(width = 0.8), color = "black", alpha = 0.7, width = 0.7) +
    geom_errorbar(
      aes(ymin = pmax(0, response - SE), ymax = response + SE),
      position = position_dodge(width = 0.8),
      width = 0.2
    ) +
    
    # TEXT LAYER 1: Individual Tukey letters (placed only if significant, dodged)
    geom_text(
      aes(label = Bar_Letter, y = response + SE),
      position = position_dodge(width = 0.8),
      vjust = -0.5,
      size = 3.2,
      fontface = "bold"
    ) +
    
    # TEXT LAYER 2: Single "n.s." label (undodged; naturally centers on the x-axis category)
    geom_text(
      aes(label = Cluster_NS, y = Cluster_Max_Y),
      vjust = -0.8,
      size = 3.5,
      fontface = "bold"
    ) +
    
    # Aesthetics & Theme
    scale_fill_manual(values = color_map) +
    scale_y_continuous(expand = expansion(mult = c(0.01, 0.22))) +
    labs(
      title = paste("Fall 2024 Soil Nitrate-N Profile:", site_name),
      y = expression("Estimated Mean Soil Nitrate-N (kg ha"^{-1}*")"),
      x = "Soil Depth Horizon",
      caption = paste0(
        "Bars represent back-transformed emmeans.\n",
        "Error bars represent Delta-method approximated ±1 SE.\n",
        "Letters indicate significant differences within each site and depth combination (Tukey HSD, alpha = 0.05)."
      ),
      fill = "Cover Crop Treatment"
    ) +
    theme_bw(base_family = "serif") +
    theme(
      plot.title = element_text(face = "bold", size = 12, hjust = 0.5),
      legend.position = "bottom",
      legend.title = element_text(face = "bold", size = 9),
      legend.text = element_text(size = 8),
      axis.text = element_text(color = "black", size = 9),
      panel.grid.major.x = element_blank()
    ) +
    guides(fill = guide_legend(ncol = 3))
}

# ==============================================================================
# 3. Regenerate the Standalone Objects
# ==============================================================================
F24_nitrate_plot_StPaul    <- F24_plot_site_nitrate("Saint Paul")
F24_nitrate_plot_Waseca    <- F24_plot_site_nitrate("Waseca")
F24_nitrate_plot_Lamberton <- F24_plot_site_nitrate("Lamberton")

# View a plot in RStudio
F24_nitrate_plot_StPaul 
F24_nitrate_plot_Waseca    
F24_nitrate_plot_Lamberton











#----F24 AMMNONIUM----


#----EXPLORING THE DATA----
# ==============================================================================
# 1. Generate Descriptive Statistics Dataframe
# ==============================================================================
F24_amm_summary <- F24_3depth_soilN %>%
  group_by(Site, Depth_cm, Treatment_Full) %>%
  summarize(
    n    = sum(!is.na(AmmoniumN_kgha)),
    mean = mean(AmmoniumN_kgha, na.rm = TRUE),
    sd   = sd(AmmoniumN_kgha, na.rm = TRUE),
    # Standard Error of the Mean: SE = SD / sqrt(n)
    se   = sd / sqrt(n),
    # Coefficient of Variation: CV% = (SD / Mean) * 100
    cv   = (sd / mean) * 100,
    .groups = "drop" )

# ==============================================================================
# 2. Export as Styled Table
# ==============================================================================
F24_amm_summary %>%
  mutate(
    # Format metrics for standard scientific display
    mean_se = sprintf("%.2f ± %.2f", mean, se),
    cv_pct  = sprintf("%.1f%%", cv)
  ) %>%
  select(Site, Depth_cm, Treatment_Full, n, mean_se, cv_pct) %>%
  kable(
    format = "html",
    caption = "Descriptive statistics for Fall 2024 soil Ammonium-N (kg/ha) by site, depth, and cover crop treatment.",
    col.names = c("Site", "Depth (cm)", "Treatment", "n", "Mean ± SE", "CV (%)"),
    align = "llclcc"
  ) %>%
  kable_styling(
    bootstrap_options = c("striped", "hover", "condensed"),
    full_width = FALSE,
    position = "left"
  ) %>%
  collapse_rows(columns = 1:2, valign = "top")


#----IN PROGRESS: MAKING AN EXPORTABLE TABLE DIRECTLY TO MY COMPUTER----

# 1. Define the absolute path explicitly (ensuring quotes match perfectly)
output_dir <- "/Users/robertfowler/Documents/CC Outcomes/Y2_NRCS_new_analysis/NRCS_new_plots"

# 2. Safely check and create the directory if it doesn't exist
if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
}


F24_amm_table <- F24_amm_summary %>%
  mutate(
    mean_se = sprintf("%.2f ± %.2f", mean, se),
    cv_pct  = sprintf("%.1f%%", cv)
  ) %>%
  select(Site, Depth_cm, Treatment_Full, n, mean_se, cv_pct) %>%
  kable(
    format = "html",
    caption = "Descriptive statistics for Fall 2024 soil Ammonium-N (kg/ha) by site, depth, and cover crop treatment.",
    col.names = c("Site", "Depth (cm)", "Treatment", "n", "Mean ± SE", "CV (%)"),
    align = "llclcc"
  ) %>%
  kable_styling(
    bootstrap_options = c("striped", "hover", "condensed"),
    full_width = FALSE,
    position = "left"
  ) %>%
  collapse_rows(columns = 1:2, valign = "top")

# 3. Export to the specific folder using file.path()
save_kable(
  F24_amm_table, 
  file = file.path(output_dir, "F24_amm_summary.png"), 
  zoom = 2)

#----SOIL NITRATE EXPLORATORY BOXPLOT----
# ==============================================================================
# Refined Soil Nitrate Boxplot (Linear Scale, High-Contrast Facets)
# ==============================================================================
ggplot(F24_3depth_soilN, aes(x = Treatment_Full, y = AmmoniumN_kgha, fill = Treatment_Full)) +
  # Boxplot layer with clean lines and no underlying raw data points
  geom_boxplot(outlier.shape = 16, outlier.size = 1.5, alpha = 0.85, color = "black") +
  
  # Facet grid: Depth vertically (metric), Site horizontally
  facet_grid(Depth_cm ~ Site, scales = "free_y") +
  
  # Custom treatment color mapping
  scale_fill_manual(values = color_map) +
  
  # Y-axis label using corrected R mathematical expression syntax
  labs(
    x = "Cover Crop Treatment",
    y = expression(Ammonium-N~(kg~ha^-1))
  ) +
  
  # Set serif font family globally
  theme_bw(base_family = "serif") +
  
  # Visual adjustments for facet banners and labels
  theme(
    legend.position = "none",
    
    # Rotate and align long treatment names on the horizontal axis
    axis.text.x = element_text(angle = 45, hjust = 1, size = 9, color = "black"),
    axis.text.y = element_text(size = 9, color = "black"),
    axis.title  = element_text(face = "bold", size = 11, color = "black"),
    
    # Facet strips: Light grey background with a crisp, solid black border
    strip.background = element_rect(fill = "grey90", color = "black", linewidth = 0.7),
    strip.text       = element_text(face = "bold", size = 10, color = "black"),
    
    # Clean up background grid lines
    panel.grid.major = element_line(color = "grey95"),
    panel.grid.minor = element_blank()
  )


#----MODEL----

F24_amm_MOD = lmer(AmmoniumN_kgha ~ Trt * Depth_cm * Site + (1|Site:Rep),
                          data = F24_3depth_soilN)

#----CHECKING MODEL----
# 1. Set up a 1-row, 2-column plotting grid
par(mfrow = c(1, 2), family = "serif")

# 2. Residuals vs. Fitted (Tests Homoskedasticity / Constant Variance)
plot(fitted(F24_amm_MOD), residuals(F24_amm_MOD),
     xlab = "Fitted Values", ylab = "Residuals",
     main = "Residuals vs. Fitted")
abline(h = 0, col = "red", lty = 2)

# 3. Normal Q-Q Plot (Tests Normality of Residuals)
qqnorm(residuals(F24_amm_MOD), main = "Normal Q-Q Plot")
qqline(residuals(F24_amm_MOD), col = "red", lty = 2)

# Reset plotting grid
par(mfrow = c(1, 1))

#----TRANSFORMING THE MODEL----
# 1. Fit the log-transformed model (using the cleaned data)
F24_amm_logMOD <- lmer(
  log(AmmoniumN_kgha) ~ Treatment_Full * Depth_cm * Site + (1|Site:Rep),
  data = F24_3depth_soilN)

# 2. Run diagnostics on the log-transformed model to verify correction
par(mfrow = c(1, 2), family = "serif")
plot(fitted(F24_amm_logMOD), residuals(F24_amm_logMOD),
     xlab = "Fitted Values", ylab = "Residuals", main = "Log-Model: Residuals vs. Fitted")
abline(h = 0, col = "red", lty = 2)

qqnorm(residuals(F24_amm_logMOD), main = "Log-Model: Q-Q Plot")
qqline(residuals(F24_amm_logMOD), col = "red", lty = 2)
par(mfrow = c(1, 1))


#----VARIANCE COMPONENTS----
# Extract and calculate the ICC
vc <- as.data.frame(VarCorr(F24_amm_logMOD))
sigma2_block <- vc$vcov[vc$grp == "Site:Rep"]
sigma2_resid <- vc$vcov[vc$grp == "Residual"]

icc <- sigma2_block / (sigma2_block + sigma2_resid)
cat("Blocking accounted for", round(icc * 100, 1), "% of the total random variation.\n")

performance::icc(F24_amm_logMOD)

#----MODEL SUMMARY AND ANOVA----
# 1. Generate the standard model parameter summary
summary(F24_amm_logMOD)

#----EXPONENTIATE FOR TRANSFORMED MODELS

exp(fixef(F24_amm_logMOD))


# 2. Run Type III ANOVA using Satterthwaite ddf
F24_amm_logANOVA <- anova(F24_amm_logMOD, type = 3, ddf = "Satterthwaite")
print(F24_amm_logANOVA)




#----FORMATTED ANOVA AND EXPORT----


# ==============================================================================
# 1. Clean, Round, and Format ANOVA Table (with Safe HTML Entities)
# ==============================================================================
F24_SoilAmm_anova <-F24_amm_logANOVA  %>%
  as.data.frame() %>%
  tibble::rownames_to_column("Source") %>%
  # Clean up the Source names
  mutate(
    Source = gsub("Depth_cm", "Depth", Source),
    Source = gsub("Treatment_Full|Trt", "Treatment", Source),
    Source = gsub(":", " × ", Source)
  ) %>%
  # Safely round the numerical columns
  mutate(
    `Sum Sq`  = round(`Sum Sq`, 2),
    `Mean Sq` = round(`Mean Sq`, 2),
    NumDF     = round(NumDF, 0),
    DenDF     = round(DenDF, 2),
    `F value` = round(`F value`, 2)
  ) %>%
  # Format p-values using &lt; instead of < to prevent HTML tag clipping
  mutate(
    `p-value` = case_when(
      is.na(`Pr(>F)`)   ~ "NS",
      `Pr(>F)` < 0.001  ~ "<b>&lt; 0.001</b>", # HTML-safe less-than
      `Pr(>F)` < 0.05   ~ sprintf("<b>%.4f</b>", `Pr(>F)`),
      TRUE              ~ sprintf("%.4f", `Pr(>F)`)
    )
  ) %>%
  # Keep only display-ready columns
  select(Source, `Sum Sq`, `Mean Sq`, NumDF, DenDF, `F value`, `p-value`)

# ==============================================================================
# 2. Export as High-Contrast HTML Table
# ==============================================================================
F24_SoilAmm_anova %>%
  kable(
    format = "html",
    caption = "Type III Analysis of Variance with Satterthwaite's method for soil ammonium-N 
    (log-transformed kg ha<sup>-1</sup>) in fall 2024. Bold p-values indicate statistical significance (alpha = 0.05). 
    Data was analyzed as a RCBD with the form: lmer(log(Ammonium) ~ Treatment * Depth * Site + (1|Site:Rep)).",
    col.names = c("Source of Variation", "Sum Sq", "Mean Sq", "Num Df", "Den Df", "F value", "p-value"),
    align = "lcccccc",
    escape = FALSE # Crucial: parses <b> and &lt; correctly
  ) %>%
  kable_styling(
    bootstrap_options = c("striped", "hover", "condensed"),
    full_width = FALSE,
    position = "left"
  ) %>%
  row_spec(0, bold = TRUE, color = "black")

#----EXTRACTING EMMEANS AND CLD, BACK-TRANSFORMING----

# ==============================================================================
# 1. Back-Transform using regrid() for Symmetric Standard Errors (response ± SE)
# ==============================================================================
# First, estimate marginal means on the log scale
F24_amm_emm_log <- emmeans(
  F24_amm_logMOD, 
  ~ Treatment_Full|Depth_cm * Site)

F24_amm_emm_log %>% 
  summary(infer = TRUE, type = "response")

F24_amm_emm_log %>% 
  summary(infer = TRUE)

F24_SoilAmm_contrasts <- pairs(F24_amm_emm_log, adjust = "tukey")
print(F24_SoilAmm_contrasts)

# Perform Tukey comparisons and get CLD letters on the log scale (where assumptions hold)
F24_SoilAmm_cld <- cld(
  F24_amm_emm_log, 
  alpha = 0.05, 
  Letters = letters, 
  adjust = "tukey", 
  decreasing = TRUE)



# Convert the grid to the response scale using regrid() for symmetric SEs
# Then merge the letters back based on all indexing factors
F24_SoilAmm_plot_data <- regrid(F24_amm_emm_log, transform = "response") %>%
  as.data.frame() %>%
  left_join(
    as.data.frame(F24_SoilAmm_cld) %>% 
      select(Treatment_Full, Depth_cm, Site, .group), 
    by = c("Treatment_Full", "Depth_cm", "Site")
  ) %>%
  mutate(Letter = trimws(.group))

print(F24_SoilAmm_plot_data)

#----CLUTTERED, FACET-WRAPPED PLOTS WITH CLD LETTERS CUT OFF----
ggplot(F24_SoilAmm_plot_data, aes(x = Treatment_Full, y = response, fill = Treatment_Full)) +
  geom_bar(stat = "identity", color = "black", alpha = 0.7) +
  # Symmetrical Delta-method SE bars
  geom_errorbar(aes(ymin = response - SE, ymax = response + SE), width = 0.2) +
  # Letter positioning safely above the SE bar
  geom_text(aes(label = Letter, y = response + SE), vjust = -0.5, size = 3) +
  # 3x3 wrapped grid (Site x Depth) with independent y-axes
  facet_wrap(Site ~ Depth_cm, scales = "free_y", ncol = 3) + 
  scale_fill_viridis_d(option = "mako") +
  labs(
    y = expression("Estimated Mean Soil Ammonium-N (kg ha"^{-1}*")"), # Adjust variable name as needed
    x = "Cover Crop Treatment",
    caption = paste0(
      "Bars represent back-transformed emmeans.\n",
      "Error bars represent Delta-method approximated ±1 SE.\n",
      "Letters indicate significant differences within each site and depth combination (Tukey HSD, alpha = 0.05)."
    )
  ) +
  theme_bw(base_family = "serif") +
  theme(
    legend.position = "none", 
    axis.text.x = element_text(angle = 45, hjust = 1, size = 9),
    strip.text = element_text(face = "bold", size = 10)
  )

#----WEIRD NEW PIPELINE FOR SITE-SPECIFIC GRAPHS----



# ==============================================================================
# 1. Simplified Data Wrangling (No complex indexing or middle-bar math)
# ==============================================================================
F24_SoilAmm_plot_data <- F24_SoilAmm_plot_data %>%
  group_by(Site, Depth_cm) %>%
  mutate(
    # Count unique letters in this site-depth cluster
    Is_Sig = n_distinct(Letter) > 1,
    
    # Layer 1: Individual letters (blanked out if the group is non-significant)
    Bar_Letter = if_else(Is_Sig, Letter, ""),
    
    # Layer 2: Single "n.s." label (placed only on the first row of a non-sig cluster)
    Cluster_NS = if_else(!Is_Sig & row_number() == 1, "n.s.", ""),
    
    # Find the maximum height in this cluster to float the "n.s." label safely
    Cluster_Max_Y = max(response + SE)
  ) %>%
  ungroup()

# ==============================================================================
# 2. Simplified Plotting Function (No complex positioning for n.s.)
# ==============================================================================
F24_plot_site_ammonium <- function(site_name) {
  F24_SoilAmm_plot_data %>%
    filter(Site == site_name) %>%
    
    ggplot(aes(x = Depth_cm, y = response, fill = Treatment_Full)) +
    # Clustered Bars and Error Bars
    geom_bar(stat = "identity", position = position_dodge(width = 0.8), color = "black", alpha = 0.7, width = 0.7) +
    geom_errorbar(
      aes(ymin = pmax(0, response - SE), ymax = response + SE),
      position = position_dodge(width = 0.8),
      width = 0.2
    ) +
    
    # TEXT LAYER 1: Individual Tukey letters (placed only if significant, dodged)
    geom_text(
      aes(label = Bar_Letter, y = response + SE),
      position = position_dodge(width = 0.8),
      vjust = -0.5,
      size = 3.2,
      fontface = "bold"
    ) +
    
    # TEXT LAYER 2: Single "n.s." label (undodged; naturally centers on the x-axis category)
    geom_text(
      aes(label = Cluster_NS, y = Cluster_Max_Y),
      vjust = -0.8,
      size = 3.5,
      fontface = "bold"
    ) +
    
    # Aesthetics & Theme
    scale_fill_manual(values = color_map) +
    scale_y_continuous(expand = expansion(mult = c(0.01, 0.22))) +
    labs(
      title = paste("Fall 2024 Soil Ammonium-N Profile:", site_name),
      y = expression("Estimated Mean Soil Ammonium-N (kg ha"^{-1}*")"),
      x = "Soil Depth Horizon",
      caption = paste0(
        "Bars represent back-transformed emmeans.\n",
        "Error bars represent Delta-method approximated ±1 SE.\n",
        "Letters indicate significant differences within each site and depth combination (Tukey HSD, alpha = 0.05)."
      ),
      fill = "Cover Crop Treatment"
    ) +
    theme_bw(base_family = "serif") +
    theme(
      plot.title = element_text(face = "bold", size = 12, hjust = 0.5),
      legend.position = "bottom",
      legend.title = element_text(face = "bold", size = 9),
      legend.text = element_text(size = 8),
      axis.text = element_text(color = "black", size = 9),
      panel.grid.major.x = element_blank()
    ) +
    guides(fill = guide_legend(ncol = 3))
}

# ==============================================================================
# 3. Regenerate the Standalone Objects
# ==============================================================================
F24_ammonium_plot_StPaul    <- F24_plot_site_ammonium("Saint Paul")
F24_ammonium_plot_Waseca    <- F24_plot_site_ammonium("Waseca")
F24_ammonium_plot_Lamberton <- F24_plot_site_ammonium("Lamberton")

# View a plot in RStudio
F24_ammonium_plot_StPaul  
F24_ammonium_plot_Waseca  
F24_ammonium_plot_Lamberton






#----F24 Total within-depth N by Site----

#----Process is streamlined, removed alot of the frills and exportable table code for speed----

view(F24_3depth_soilN)



#----EXPLORING THE DATA----
# ==============================================================================
# 1. Generate Descriptive Statistics Dataframe
# ==============================================================================
F24_depthN_summary <- F24_3depth_soilN %>%
  group_by(Site, Depth_cm, Treatment_Full) %>%
  summarize(
    n    = sum(!is.na(N_totindep_kgha)),
    mean = mean(N_totindep_kgha, na.rm = TRUE),
    sd   = sd(N_totindep_kgha, na.rm = TRUE),
    # Standard Error of the Mean: SE = SD / sqrt(n)
    se   = sd / sqrt(n),
    # Coefficient of Variation: CV% = (SD / Mean) * 100
    cv   = (sd / mean) * 100,
    .groups = "drop" )
print(F24_depthN_summary, n = 81)

#----SOIL total in-depth N EXPLORATORY BOXPLOT----
# ==============================================================================
# Refined Soil N Boxplot (Linear Scale, High-Contrast Facets)
# ==============================================================================
ggplot(F24_3depth_soilN, aes(x = Treatment_Full, y = N_totindep_kgha, fill = Treatment_Full)) +
  # Boxplot layer with clean lines and no underlying raw data points
  geom_boxplot(outlier.shape = 16, outlier.size = 1.5, alpha = 0.85, color = "black") +
  
  # Facet grid: Depth vertically (metric), Site horizontally
  facet_grid(Depth_cm ~ Site, scales = "free_y") +
  
  # Custom treatment color mapping
  scale_fill_manual(values = color_map) +
  
  # Y-axis label using corrected R mathematical expression syntax
  labs(
    x = "Cover Crop Treatment",
    y = expression(Total~Inorganic~N~(kg~ha^-1))
  ) +
  
  # Set serif font family globally
  theme_bw(base_family = "serif") +
  
  # Visual adjustments for facet banners and labels
  theme(
    legend.position = "none",
    
    # Rotate and align long treatment names on the horizontal axis
    axis.text.x = element_text(angle = 45, hjust = 1, size = 9, color = "black"),
    axis.text.y = element_text(size = 9, color = "black"),
    axis.title  = element_text(face = "bold", size = 11, color = "black"),
    
    # Facet strips: Light grey background with a crisp, solid black border
    strip.background = element_rect(fill = "grey90", color = "black", linewidth = 0.7),
    strip.text       = element_text(face = "bold", size = 10, color = "black"),
    
    # Clean up background grid lines
    panel.grid.major = element_line(color = "grey95"),
    panel.grid.minor = element_blank()
  )


#----MODEL----

F24_deptotN_MOD = lmer(N_totindep_kgha ~ Trt * Depth_cm * Site + (1|Site:Rep),
                   data = F24_3depth_soilN)

#----CHECKING MODEL----
# 1. Set up a 1-row, 2-column plotting grid
par(mfrow = c(1, 2), family = "serif")

# 2. Residuals vs. Fitted (Tests Homoskedasticity / Constant Variance)
plot(fitted(F24_deptotN_MOD), residuals(F24_deptotN_MOD),
     xlab = "Fitted Values", ylab = "Residuals",
     main = "Residuals vs. Fitted")
abline(h = 0, col = "red", lty = 2)

# 3. Normal Q-Q Plot (Tests Normality of Residuals)
qqnorm(residuals(F24_deptotN_MOD), main = "Normal Q-Q Plot")
qqline(residuals(F24_deptotN_MOD), col = "red", lty = 2)

# Reset plotting grid
par(mfrow = c(1, 1))

#----TRANSFORMING THE MODEL----
# 1. Fit the log-transformed model (using the cleaned data)
F24_deptotN_logMOD <- lmer(
  log(N_totindep_kgha) ~ Treatment_Full * Depth_cm * Site + (1|Site:Rep),
  data = F24_3depth_soilN)

# 2. Run diagnostics on the log-transformed model to verify correction
par(mfrow = c(1, 2), family = "serif")
plot(fitted(F24_deptotN_logMOD), residuals(F24_deptotN_logMOD),
     xlab = "Fitted Values", ylab = "Residuals", main = "Log-Model: Residuals vs. Fitted")
abline(h = 0, col = "red", lty = 2)

qqnorm(residuals(F24_deptotN_logMOD), main = "Log-Model: Q-Q Plot")
qqline(residuals(F24_deptotN_logMOD), col = "red", lty = 2)
par(mfrow = c(1, 1))


#----VARIANCE COMPONENTS----
# Extract and calculate the ICC
vc <- as.data.frame(VarCorr(F24_deptotN_logMOD))
sigma2_block <- vc$vcov[vc$grp == "Site:Rep"]
sigma2_resid <- vc$vcov[vc$grp == "Residual"]

icc <- sigma2_block / (sigma2_block + sigma2_resid)
cat("Blocking accounted for", round(icc * 100, 1), "% of the total random variation.\n")

performance::icc(F24_deptotN_logMOD)

#----MODEL SUMMARY AND ANOVA----
# 1. Generate the standard model parameter summary
summary(F24_deptotN_logMOD)

# 2. Run Type III ANOVA using Satterthwaite ddf
F24_deptotN_logANOVA <- anova(F24_deptotN_logMOD, type = 3, ddf = "Satterthwaite")
print(F24_deptotN_logANOVA)


#----EXTRACTING EMMEANS AND CLD, BACK-TRANSFORMING----

# ==============================================================================
# 1. Back-Transform using regrid() for Symmetric Standard Errors (response ± SE)
# ==============================================================================
# First, estimate marginal means on the log scale
F24_deptotN_emm_log <- emmeans(
  F24_deptotN_logMOD, 
  ~ Treatment_Full|Depth_cm * Site)

# Perform Tukey comparisons and get CLD letters on the log scale (where assumptions hold)
F24_deptotN_cld <- cld(
  F24_deptotN_emm_log, 
  alpha = 0.05, 
  Letters = letters, 
  adjust = "tukey", 
  decreasing = TRUE)



# Convert the grid to the response scale using regrid() for symmetric SEs
# Then merge the letters back based on all indexing factors
F24_deptotN_plot_data <- regrid(F24_deptotN_emm_log, transform = "response") %>%
  as.data.frame() %>%
  left_join(
    as.data.frame(F24_deptotN_cld) %>% 
      select(Treatment_Full, Depth_cm, Site, .group), 
    by = c("Treatment_Full", "Depth_cm", "Site")
  ) %>%
  mutate(Letter = trimws(.group))

print(F24_deptotN_plot_data)



#----WEIRD NEW PIPELINE FOR SITE-SPECIFIC GRAPHS----



# ==============================================================================
# 1. Simplified Data Wrangling (No complex indexing or middle-bar math)
# ==============================================================================
F24_deptotN_plot_data <- F24_deptotN_plot_data %>%
  group_by(Site, Depth_cm) %>%
  mutate(
    # Count unique letters in this site-depth cluster
    Is_Sig = n_distinct(Letter) > 1,
    
    # Layer 1: Individual letters (blanked out if the group is non-significant)
    Bar_Letter = if_else(Is_Sig, Letter, ""),
    
    # Layer 2: Single "n.s." label (placed only on the first row of a non-sig cluster)
    Cluster_NS = if_else(!Is_Sig & row_number() == 1, "n.s.", ""),
    
    # Find the maximum height in this cluster to float the "n.s." label safely
    Cluster_Max_Y = max(response + SE)
  ) %>%
  ungroup()

# ==============================================================================
# 2. Simplified Plotting Function (No complex positioning for n.s.)
# ==============================================================================
F24_plot_site_deptotN<- function(site_name) {
  F24_deptotN_plot_data %>%
    filter(Site == site_name) %>%
    
    ggplot(aes(x = Depth_cm, y = response, fill = Treatment_Full)) +
    # Clustered Bars and Error Bars
    geom_bar(stat = "identity", position = position_dodge(width = 0.8), color = "black", alpha = 0.7, width = 0.7) +
    geom_errorbar(
      aes(ymin = pmax(0, response - SE), ymax = response + SE),
      position = position_dodge(width = 0.8),
      width = 0.2
    ) +
    
    # TEXT LAYER 1: Individual Tukey letters (placed only if significant, dodged)
    geom_text(
      aes(label = Bar_Letter, y = response + SE),
      position = position_dodge(width = 0.8),
      vjust = -0.5,
      size = 3.2,
      fontface = "bold"
    ) +
    
    # TEXT LAYER 2: Single "n.s." label (undodged; naturally centers on the x-axis category)
    geom_text(
      aes(label = Cluster_NS, y = Cluster_Max_Y),
      vjust = -0.8,
      size = 3.5,
      fontface = "bold"
    ) +
    
    # Aesthetics & Theme
    scale_fill_manual(values = color_map) +
    scale_y_continuous(expand = expansion(mult = c(0.01, 0.22))) +
    labs(
      title = paste("Fall 2024 Soil Total Inorganic N Profile:", site_name),
      y = expression("Estimated Mean Soil Inorganic N (kg ha"^{-1}*")"),
      x = "Soil Depth Horizon",
      caption = paste0(
        "Bars represent back-transformed emmeans.\n",
        "Error bars represent Delta-method approximated ±1 SE.\n",
        "Letters indicate significant differences within each site and depth combination (Tukey HSD, alpha = 0.05)."
      ),
      fill = "Cover Crop Treatment"
    ) +
    theme_bw(base_family = "serif") +
    theme(
      plot.title = element_text(face = "bold", size = 12, hjust = 0.5),
      legend.position = "bottom",
      legend.title = element_text(face = "bold", size = 9),
      legend.text = element_text(size = 8),
      axis.text = element_text(color = "black", size = 9),
      panel.grid.major.x = element_blank()
    ) +
    guides(fill = guide_legend(ncol = 3))
}

# ==============================================================================
# 3. Regenerate the Standalone Objects
# ==============================================================================
F24_deptotN_plot_StPaul    <- F24_plot_site_deptotN("Saint Paul")
F24_deptotN_plot_Waseca    <- F24_plot_site_deptotN("Waseca")
F24_deptotN_plot_Lamberton <- F24_plot_site_deptotN("Lamberton")

# View a plot in RStudio
F24_deptotN_plot_StPaul  
F24_deptotN_plot_Waseca  
F24_deptotN_plot_Lamberton







#----S25 Work----

#----S25 3-depth soil Nitrate/Nitrite-N----



#----EXPLORING THE DATA----
# ==============================================================================
# 1. Generate Descriptive Statistics Dataframe
# ==============================================================================
S25_Nit_summary <- S25_3depth_soilN %>%
  group_by(Site, Depth_cm, Treatment_Full) %>%
  summarize(
    n    = sum(!is.na(Nitrate_NitriteN_kgha)),
    mean = mean(Nitrate_NitriteN_kgha, na.rm = TRUE),
    sd   = sd(Nitrate_NitriteN_kgha, na.rm = TRUE),
    # Standard Error of the Mean: SE = SD / sqrt(n)
    se   = sd / sqrt(n),
    # Coefficient of Variation: CV% = (SD / Mean) * 100
    cv   = (sd / mean) * 100,
    .groups = "drop" )
print(S25_Nit_summary , n = 81)

#----SOIL total in-depth N EXPLORATORY BOXPLOT----
# ==============================================================================
# Refined Soil N Boxplot (Linear Scale, High-Contrast Facets)
# ==============================================================================
ggplot(S25_3depth_soilN, aes(x = Treatment_Full, y = Nitrate_NitriteN_kgha, fill = Treatment_Full)) +
  # Boxplot layer with clean lines and no underlying raw data points
  geom_boxplot(outlier.shape = 16, outlier.size = 1.5, alpha = 0.85, color = "black") +
  
  # Facet grid: Depth vertically (metric), Site horizontally
  facet_grid(Depth_cm ~ Site, scales = "free_y") +
  
  # Custom treatment color mapping
  scale_fill_manual(values = color_map) +
  
  # Y-axis label using corrected R mathematical expression syntax
  labs(
    x = "Cover Crop Treatment",
    y = expression(Nitrate/Nitrite-N~(kg~ha^-1))
  ) +
  
  # Set serif font family globally
  theme_bw(base_family = "serif") +
  
  # Visual adjustments for facet banners and labels
  theme(
    legend.position = "none",
    
    # Rotate and align long treatment names on the horizontal axis
    axis.text.x = element_text(angle = 45, hjust = 1, size = 9, color = "black"),
    axis.text.y = element_text(size = 9, color = "black"),
    axis.title  = element_text(face = "bold", size = 11, color = "black"),
    
    # Facet strips: Light grey background with a crisp, solid black border
    strip.background = element_rect(fill = "grey90", color = "black", linewidth = 0.7),
    strip.text       = element_text(face = "bold", size = 10, color = "black"),
    
    # Clean up background grid lines
    panel.grid.major = element_line(color = "grey95"),
    panel.grid.minor = element_blank()
  )


#----MODEL----

S25_SoilNit_MOD = lmer(Nitrate_NitriteN_kgha ~ Trt * Depth_cm * Site + (1|Site:Rep),
                       data = S25_3depth_soilN)

#----CHECKING MODEL----
# 1. Set up a 1-row, 2-column plotting grid
par(mfrow = c(1, 2), family = "serif")

# 2. Residuals vs. Fitted (Tests Homoskedasticity / Constant Variance)
plot(fitted(S25_SoilNit_MOD), residuals(S25_SoilNit_MOD),
     xlab = "Fitted Values", ylab = "Residuals",
     main = "Residuals vs. Fitted")
abline(h = 0, col = "red", lty = 2)

# 3. Normal Q-Q Plot (Tests Normality of Residuals)
qqnorm(residuals(S25_SoilNit_MOD), main = "Normal Q-Q Plot")
qqline(residuals(S25_SoilNit_MOD), col = "red", lty = 2)

# Reset plotting grid
par(mfrow = c(1, 1))

#----TRANSFORMING THE MODEL----
# 1. Fit the log-transformed model (using the cleaned data)
S25_SoilNit_logMOD <- lmer(
  log(Nitrate_NitriteN_kgha) ~ Treatment_Full * Depth_cm * Site + (1|Site:Rep),
  data = S25_3depth_soilN)

# 2. Run diagnostics on the log-transformed model to verify correction
par(mfrow = c(1, 2), family = "serif")
plot(fitted(S25_SoilNit_logMOD), residuals(S25_SoilNit_logMOD),
     xlab = "Fitted Values", ylab = "Residuals", main = "Log-Model: Residuals vs. Fitted")
abline(h = 0, col = "red", lty = 2)

qqnorm(residuals(S25_SoilNit_logMOD), main = "Log-Model: Q-Q Plot")
qqline(residuals(S25_SoilNit_logMOD), col = "red", lty = 2)
par(mfrow = c(1, 1))


#----VARIANCE COMPONENTS----
# Extract and calculate the ICC
vc <- as.data.frame(VarCorr(S25_SoilNit_logMOD))
sigma2_block <- vc$vcov[vc$grp == "Site:Rep"]
sigma2_resid <- vc$vcov[vc$grp == "Residual"]

icc <- sigma2_block / (sigma2_block + sigma2_resid)
cat("Blocking accounted for", round(icc * 100, 1), "% of the total random variation.\n")

performance::icc(S25_SoilNit_logMOD)

#----MODEL SUMMARY AND ANOVA----
# 1. Generate the standard model parameter summary
summary(S25_SoilNit_logMOD)

# 2. Run Type III ANOVA using Satterthwaite ddf
S25_SoilNit_logANOVA <- anova(S25_SoilNit_logMOD, type = 3, ddf = "Satterthwaite")
print(S25_SoilNit_logANOVA)


#----EXTRACTING EMMEANS AND CLD, BACK-TRANSFORMING----

# ==============================================================================
# 1. Back-Transform using regrid() for Symmetric Standard Errors (response ± SE)
# ==============================================================================
# First, estimate marginal means on the log scale
S25_SoilNit_emm_log <- emmeans(
  S25_SoilNit_logMOD, 
  ~ Treatment_Full|Depth_cm * Site)

# Perform Tukey comparisons and get CLD letters on the log scale (where assumptions hold)
S25_SoilNit_cld <- cld(
  S25_SoilNit_emm_log, 
  alpha = 0.05, 
  Letters = letters, 
  adjust = "tukey", 
  decreasing = TRUE)



# Convert the grid to the response scale using regrid() for symmetric SEs
# Then merge the letters back based on all indexing factors
S25_SoilNit_plot_data <- regrid(S25_SoilNit_emm_log, transform = "response") %>%
  as.data.frame() %>%
  left_join(
    as.data.frame(S25_SoilNit_cld) %>% 
      select(Treatment_Full, Depth_cm, Site, .group), 
    by = c("Treatment_Full", "Depth_cm", "Site")
  ) %>%
  mutate(Letter = trimws(.group))

print(S25_SoilNit_plot_data)



#----WEIRD NEW PIPELINE FOR SITE-SPECIFIC GRAPHS----



# ==============================================================================
# 1. Simplified Data Wrangling (No complex indexing or middle-bar math)
# ==============================================================================
S25_SoilNit_plot_data <- S25_SoilNit_plot_data %>%
  group_by(Site, Depth_cm) %>%
  mutate(
    # Count unique letters in this site-depth cluster
    Is_Sig = n_distinct(Letter) > 1,
    
    # Layer 1: Individual letters (blanked out if the group is non-significant)
    Bar_Letter = if_else(Is_Sig, Letter, ""),
    
    # Layer 2: Single "n.s." label (placed only on the first row of a non-sig cluster)
    Cluster_NS = if_else(!Is_Sig & row_number() == 1, "n.s.", ""),
    
    # Find the maximum height in this cluster to float the "n.s." label safely
    Cluster_Max_Y = max(response + SE)
  ) %>%
  ungroup()




S25_plot_site_SoilNit<- function(site_name) {
  S25_SoilNit_plot_data %>%
    filter(Site == site_name) %>%
  
  ggplot(aes(x = Depth_cm, y = response, fill = Treatment_Full)) +
  # Clustered Bars and Error Bars
  geom_bar(stat = "identity", position = position_dodge(width = 0.8), color = "black", alpha = 0.7, width = 0.7) +
  geom_errorbar(
    aes(ymin = pmax(0, response - SE), ymax = response + SE),
    position = position_dodge(width = 0.8),
    width = 0.2
  ) +
  
  # TEXT LAYER 1: Individual Tukey letters (placed only if significant, dodged)
  geom_text(
    aes(label = Bar_Letter, y = response + SE),
    position = position_dodge(width = 0.8),
    vjust = -0.5,
    size = 3.2,
    fontface = "bold"
  ) +
  
  # TEXT LAYER 2: Single "n.s." label (undodged; naturally centers on the x-axis category)
  geom_text(
    aes(label = Cluster_NS, y = Cluster_Max_Y),
    vjust = -0.8,
    size = 3.5,
    fontface = "bold"
  ) +
  
  # Aesthetics & Theme
  scale_fill_manual(values = color_map) +
  scale_y_continuous(expand = expansion(mult = c(0.01, 0.22))) +
  labs(
    subtitle = paste(site_name),
    y = expression(bold("Estimated Mean Soil NO")[bold("3")]^bold("-")*bold
                   ("/NO")[bold("2")]^bold("-")~bold("(kg ha")^bold("-1")*bold(")")),
    x = NULL,
    fill = "Cover Crop Treatment"
  ) +
  theme_bw(base_family = "serif") +
  theme(
    axis.text.x = element_text(color = "black", size = 14, face = "bold"),
    axis.text.y = element_text(color = "black", size = 12),
    axis.title.y = element_text(size = 12, color = "black", face = "bold"),
    legend.position = "bottom",
    legend.box = "vertical",
    legend.spacing.y = unit(0.2, "cm"),
    legend.text = element_text(size = 9),
    legend.title = element_text(face = "bold", size = 10),
    plot.subtitle = element_text(face = "bold", size = 11)
  )
}

# ==============================================================================
# 3. Regenerate the Standalone Objects
# ==============================================================================
S25_SoilNit_plot_StPaul    <- S25_plot_site_SoilNit("Saint Paul")
S25_SoilNit_plot_Waseca    <- S25_plot_site_SoilNit("Waseca")
S25_SoilNit_plot_Lamberton <- S25_plot_site_SoilNit("Lamberton")

# View a plot in RStudio
S25_SoilNit_plot_StPaul  
S25_SoilNit_plot_Waseca  
S25_SoilNit_plot_Lamberton




# Suppress x-axis text/ticks on top two subplots (St. Paul & Waseca)
S25_SoilNit_plot_StPaul <- S25_SoilNit_plot_StPaul + 
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())

S25_SoilNit_plot_Waseca   <- S25_SoilNit_plot_Waseca   + 
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())

S25_SoilNit_plot_Lamberton <- S25_SoilNit_plot_Lamberton +
  labs(x = "Depth (cm)") +
  theme(
    axis.title.x = element_text(face = "bold", size = 12, margin = margin(t = 8, b = 5))
  )

# 2. Assemble stacked patchwork without using plot_annotation caption
S25_SoilNit_composite_figure <- (S25_SoilNit_plot_StPaul / S25_SoilNit_plot_Waseca / S25_SoilNit_plot_Lamberton) +
  plot_layout(guides = "collect") +
  plot_annotation(
    tag_levels = "a", 
    tag_prefix = "(", 
    tag_suffix = ")"
  ) &
  theme(
    legend.position = "bottom",
    legend.box.margin = margin(t = 5, r = 30, b = 5, l = 0),
    legend.justification = "center",
    plot.tag = element_text(face = "bold", size = 11, family = "serif"),
    plot.title = element_text(face = "bold", size = 13, family = "serif")
  )

# 3. Direct Disk Export
ggsave(
  filename = "S25_Composite_SoilNit.png",
  plot = S25_SoilNit_composite_figure,
  width = 8.5,
  height = 12,
  dpi = 300,
  device = "png"
)












#----S25 3-depth soil Ammonium-N----



#----EXPLORING THE DATA----
# ==============================================================================
# 1. Generate Descriptive Statistics Dataframe
# ==============================================================================
S25_Amm_summary <- S25_3depth_soilN %>%
  group_by(Site, Depth_cm, Treatment_Full) %>%
  summarize(
    n    = sum(!is.na(AmmoniumN_kgha)),
    mean = mean(AmmoniumN_kgha, na.rm = TRUE),
    sd   = sd(AmmoniumN_kgha, na.rm = TRUE),
    # Standard Error of the Mean: SE = SD / sqrt(n)
    se   = sd / sqrt(n),
    # Coefficient of Variation: CV% = (SD / Mean) * 100
    cv   = (sd / mean) * 100,
    .groups = "drop" )
print(S25_Amm_summary , n = 81)

#----SOIL total in-depth N EXPLORATORY BOXPLOT----
# ==============================================================================
# Refined Soil N Boxplot (Linear Scale, High-Contrast Facets)
# ==============================================================================
ggplot(S25_3depth_soilN, aes(x = Treatment_Full, y = AmmoniumN_kgha, fill = Treatment_Full)) +
  # Boxplot layer with clean lines and no underlying raw data points
  geom_boxplot(outlier.shape = 16, outlier.size = 1.5, alpha = 0.85, color = "black") +
  
  # Facet grid: Depth vertically (metric), Site horizontally
  facet_grid(Depth_cm ~ Site, scales = "free_y") +
  
  # Custom treatment color mapping
  scale_fill_manual(values = color_map) +
  
  # Y-axis label using corrected R mathematical expression syntax
  labs(
    x = "Cover Crop Treatment",
    y = expression(NH[4]^+{}~(kg~ha^-1))
  ) +
  
  # Set serif font family globally
  theme_bw(base_family = "serif") +
  
  # Visual adjustments for facet banners and labels
  theme(
    legend.position = "none",
    
    # Rotate and align long treatment names on the horizontal axis
    axis.text.x = element_text(angle = 45, hjust = 1, size = 9, color = "black"),
    axis.text.y = element_text(size = 9, color = "black"),
    axis.title  = element_text(face = "bold", size = 11, color = "black"),
    
    # Facet strips: Light grey background with a crisp, solid black border
    strip.background = element_rect(fill = "grey90", color = "black", linewidth = 0.7),
    strip.text       = element_text(face = "bold", size = 10, color = "black"),
    
    # Clean up background grid lines
    panel.grid.major = element_line(color = "grey95"),
    panel.grid.minor = element_blank()
  )


#----MODEL----

S25_SoilAmm_MOD = lmer(AmmoniumN_kgha ~ Trt * Depth_cm * Site + (1|Site:Rep),
                       data = S25_3depth_soilN)

#----CHECKING MODEL----
# 1. Set up a 1-row, 2-column plotting grid
par(mfrow = c(1, 2), family = "serif")

# 2. Residuals vs. Fitted (Tests Homoskedasticity / Constant Variance)
plot(fitted(S25_SoilAmm_MOD), residuals(S25_SoilAmm_MOD),
     xlab = "Fitted Values", ylab = "Residuals",
     main = "Residuals vs. Fitted")
abline(h = 0, col = "red", lty = 2)

# 3. Normal Q-Q Plot (Tests Normality of Residuals)
qqnorm(residuals(S25_SoilAmm_MOD), main = "Normal Q-Q Plot")
qqline(residuals(S25_SoilAmm_MOD), col = "red", lty = 2)

# Reset plotting grid
par(mfrow = c(1, 1))

#----TRANSFORMING THE MODEL----
# 1. Fit the log-transformed model (using the cleaned data)
S25_SoilAmm_logMOD <- lmer(
  log(AmmoniumN_kgha) ~ Treatment_Full * Depth_cm * Site + (1|Site:Rep),
  data = S25_3depth_soilN)

# 2. Run diagnostics on the log-transformed model to verify correction
par(mfrow = c(1, 2), family = "serif")
plot(fitted(S25_SoilAmm_logMOD), residuals(S25_SoilAmm_logMOD),
     xlab = "Fitted Values", ylab = "Residuals", main = "Log-Model: Residuals vs. Fitted")
abline(h = 0, col = "red", lty = 2)

qqnorm(residuals(S25_SoilAmm_logMOD), main = "Log-Model: Q-Q Plot")
qqline(residuals(S25_SoilAmm_logMOD), col = "red", lty = 2)
par(mfrow = c(1, 1))


#----MODEL SUMMARY AND ANOVA----
# 1. Generate the standard model parameter summary
summary(S25_SoilAmm_logMOD)

# 2. Run Type III ANOVA using Satterthwaite ddf
S25_SoilAmm_logANOVA <- anova(S25_SoilAmm_logMOD, type = 3, ddf = "Satterthwaite")
print(S25_SoilAmm_logANOVA)


#----EXTRACTING EMMEANS AND CLD, BACK-TRANSFORMING----

# ==============================================================================
# 1. Back-Transform using regrid() for Symmetric Standard Errors (response ± SE)
# ==============================================================================
# First, estimate marginal means on the log scale
S25_SoilAmm_emm_log <- emmeans(
  S25_SoilAmm_logMOD, 
  ~ Treatment_Full|Depth_cm * Site)

# Perform Tukey comparisons and get CLD letters on the log scale (where assumptions hold)
S25_SoilAmm_cld <- cld(
  S25_SoilAmm_emm_log, 
  alpha = 0.05, 
  Letters = letters, 
  adjust = "tukey", 
  decreasing = TRUE)



# Convert the grid to the response scale using regrid() for symmetric SEs
# Then merge the letters back based on all indexing factors
S25_SoilAmm_plot_data <- regrid(S25_SoilAmm_emm_log, transform = "response") %>%
  as.data.frame() %>%
  left_join(
    as.data.frame(S25_SoilAmm_cld) %>% 
      select(Treatment_Full, Depth_cm, Site, .group), 
    by = c("Treatment_Full", "Depth_cm", "Site")
  ) %>%
  mutate(Letter = trimws(.group))

print(S25_SoilAmm_plot_data)



#----WEIRD NEW PIPELINE FOR SITE-SPECIFIC GRAPHS----



# ==============================================================================
# 1. Simplified Data Wrangling (No complex indexing or middle-bar math)
# ==============================================================================
S25_SoilAmm_plot_data <- S25_SoilAmm_plot_data %>%
  group_by(Site, Depth_cm) %>%
  mutate(
    # Count unique letters in this site-depth cluster
    Is_Sig = n_distinct(Letter) > 1,
    
    # Layer 1: Individual letters (blanked out if the group is non-significant)
    Bar_Letter = if_else(Is_Sig, Letter, ""),
    
    # Layer 2: Single "n.s." label (placed only on the first row of a non-sig cluster)
    Cluster_NS = if_else(!Is_Sig & row_number() == 1, "n.s.", ""),
    
    # Find the maximum height in this cluster to float the "n.s." label safely
    Cluster_Max_Y = max(response + SE)
  ) %>%
  ungroup()

# ==============================================================================
# 2. Simplified Plotting Function (No complex positioning for n.s.)
# ==============================================================================
S25_plot_site_SoilAmm<- function(site_name) {
  S25_SoilAmm_plot_data %>%
    filter(Site == site_name) %>%
    
    ggplot(aes(x = Depth_cm, y = response, fill = Treatment_Full)) +
    # Clustered Bars and Error Bars
    geom_bar(stat = "identity", position = position_dodge(width = 0.8), color = "black", alpha = 0.7, width = 0.7) +
    geom_errorbar(
      aes(ymin = pmax(0, response - SE), ymax = response + SE),
      position = position_dodge(width = 0.8),
      width = 0.2
    ) +
    
    # TEXT LAYER 1: Individual Tukey letters (placed only if significant, dodged)
    geom_text(
      aes(label = Bar_Letter, y = response + SE),
      position = position_dodge(width = 0.8),
      vjust = -0.5,
      size = 3.2,
      fontface = "bold"
    ) +
    
    # TEXT LAYER 2: Single "n.s." label (undodged; naturally centers on the x-axis category)
    geom_text(
      aes(label = Cluster_NS, y = Cluster_Max_Y),
      vjust = -0.8,
      size = 3.5,
      fontface = "bold"
    ) +
    
    # Aesthetics & Theme
    scale_fill_manual(values = color_map) +
    scale_y_continuous(expand = expansion(mult = c(0.01, 0.22))) +
    labs(
      title = paste("Spring 2025 Soil Ammonium-N Profile:", site_name),
      y = expression(Estimated~Marginal~Mean~Soil~NH[4]^+{}~(kg~ha^-1)),
      x = "Soil Depth Horizon",
      caption = paste0(
        "Bars represent back-transformed emmeans.\n",
        "Error bars represent Delta-method approximated ±1 SE.\n",
        "Letters indicate significant differences within each site and depth combination (Tukey HSD, alpha = 0.05)."
      ),
      fill = "Cover Crop Treatment"
    ) +
    theme_bw(base_family = "serif") +
    theme(
      plot.title = element_text(face = "bold", size = 12, hjust = 0.5),
      legend.position = "bottom",
      legend.title = element_text(face = "bold", size = 9),
      legend.text = element_text(size = 8),
      axis.text = element_text(color = "black", size = 9),
      panel.grid.major.x = element_blank()
    ) +
    guides(fill = guide_legend(ncol = 3))
}

# ==============================================================================
# 3. Regenerate the Standalone Objects
# ==============================================================================
S25_SoilAmm_plot_StPaul    <- S25_plot_site_SoilAmm("Saint Paul")
S25_SoilAmm_plot_Waseca    <- S25_plot_site_SoilAmm("Waseca")
S25_SoilAmm_plot_Lamberton <- S25_plot_site_SoilAmm("Lamberton")

# View a plot in RStudio
S25_SoilAmm_plot_StPaul  
S25_SoilAmm_plot_Waseca  
S25_SoilAmm_plot_Lamberton









#----S25 3-depth total N----



#----EXPLORING THE DATA----
# ==============================================================================
# 1. Generate Descriptive Statistics Dataframe
# ==============================================================================
S25_depthN_summary <- S25_3depth_soilN %>%
  group_by(Site, Depth_cm, Treatment_Full) %>%
  summarize(
    n    = sum(!is.na(N_totindep_kgha)),
    mean = mean(N_totindep_kgha, na.rm = TRUE),
    sd   = sd(N_totindep_kgha, na.rm = TRUE),
    # Standard Error of the Mean: SE = SD / sqrt(n)
    se   = sd / sqrt(n),
    # Coefficient of Variation: CV% = (SD / Mean) * 100
    cv   = (sd / mean) * 100,
    .groups = "drop" )
print(S25_depthN_summary , n = 81)

#----SOIL total in-depth N EXPLORATORY BOXPLOT----
# ==============================================================================
# Refined Soil N Boxplot (Linear Scale, High-Contrast Facets)
# ==============================================================================
ggplot(S25_3depth_soilN, aes(x = Treatment_Full, y = N_totindep_kgha, fill = Treatment_Full)) +
  # Boxplot layer with clean lines and no underlying raw data points
  geom_boxplot(outlier.shape = 16, outlier.size = 1.5, alpha = 0.85, color = "black") +
  
  # Facet grid: Depth vertically (metric), Site horizontally
  facet_grid(Depth_cm ~ Site, scales = "free_y") +
  
  # Custom treatment color mapping
  scale_fill_manual(values = color_map) +
  
  # Y-axis label using corrected R mathematical expression syntax
  labs(
    x = "Cover Crop Treatment",
    y = expression(Total~Inorganic~N~(kg~ha^-1))
  ) +
  
  # Set serif font family globally
  theme_bw(base_family = "serif") +
  
  # Visual adjustments for facet banners and labels
  theme(
    legend.position = "none",
    
    # Rotate and align long treatment names on the horizontal axis
    axis.text.x = element_text(angle = 45, hjust = 1, size = 9, color = "black"),
    axis.text.y = element_text(size = 9, color = "black"),
    axis.title  = element_text(face = "bold", size = 11, color = "black"),
    
    # Facet strips: Light grey background with a crisp, solid black border
    strip.background = element_rect(fill = "grey90", color = "black", linewidth = 0.7),
    strip.text       = element_text(face = "bold", size = 10, color = "black"),
    
    # Clean up background grid lines
    panel.grid.major = element_line(color = "grey95"),
    panel.grid.minor = element_blank()
  )


#----MODEL----

S25_deptotN_MOD = lmer(N_totindep_kgha ~ Trt * Depth_cm * Site + (1|Site:Rep),
                       data = S25_3depth_soilN)

#----CHECKING MODEL----
# 1. Set up a 1-row, 2-column plotting grid
par(mfrow = c(1, 2), family = "serif")

# 2. Residuals vs. Fitted (Tests Homoskedasticity / Constant Variance)
plot(fitted(S25_deptotN_MOD), residuals(S25_deptotN_MOD),
     xlab = "Fitted Values", ylab = "Residuals",
     main = "Residuals vs. Fitted")
abline(h = 0, col = "red", lty = 2)

# 3. Normal Q-Q Plot (Tests Normality of Residuals)
qqnorm(residuals(S25_deptotN_MOD), main = "Normal Q-Q Plot")
qqline(residuals(S25_deptotN_MOD), col = "red", lty = 2)

# Reset plotting grid
par(mfrow = c(1, 1))

#----TRANSFORMING THE MODEL----
# 1. Fit the log-transformed model (using the cleaned data)
S25_deptotN_logMOD <- lmer(
  log(N_totindep_kgha) ~ Treatment_Full * Depth_cm * Site + (1|Site:Rep),
  data = S25_3depth_soilN)

# 2. Run diagnostics on the log-transformed model to verify correction
par(mfrow = c(1, 2), family = "serif")
plot(fitted(S25_deptotN_logMOD), residuals(S25_deptotN_logMOD),
     xlab = "Fitted Values", ylab = "Residuals", main = "Log-Model: Residuals vs. Fitted")
abline(h = 0, col = "red", lty = 2)

qqnorm(residuals(S25_deptotN_logMOD), main = "Log-Model: Q-Q Plot")
qqline(residuals(S25_deptotN_logMOD), col = "red", lty = 2)
par(mfrow = c(1, 1))


#----MODEL SUMMARY AND ANOVA----
# 1. Generate the standard model parameter summary
summary(S25_deptotN_logMOD)

# 2. Run Type III ANOVA using Satterthwaite ddf
S25_deptotN_logANOVA <- anova(S25_deptotN_logMOD, type = 3, ddf = "Satterthwaite")
print(S25_deptotN_logANOVA )


#----EXTRACTING EMMEANS AND CLD, BACK-TRANSFORMING----

# ==============================================================================
# 1. Back-Transform using regrid() for Symmetric Standard Errors (response ± SE)
# ==============================================================================
# First, estimate marginal means on the log scale
S25_deptotN_emm_log <- emmeans(
  S25_deptotN_logMOD, 
  ~ Treatment_Full|Depth_cm * Site)

# Perform Tukey comparisons and get CLD letters on the log scale (where assumptions hold)
S25_deptotN_cld <- cld(
  S25_deptotN_emm_log, 
  alpha = 0.05, 
  Letters = letters, 
  adjust = "tukey", 
  decreasing = TRUE)



# Convert the grid to the response scale using regrid() for symmetric SEs
# Then merge the letters back based on all indexing factors
S25_deptotN_plot_data <- regrid(S25_deptotN_emm_log , transform = "response") %>%
  as.data.frame() %>%
  left_join(
    as.data.frame(S25_deptotN_cld) %>% 
      select(Treatment_Full, Depth_cm, Site, .group), 
    by = c("Treatment_Full", "Depth_cm", "Site")
  ) %>%
  mutate(Letter = trimws(.group))

print(S25_deptotN_plot_data)



#----WEIRD NEW PIPELINE FOR SITE-SPECIFIC GRAPHS----



# ==============================================================================
# 1. Simplified Data Wrangling (No complex indexing or middle-bar math)
# ==============================================================================
S25_deptotN_plot_data <- S25_deptotN_plot_data %>%
  group_by(Site, Depth_cm) %>%
  mutate(
    # Count unique letters in this site-depth cluster
    Is_Sig = n_distinct(Letter) > 1,
    
    # Layer 1: Individual letters (blanked out if the group is non-significant)
    Bar_Letter = if_else(Is_Sig, Letter, ""),
    
    # Layer 2: Single "n.s." label (placed only on the first row of a non-sig cluster)
    Cluster_NS = if_else(!Is_Sig & row_number() == 1, "n.s.", ""),
    
    # Find the maximum height in this cluster to float the "n.s." label safely
    Cluster_Max_Y = max(response + SE)
  ) %>%
  ungroup()

# ==============================================================================
# 2. Simplified Plotting Function (No complex positioning for n.s.)
# ==============================================================================
S25_plot_site_deptotN<- function(site_name) {
  S25_deptotN_plot_data %>%
    filter(Site == site_name) %>%
    
    ggplot(aes(x = Depth_cm, y = response, fill = Treatment_Full)) +
    # Clustered Bars and Error Bars
    geom_bar(stat = "identity", position = position_dodge(width = 0.8), color = "black", alpha = 0.7, width = 0.7) +
    geom_errorbar(
      aes(ymin = pmax(0, response - SE), ymax = response + SE),
      position = position_dodge(width = 0.8),
      width = 0.2
    ) +
    
    # TEXT LAYER 1: Individual Tukey letters (placed only if significant, dodged)
    geom_text(
      aes(label = Bar_Letter, y = response + SE),
      position = position_dodge(width = 0.8),
      vjust = -0.5,
      size = 3.2,
      fontface = "bold"
    ) +
    
    # TEXT LAYER 2: Single "n.s." label (undodged; naturally centers on the x-axis category)
    geom_text(
      aes(label = Cluster_NS, y = Cluster_Max_Y),
      vjust = -0.8,
      size = 3.5,
      fontface = "bold"
    ) +
    
    # Aesthetics & Theme
    scale_fill_manual(values = color_map) +
    scale_y_continuous(expand = expansion(mult = c(0.01, 0.22))) +
    labs(
      subtitle = paste(site_name),
      y = expression(bold("Estimated Mean Soil Inorganic N (kg ha")^bold("-1")*bold(")")),
      x = NULL,
      fill = "Cover Crop Treatment"
    ) +
    theme_bw(base_family = "serif") +
    theme(
      axis.text.x = element_text(color = "black", size = 14, face = "bold"),
      axis.text.y = element_text(color = "black", size = 12),
      axis.title.y = element_text(size = 12, color = "black", face = "bold"),
      legend.position = "bottom",
      legend.box = "vertical",
      legend.spacing.y = unit(0.2, "cm"),
      legend.text = element_text(size = 9),
      legend.title = element_text(face = "bold", size = 10),
      plot.subtitle = element_text(face = "bold", size = 11)
    )
}

# ==============================================================================
# 3. Regenerate the Standalone Objects
# ==============================================================================
S25_deptotN_plot_StPaul    <- S25_plot_site_deptotN("Saint Paul")
S25_deptotN_plot_Waseca    <- S25_plot_site_deptotN("Waseca")
S25_deptotN_plot_Lamberton <- S25_plot_site_deptotN("Lamberton")

# View a plot in RStudio
S25_deptotN_plot_StPaul  
S25_deptotN_plot_Waseca  
S25_deptotN_plot_Lamberton




# Suppress x-axis text/ticks on top two subplots (St. Paul & Waseca)
S25_deptotN_plot_StPaul <- S25_deptotN_plot_StPaul + 
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())

S25_deptotN_plot_Waseca   <- S25_deptotN_plot_Waseca   + 
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())

# Stack plots into patchwork
S25_deptotN_composite_figure <- (S25_deptotN_plot_StPaul  / S25_deptotN_plot_Waseca / S25_deptotN_plot_Lamberton) +
  plot_layout(guides = "collect") +
  plot_annotation(
    tag_levels = "a", 
    tag_prefix = "(", 
    tag_suffix = ")"
  ) &
  theme(
    legend.position = "bottom",
    legend.box.margin = margin(t = 5, r = 30, b = 5, l = 0), # Adds right-side padding to prevent clipping
    legend.justification = "center",
    plot.tag = element_text(face = "bold", size = 11, family = "serif"),
    plot.title = element_text(face = "bold", size = 13, family = "serif"),
    plot.caption = element_text(face = "bold", size = 11, family = "serif", hjust = 0.5, margin = margin(t = 5, b = 5))
  )

# ==============================================================================
# 3. DIRECT DISK EXPORT
# ==============================================================================
ggsave(
  filename = "S25_Composite_totindepthN.png",
  plot = S25_deptotN_composite_figure,
  width = 8.5,
  height = 12,
  dpi = 300,
  device = "png"
)









#----F25 Total within-depth N by Site----

#----Process is streamlined, removed alot of the frills and exportable table code for speed----

#----EXPLORING THE DATA----
# ==============================================================================
# 1. Generate Descriptive Statistics Dataframe
# ==============================================================================
F25_depthN_summary <- F25_3depth_soilN %>%
  group_by(Site, Depth_cm, Treatment_Full) %>%
  summarize(
    n    = sum(!is.na(N_totindep_kgha)),
    mean = mean(N_totindep_kgha, na.rm = TRUE),
    sd   = sd(N_totindep_kgha, na.rm = TRUE),
    # Standard Error of the Mean: SE = SD / sqrt(n)
    se   = sd / sqrt(n),
    # Coefficient of Variation: CV% = (SD / Mean) * 100
    cv   = (sd / mean) * 100,
    .groups = "drop" )
print(F25_depthN_summary, n = 81)

#----SOIL total in-depth N EXPLORATORY BOXPLOT----
# ==============================================================================
# Refined Soil N Boxplot (Linear Scale, High-Contrast Facets)
# ==============================================================================
ggplot(F25_3depth_soilN, aes(x = Treatment_Full, y = N_totindep_kgha, fill = Treatment_Full)) +
  # Boxplot layer with clean lines and no underlying raw data points
  geom_boxplot(outlier.shape = 16, outlier.size = 1.5, alpha = 0.85, color = "black") +
  
  # Facet grid: Depth vertically (metric), Site horizontally
  facet_grid(Depth_cm ~ Site, scales = "free_y") +
  
  # Custom treatment color mapping
  scale_fill_manual(values = color_map) +
  
  # Y-axis label using corrected R mathematical expression syntax
  labs(
    x = "Cover Crop Treatment",
    y = expression(Total~Inorganic~N~(kg~ha^-1))
  ) +
  
  # Set serif font family globally
  theme_bw(base_family = "serif") +
  
  # Visual adjustments for facet banners and labels
  theme(
    legend.position = "none",
    
    # Rotate and align long treatment names on the horizontal axis
    axis.text.x = element_text(angle = 45, hjust = 1, size = 9, color = "black"),
    axis.text.y = element_text(size = 9, color = "black"),
    axis.title  = element_text(face = "bold", size = 11, color = "black"),
    
    # Facet strips: Light grey background with a crisp, solid black border
    strip.background = element_rect(fill = "grey90", color = "black", linewidth = 0.7),
    strip.text       = element_text(face = "bold", size = 10, color = "black"),
    
    # Clean up background grid lines
    panel.grid.major = element_line(color = "grey95"),
    panel.grid.minor = element_blank()
  )


#----MODEL----

F25_deptotN_MOD = lmer(N_totindep_kgha ~ Trt * Depth_cm * Site + (1|Site:Rep),
                       data = F25_3depth_soilN)

#----CHECKING MODEL----
# 1. Set up a 1-row, 2-column plotting grid
par(mfrow = c(1, 2), family = "serif")

# 2. Residuals vs. Fitted (Tests Homoskedasticity / Constant Variance)
plot(fitted(F25_deptotN_MOD), residuals(F25_deptotN_MOD),
     xlab = "Fitted Values", ylab = "Residuals",
     main = "Residuals vs. Fitted")
abline(h = 0, col = "red", lty = 2)

# 3. Normal Q-Q Plot (Tests Normality of Residuals)
qqnorm(residuals(F25_deptotN_MOD), main = "Normal Q-Q Plot")
qqline(residuals(F25_deptotN_MOD), col = "red", lty = 2)

# Reset plotting grid
par(mfrow = c(1, 1))

#----TRANSFORMING THE MODEL----
# 1. Fit the log-transformed model (using the cleaned data)
F25_deptotN_logMOD <- lmer(
  log(N_totindep_kgha) ~ Treatment_Full * Depth_cm * Site + (1|Site:Rep),
  data = F25_3depth_soilN)

# 2. Run diagnostics on the log-transformed model to verify correction
par(mfrow = c(1, 2), family = "serif")
plot(fitted(F25_deptotN_logMOD), residuals(F25_deptotN_logMOD),
     xlab = "Fitted Values", ylab = "Residuals", main = "Log-Model: Residuals vs. Fitted")
abline(h = 0, col = "red", lty = 2)

qqnorm(residuals(F25_deptotN_logMOD), main = "Log-Model: Q-Q Plot")
qqline(residuals(F25_deptotN_logMOD), col = "red", lty = 2)
par(mfrow = c(1, 1))


#----VARIANCE COMPONENTS----
# Extract and calculate the ICC
vc <- as.data.frame(VarCorr(F25_deptotN_logMOD))
sigma2_block <- vc$vcov[vc$grp == "Site:Rep"]
sigma2_resid <- vc$vcov[vc$grp == "Residual"]

icc <- sigma2_block / (sigma2_block + sigma2_resid)
cat("Blocking accounted for", round(icc * 100, 1), "% of the total random variation.\n")

performance::icc(F25_deptotN_logMOD)

#----MODEL SUMMARY AND ANOVA----
# 1. Generate the standard model parameter summary
summary(F25_deptotN_logMOD)

# 2. Run Type III ANOVA using Satterthwaite ddf
F25_deptotN_logANOVA <- anova(F25_deptotN_logMOD, type = 3, ddf = "Satterthwaite")
print(F25_deptotN_logANOVA)


#----EXTRACTING EMMEANS AND CLD, BACK-TRANSFORMING----

# ==============================================================================
# 1. Back-Transform using regrid() for Symmetric Standard Errors (response ± SE)
# ==============================================================================
# First, estimate marginal means on the log scale
F25_deptotN_emm_log <- emmeans(
  F25_deptotN_logMOD, 
  ~ Treatment_Full|Depth_cm * Site)

# Perform Tukey comparisons and get CLD letters on the log scale (where assumptions hold)
F25_deptotN_cld <- cld(
  F25_deptotN_emm_log, 
  alpha = 0.05, 
  Letters = letters, 
  adjust = "tukey", 
  decreasing = TRUE)

F25_deptotN_cld

# Convert the grid to the response scale using regrid() for symmetric SEs
# Then merge the letters back based on all indexing factors
F25_deptotN_plot_data <- regrid(F25_deptotN_emm_log, transform = "response") %>%
  as.data.frame() %>%
  left_join(
    as.data.frame(F25_deptotN_cld) %>% 
      select(Treatment_Full, Depth_cm, Site, .group), 
    by = c("Treatment_Full", "Depth_cm", "Site")
  ) %>%
  mutate(Letter = trimws(.group))

print(F25_deptotN_plot_data)



#----WEIRD NEW PIPELINE FOR SITE-SPECIFIC GRAPHS----



# ==============================================================================
# 1. Simplified Data Wrangling (No complex indexing or middle-bar math)
# ==============================================================================
F25_deptotN_plot_data <- F25_deptotN_plot_data %>%
  group_by(Site, Depth_cm) %>%
  mutate(
    # Count unique letters in this site-depth cluster
    Is_Sig = n_distinct(Letter) > 1,
    
    # Layer 1: Individual letters (blanked out if the group is non-significant)
    Bar_Letter = if_else(Is_Sig, Letter, ""),
    
    # Layer 2: Single "n.s." label (placed only on the first row of a non-sig cluster)
    Cluster_NS = if_else(!Is_Sig & row_number() == 1, "n.s.", ""),
    
    # Find the maximum height in this cluster to float the "n.s." label safely
    Cluster_Max_Y = max(response + SE)
  ) %>%
  ungroup()

# ==============================================================================
# 2. Simplified Plotting Function (No complex positioning for n.s.)
# ==============================================================================
F25_plot_site_deptotN<- function(site_name) {
  F25_deptotN_plot_data %>%
    filter(Site == site_name) %>%
    
    ggplot(aes(x = Depth_cm, y = response, fill = Treatment_Full)) +
    # Clustered Bars and Error Bars
    geom_bar(stat = "identity", position = position_dodge(width = 0.8), color = "black", alpha = 0.7, width = 0.7) +
    geom_errorbar(
      aes(ymin = pmax(0, response - SE), ymax = response + SE),
      position = position_dodge(width = 0.8),
      width = 0.2
    ) +
    
    # TEXT LAYER 1: Individual Tukey letters (placed only if significant, dodged)
    geom_text(
      aes(label = Bar_Letter, y = response + SE),
      position = position_dodge(width = 0.8),
      vjust = -0.5,
      size = 3.2,
      fontface = "bold"
    ) +
    
    # TEXT LAYER 2: Single "n.s." label (undodged; naturally centers on the x-axis category)
    geom_text(
      aes(label = Cluster_NS, y = Cluster_Max_Y),
      vjust = -0.8,
      size = 3.5,
      fontface = "bold"
    ) +
    
    # Aesthetics & Theme
    scale_fill_manual(values = color_map) +
    scale_y_continuous(expand = expansion(mult = c(0.01, 0.22))) +
    labs(
      y = expression("Estimated Mean Soil Inorganic N (kg ha"^{-1}*")"),
      x = "Soil Depth Horizon",
      fill = "Cover Crop Treatment"
    ) +
    theme_bw(base_family = "serif") +
    theme(
      plot.title = element_text(face = "bold", size = 12, hjust = 0.5),
      legend.position = "bottom",
      legend.title = element_text(face = "bold", size = 9),
      legend.text = element_text(size = 8),
      axis.text = element_text(color = "black", size = 9),
      panel.grid.major.x = element_blank()
    ) +
    guides(fill = guide_legend(ncol = 3))
}

# ==============================================================================
# 3. Regenerate the Standalone Objects
# ==============================================================================
F25_deptotN_plot_StPaul    <- F25_plot_site_deptotN("Saint Paul")
F25_deptotN_plot_Waseca    <- F25_plot_site_deptotN("Waseca")
F25_deptotN_plot_Lamberton <- F25_plot_site_deptotN("Lamberton")

# View a plot in RStudio
F25_deptotN_plot_StPaul  
F25_deptotN_plot_Waseca  
F25_deptotN_plot_Lamberton








#----F25 soil ammonium----





#----MODEL----

F25_amm_MOD = lm(AmmoniumN_kgha ~ Treatment_Full * Depth_cm * Site,
                   data = F25_3depth_soilN)

#----CHECKING MODEL----
# 1. Set up a 1-row, 2-column plotting grid
par(mfrow = c(1, 2), family = "serif")

# 2. Residuals vs. Fitted (Tests Homoskedasticity / Constant Variance)
plot(fitted(F25_amm_MOD), residuals(F25_amm_MOD),
     xlab = "Fitted Values", ylab = "Residuals",
     main = "Residuals vs. Fitted")
abline(h = 0, col = "red", lty = 2)

# 3. Normal Q-Q Plot (Tests Normality of Residuals)
qqnorm(residuals(F25_amm_MOD), main = "Normal Q-Q Plot")
qqline(residuals(F25_amm_MOD), col = "red", lty = 2)

# Reset plotting grid
par(mfrow = c(1, 1))

#----TRANSFORMING THE MODEL----
# 1. Fit the log-transformed model (using the cleaned data)
F25_amm_logMOD <- lmer(
  log(AmmoniumN_kgha) ~ Treatment_Full * Depth_cm * Site + (1|Site:Rep),
  data = F25_3depth_soilN)

# 2. Run diagnostics on the log-transformed model to verify correction
par(mfrow = c(1, 2), family = "serif")
plot(fitted(F25_amm_logMOD), residuals(F25_amm_logMOD),
     xlab = "Fitted Values", ylab = "Residuals", main = "Log-Model: Residuals vs. Fitted")
abline(h = 0, col = "red", lty = 2)

qqnorm(residuals(F25_amm_logMOD), main = "Log-Model: Q-Q Plot")
qqline(residuals(F25_amm_logMOD), col = "red", lty = 2)
par(mfrow = c(1, 1))


#----VARIANCE COMPONENTS----
# Extract and calculate the ICC
vc <- as.data.frame(VarCorr(F25_amm_logMOD))
sigma2_block <- vc$vcov[vc$grp == "Site:Rep"]
sigma2_resid <- vc$vcov[vc$grp == "Residual"]

icc <- sigma2_block / (sigma2_block + sigma2_resid)
cat("Blocking accounted for", round(icc * 100, 1), "% of the total random variation.\n")

performance::icc(F25_amm_logMOD)

#----MODEL SUMMARY AND ANOVA----
# 1. Generate the standard model parameter summary
summary(F25_amm_logMOD)

#----EXPONENTIATE FOR TRANSFORMED MODELS

exp(fixef(F25_amm_logMOD))


# 2. Run Type III ANOVA using Satterthwaite ddf
F25_amm_logANOVA <- anova(F25_amm_logMOD, type = 3, ddf = "Satterthwaite")
print(F25_amm_logANOVA)



#----EXTRACTING EMMEANS AND CLD, BACK-TRANSFORMING----

# ==============================================================================
# 1. Back-Transform using regrid() for Symmetric Standard Errors (response ± SE)
# ==============================================================================
# First, estimate marginal means on the log scale
F25_amm_emm_log <- emmeans(
  F25_amm_logMOD, 
  ~ Treatment_Full|Depth_cm * Site)

F25_amm_emm_log %>% 
  summary(infer = TRUE, type = "response")

F25_amm_emm_log %>% 
  summary(infer = TRUE)

F25_SoilAmm_contrasts <- pairs(F25_amm_emm_log, adjust = "tukey")
print(F24_SoilAmm_contrasts)

# Perform Tukey comparisons and get CLD letters on the log scale (where assumptions hold)
F25_SoilAmm_cld <- cld(
  F25_amm_emm_log, 
  alpha = 0.05, 
  Letters = letters, 
  adjust = "tukey", 
  decreasing = TRUE)



# Convert the grid to the response scale using regrid() for symmetric SEs
# Then merge the letters back based on all indexing factors
F25_SoilAmm_plot_data <- regrid(F25_amm_emm_log, transform = "response") %>%
  as.data.frame() %>%
  left_join(
    as.data.frame(F25_SoilAmm_cld) %>% 
      select(Treatment_Full, Depth_cm, Site, .group), 
    by = c("Treatment_Full", "Depth_cm", "Site")
  ) %>%
  mutate(Letter = trimws(.group))

print(F25_SoilAmm_plot_data)



#----WEIRD NEW PIPELINE FOR SITE-SPECIFIC GRAPHS----



# ==============================================================================
# 1. Simplified Data Wrangling (No complex indexing or middle-bar math)
# ==============================================================================
F25_SoilAmm_plot_data <- F25_SoilAmm_plot_data %>%
  group_by(Site, Depth_cm) %>%
  mutate(
    # Count unique letters in this site-depth cluster
    Is_Sig = n_distinct(Letter) > 1,
    
    # Layer 1: Individual letters (blanked out if the group is non-significant)
    Bar_Letter = if_else(Is_Sig, Letter, ""),
    
    # Layer 2: Single "n.s." label (placed only on the first row of a non-sig cluster)
    Cluster_NS = if_else(!Is_Sig & row_number() == 1, "n.s.", ""),
    
    # Find the maximum height in this cluster to float the "n.s." label safely
    Cluster_Max_Y = max(response + SE)
  ) %>%
  ungroup()

# ==============================================================================
# 2. Simplified Plotting Function (No complex positioning for n.s.)
# ==============================================================================
F25_plot_site_ammonium <- function(site_name) {
  F25_SoilAmm_plot_data %>%
    filter(Site == site_name) %>%
    
    ggplot(aes(x = Depth_cm, y = response, fill = Treatment_Full)) +
    # Clustered Bars and Error Bars
    geom_bar(stat = "identity", position = position_dodge(width = 0.8), color = "black", alpha = 0.7, width = 0.7) +
    geom_errorbar(
      aes(ymin = pmax(0, response - SE), ymax = response + SE),
      position = position_dodge(width = 0.8),
      width = 0.2
    ) +
    
    # TEXT LAYER 1: Individual Tukey letters (placed only if significant, dodged)
    geom_text(
      aes(label = Bar_Letter, y = response + SE),
      position = position_dodge(width = 0.8),
      vjust = -0.5,
      size = 3.2,
      fontface = "bold"
    ) +
    
    # TEXT LAYER 2: Single "n.s." label (undodged; naturally centers on the x-axis category)
    geom_text(
      aes(label = Cluster_NS, y = Cluster_Max_Y),
      vjust = -0.8,
      size = 3.5,
      fontface = "bold"
    ) +
    
    # Aesthetics & Theme
    scale_fill_manual(values = color_map) +
    scale_y_continuous(expand = expansion(mult = c(0.01, 0.22))) +
    labs(
      title = paste("Fall 2025 Soil Ammonium-N Profile:", site_name),
      y = expression("Estimated Mean Soil Ammonium-N (kg ha"^{-1}*")"),
      x = "Soil Depth Horizon",
      fill = "Cover Crop Treatment"
    ) +
    theme_bw(base_family = "serif") +
    theme(
      plot.title = element_text(face = "bold", size = 12, hjust = 0.5),
      legend.position = "bottom",
      legend.title = element_text(face = "bold", size = 9),
      legend.text = element_text(size = 8),
      axis.text = element_text(color = "black", size = 9),
      panel.grid.major.x = element_blank()
    ) +
    guides(fill = guide_legend(ncol = 3))
}

# ==============================================================================
# 3. Regenerate the Standalone Objects
# ==============================================================================
F25_ammonium_plot_StPaul    <- F25_plot_site_ammonium("Saint Paul")
F25_ammonium_plot_Waseca    <- F25_plot_site_ammonium("Waseca")
F25_ammonium_plot_Lamberton <- F25_plot_site_ammonium("Lamberton")

# View a plot in RStudio
F25_ammonium_plot_StPaul  
F25_ammonium_plot_Waseca  
F25_ammonium_plot_Lamberton







#----F25 3-depth soil Nitrate/Nitrite-N----



#----EXPLORING THE DATA----
# ==============================================================================
# 1. Generate Descriptive Statistics Dataframe
# ==============================================================================
F25_Nit_summary <- F25_3depth_soilN %>%
  group_by(Site, Depth_cm, Treatment_Full) %>%
  summarize(
    n    = sum(!is.na(Nitrate_NitriteN_kgha)),
    mean = mean(Nitrate_NitriteN_kgha, na.rm = TRUE),
    sd   = sd(Nitrate_NitriteN_kgha, na.rm = TRUE),
    # Standard Error of the Mean: SE = SD / sqrt(n)
    se   = sd / sqrt(n),
    # Coefficient of Variation: CV% = (SD / Mean) * 100
    cv   = (sd / mean) * 100,
    .groups = "drop" )
print(F25_Nit_summary , n = 81)

#----SOIL total in-depth N EXPLORATORY BOXPLOT----
# ==============================================================================
# Refined Soil N Boxplot (Linear Scale, High-Contrast Facets)
# ==============================================================================
ggplot(F25_3depth_soilN, aes(x = Treatment_Full, y = Nitrate_NitriteN_kgha, fill = Treatment_Full)) +
  # Boxplot layer with clean lines and no underlying raw data points
  geom_boxplot(outlier.shape = 16, outlier.size = 1.5, alpha = 0.85, color = "black") +
  
  # Facet grid: Depth vertically (metric), Site horizontally
  facet_grid(Depth_cm ~ Site, scales = "free_y") +
  
  # Custom treatment color mapping
  scale_fill_manual(values = color_map) +
  
  # Y-axis label using corrected R mathematical expression syntax
  labs(
    x = "Cover Crop Treatment",
    y = expression(Nitrate/Nitrite-N~(kg~ha^-1))
  ) +
  
  # Set serif font family globally
  theme_bw(base_family = "serif") +
  
  # Visual adjustments for facet banners and labels
  theme(
    legend.position = "none",
    
    # Rotate and align long treatment names on the horizontal axis
    axis.text.x = element_text(angle = 45, hjust = 1, size = 9, color = "black"),
    axis.text.y = element_text(size = 9, color = "black"),
    axis.title  = element_text(face = "bold", size = 11, color = "black"),
    
    # Facet strips: Light grey background with a crisp, solid black border
    strip.background = element_rect(fill = "grey90", color = "black", linewidth = 0.7),
    strip.text       = element_text(face = "bold", size = 10, color = "black"),
    
    # Clean up background grid lines
    panel.grid.major = element_line(color = "grey95"),
    panel.grid.minor = element_blank()
  )


#----MODEL----

F25_SoilNit_MOD = lmer(Nitrate_NitriteN_kgha ~ Trt * Depth_cm * Site + (1|Site:Rep),
                       data = F25_3depth_soilN)

#----CHECKING MODEL----
# 1. Set up a 1-row, 2-column plotting grid
par(mfrow = c(1, 2), family = "serif")

# 2. Residuals vs. Fitted (Tests Homoskedasticity / Constant Variance)
plot(fitted(F25_SoilNit_MOD), residuals(F25_SoilNit_MOD),
     xlab = "Fitted Values", ylab = "Residuals",
     main = "Residuals vs. Fitted")
abline(h = 0, col = "red", lty = 2)

# 3. Normal Q-Q Plot (Tests Normality of Residuals)
qqnorm(residuals(F25_SoilNit_MOD), main = "Normal Q-Q Plot")
qqline(residuals(F25_SoilNit_MOD), col = "red", lty = 2)

# Reset plotting grid
par(mfrow = c(1, 1))

#----TRANSFORMING THE MODEL----
# 1. Fit the log-transformed model (using the cleaned data)
F25_SoilNit_logMOD <- lmer(
  log(Nitrate_NitriteN_kgha) ~ Treatment_Full * Depth_cm * Site + (1|Site:Rep),
  data = F25_3depth_soilN)

# 2. Run diagnostics on the log-transformed model to verify correction
par(mfrow = c(1, 2), family = "serif")
plot(fitted(F25_SoilNit_logMOD), residuals(F25_SoilNit_logMOD),
     xlab = "Fitted Values", ylab = "Residuals", main = "Log-Model: Residuals vs. Fitted")
abline(h = 0, col = "red", lty = 2)

qqnorm(residuals(F25_SoilNit_logMOD), main = "Log-Model: Q-Q Plot")
qqline(residuals(F25_SoilNit_logMOD), col = "red", lty = 2)
par(mfrow = c(1, 1))


#----VARIANCE COMPONENTS----
# Extract and calculate the ICC
vc <- as.data.frame(VarCorr(F25_SoilNit_logMOD))
sigma2_block <- vc$vcov[vc$grp == "Site:Rep"]
sigma2_resid <- vc$vcov[vc$grp == "Residual"]

icc <- sigma2_block / (sigma2_block + sigma2_resid)
cat("Blocking accounted for", round(icc * 100, 1), "% of the total random variation.\n")

performance::icc(S25_SoilNit_logMOD)

#----MODEL SUMMARY AND ANOVA----
# 1. Generate the standard model parameter summary
summary(F25_SoilNit_logMOD)

# 2. Run Type III ANOVA using Satterthwaite ddf
F25_SoilNit_logANOVA <- anova(F25_SoilNit_logMOD, type = 3, ddf = "Satterthwaite")
print(F25_SoilNit_logANOVA)


#----EXTRACTING EMMEANS AND CLD, BACK-TRANSFORMING----

# ==============================================================================
# 1. Back-Transform using regrid() for Symmetric Standard Errors (response ± SE)
# ==============================================================================
# First, estimate marginal means on the log scale
F25_SoilNit_emm_log <- emmeans(
  F25_SoilNit_logMOD, 
  ~ Treatment_Full|Depth_cm * Site)

# Perform Tukey comparisons and get CLD letters on the log scale (where assumptions hold)
F25_SoilNit_cld <- cld(
  F25_SoilNit_emm_log, 
  alpha = 0.05, 
  Letters = letters, 
  adjust = "tukey", 
  decreasing = TRUE)



# Convert the grid to the response scale using regrid() for symmetric SEs
# Then merge the letters back based on all indexing factors
F25_SoilNit_plot_data <- regrid(F25_SoilNit_emm_log, transform = "response") %>%
  as.data.frame() %>%
  left_join(
    as.data.frame(F25_SoilNit_cld) %>% 
      select(Treatment_Full, Depth_cm, Site, .group), 
    by = c("Treatment_Full", "Depth_cm", "Site")
  ) %>%
  mutate(Letter = trimws(.group))


print(F25_SoilNit_plot_data, n = 81)



#----WEIRD NEW PIPELINE FOR SITE-SPECIFIC GRAPHS----



# ==============================================================================
# 1. Simplified Data Wrangling (No complex indexing or middle-bar math)
# ==============================================================================
F25_SoilNit_plot_data <- F25_SoilNit_plot_data %>%
  group_by(Site, Depth_cm) %>%
  mutate(
    # Count unique letters in this site-depth cluster
    Is_Sig = n_distinct(Letter) > 1,
    
    # Layer 1: Individual letters (blanked out if the group is non-significant)
    Bar_Letter = if_else(Is_Sig, Letter, ""),
    
    # Layer 2: Single "n.s." label (placed only on the first row of a non-sig cluster)
    Cluster_NS = if_else(!Is_Sig & row_number() == 1, "n.s.", ""),
    
    # Find the maximum height in this cluster to float the "n.s." label safely
    Cluster_Max_Y = max(response + SE)
  ) %>%
  ungroup()

# ==============================================================================
# 2. Simplified Plotting Function (No complex positioning for n.s.)
# ==============================================================================
F25_plot_site_SoilNit<- function(site_name) {
  F25_SoilNit_plot_data %>%
    filter(Site == site_name) %>%
    
    ggplot(aes(x = Depth_cm, y = response, fill = Treatment_Full)) +
    # Clustered Bars and Error Bars
    geom_bar(stat = "identity", position = position_dodge(width = 0.8), color = "black", alpha = 0.7, width = 0.7) +
    geom_errorbar(
      aes(ymin = pmax(0, response - SE), ymax = response + SE),
      position = position_dodge(width = 0.8),
      width = 0.2
    ) +
    
    # TEXT LAYER 1: Individual Tukey letters (placed only if significant, dodged)
    geom_text(
      aes(label = Bar_Letter, y = response + SE),
      position = position_dodge(width = 0.8),
      vjust = -0.5,
      size = 3.2,
      fontface = "bold"
    ) +
    
    # TEXT LAYER 2: Single "n.s." label (undodged; naturally centers on the x-axis category)
    geom_text(
      aes(label = Cluster_NS, y = Cluster_Max_Y),
      vjust = -0.8,
      size = 3.5,
      fontface = "bold"
    ) +
    
    # Aesthetics & Theme
    scale_fill_manual(values = color_map) +
    scale_y_continuous(expand = expansion(mult = c(0.01, 0.22))) +
    labs(
      y = expression(Estimated~Marginal~Mean~Soil~NO[3]^-{}/NO[2]^-{}~(kg~ha^-1)),
      x = "Soil Depth Horizon",
      fill = "Cover Crop Treatment"
    ) +
    theme_bw(base_family = "serif") +
    theme(
      plot.title = element_text(face = "bold", size = 12, hjust = 0.5),
      legend.position = "bottom",
      legend.title = element_text(face = "bold", size = 9),
      legend.text = element_text(size = 8),
      axis.text = element_text(color = "black", size = 9),
      panel.grid.major.x = element_blank()
    ) +
    guides(fill = guide_legend(ncol = 3))
}

# ==============================================================================
# 3. Regenerate the Standalone Objects
# ==============================================================================
F25_SoilNit_plot_StPaul    <- F25_plot_site_SoilNit("Saint Paul")
F25_SoilNit_plot_Waseca    <- F25_plot_site_SoilNit("Waseca")
F25_SoilNit_plot_Lamberton <- F25_plot_site_SoilNit("Lamberton")

# View a plot in RStudio
F25_SoilNit_plot_StPaul  
F25_SoilNit_plot_Waseca  
F25_SoilNit_plot_Lamberton







#----S26 3-depth total N----



#----EXPLORING THE DATA----
# ==============================================================================
# 1. Generate Descriptive Statistics Dataframe
# ==============================================================================
S26_depthN_summary <- S26_3depth_soilN %>%
  group_by(Site, Depth_cm, Treatment_Full) %>%
  summarize(
    n    = sum(!is.na(N_totindep_kgha)),
    mean = mean(N_totindep_kgha, na.rm = TRUE),
    sd   = sd(N_totindep_kgha, na.rm = TRUE),
    # Standard Error of the Mean: SE = SD / sqrt(n)
    se   = sd / sqrt(n),
    # Coefficient of Variation: CV% = (SD / Mean) * 100
    cv   = (sd / mean) * 100,
    .groups = "drop" )
print(S25_depthN_summary , n = 81)

#----SOIL total in-depth N EXPLORATORY BOXPLOT----
# ==============================================================================
# Refined Soil N Boxplot (Linear Scale, High-Contrast Facets)
# ==============================================================================
ggplot(S26_3depth_soilN, aes(x = Treatment_Full, y = N_totindep_kgha, fill = Treatment_Full)) +
  # Boxplot layer with clean lines and no underlying raw data points
  geom_boxplot(outlier.shape = 16, outlier.size = 1.5, alpha = 0.85, color = "black") +
  
  # Facet grid: Depth vertically (metric), Site horizontally
  facet_grid(Depth_cm ~ Site, scales = "free_y") +
  
  # Custom treatment color mapping
  scale_fill_manual(values = color_map) +
  
  # Y-axis label using corrected R mathematical expression syntax
  labs(
    x = "Cover Crop Treatment",
    y = expression(Total~Inorganic~N~(kg~ha^-1))
  ) +
  
  # Set serif font family globally
  theme_bw(base_family = "serif") +
  
  # Visual adjustments for facet banners and labels
  theme(
    legend.position = "none",
    
    # Rotate and align long treatment names on the horizontal axis
    axis.text.x = element_text(angle = 45, hjust = 1, size = 9, color = "black"),
    axis.text.y = element_text(size = 9, color = "black"),
    axis.title  = element_text(face = "bold", size = 11, color = "black"),
    
    # Facet strips: Light grey background with a crisp, solid black border
    strip.background = element_rect(fill = "grey90", color = "black", linewidth = 0.7),
    strip.text       = element_text(face = "bold", size = 10, color = "black"),
    
    # Clean up background grid lines
    panel.grid.major = element_line(color = "grey95"),
    panel.grid.minor = element_blank()
  )


#----MODEL----

S26_deptotN_MOD = lmer(N_totindep_kgha ~ Trt * Depth_cm * Site + (1|Site:Rep),
                       data = S26_3depth_soilN)

#----CHECKING MODEL----
# 1. Set up a 1-row, 2-column plotting grid
par(mfrow = c(1, 2), family = "serif")

# 2. Residuals vs. Fitted (Tests Homoskedasticity / Constant Variance)
plot(fitted(S26_deptotN_MOD), residuals(S26_deptotN_MOD),
     xlab = "Fitted Values", ylab = "Residuals",
     main = "Residuals vs. Fitted")
abline(h = 0, col = "red", lty = 2)

# 3. Normal Q-Q Plot (Tests Normality of Residuals)
qqnorm(residuals(S26_deptotN_MOD), main = "Normal Q-Q Plot")
qqline(residuals(S26_deptotN_MOD), col = "red", lty = 2)

# Reset plotting grid
par(mfrow = c(1, 1))

#----TRANSFORMING THE MODEL----
# 1. Fit the log-transformed model (using the cleaned data)
S26_deptotN_logMOD <- lmer(
  log(N_totindep_kgha) ~ Treatment_Full * Depth_cm * Site + (1|Site:Rep),
  data = S26_3depth_soilN)

# 2. Run diagnostics on the log-transformed model to verify correction
par(mfrow = c(1, 2), family = "serif")
plot(fitted(S26_deptotN_logMOD), residuals(S26_deptotN_logMOD),
     xlab = "Fitted Values", ylab = "Residuals", main = "Log-Model: Residuals vs. Fitted")
abline(h = 0, col = "red", lty = 2)

qqnorm(residuals(S26_deptotN_logMOD), main = "Log-Model: Q-Q Plot")
qqline(residuals(S26_deptotN_logMOD), col = "red", lty = 2)
par(mfrow = c(1, 1))


#----MODEL SUMMARY AND ANOVA----
# 1. Generate the standard model parameter summary
summary(S26_deptotN_logMOD)

# 2. Run Type III ANOVA using Satterthwaite ddf
S26_deptotN_logANOVA <- anova(S26_deptotN_logMOD, type = 3, ddf = "Satterthwaite")
print(S26_deptotN_logANOVA )


#----EXTRACTING EMMEANS AND CLD, BACK-TRANSFORMING----

# ==============================================================================
# 1. Back-Transform using regrid() for Symmetric Standard Errors (response ± SE)
# ==============================================================================
# First, estimate marginal means on the log scale
S26_deptotN_emm_log <- emmeans(
  S26_deptotN_logMOD, 
  ~ Treatment_Full|Depth_cm * Site)

# Perform Tukey comparisons and get CLD letters on the log scale (where assumptions hold)
S26_deptotN_cld <- cld(
  S26_deptotN_emm_log, 
  alpha = 0.05, 
  Letters = letters, 
  adjust = "tukey", 
  decreasing = TRUE)



# Convert the grid to the response scale using regrid() for symmetric SEs
# Then merge the letters back based on all indexing factors
S26_deptotN_plot_data <- regrid(S26_deptotN_emm_log , transform = "response") %>%
  as.data.frame() %>%
  left_join(
    as.data.frame(S26_deptotN_cld) %>% 
      select(Treatment_Full, Depth_cm, Site, .group), 
    by = c("Treatment_Full", "Depth_cm", "Site")
  ) %>%
  mutate(Letter = trimws(.group))

print(S26_deptotN_plot_data)



#----WEIRD NEW PIPELINE FOR SITE-SPECIFIC GRAPHS----



# ==============================================================================
# 1. Simplified Data Wrangling (No complex indexing or middle-bar math)
# ==============================================================================
S26_deptotN_plot_data <- S26_deptotN_plot_data %>%
  group_by(Site, Depth_cm) %>%
  mutate(
    # Count unique letters in this site-depth cluster
    Is_Sig = n_distinct(Letter) > 1,
    
    # Layer 1: Individual letters (blanked out if the group is non-significant)
    Bar_Letter = if_else(Is_Sig, Letter, ""),
    
    # Layer 2: Single "n.s." label (placed only on the first row of a non-sig cluster)
    Cluster_NS = if_else(!Is_Sig & row_number() == 1, "n.s.", ""),
    
    # Find the maximum height in this cluster to float the "n.s." label safely
    Cluster_Max_Y = max(response + SE)
  ) %>%
  ungroup()

# ==============================================================================
# 2. Simplified Plotting Function (No complex positioning for n.s.)
# ==============================================================================
S26_plot_site_deptotN<- function(site_name) {
  S26_deptotN_plot_data %>%
    filter(Site == site_name) %>%
    
    ggplot(aes(x = Depth_cm, y = response, fill = Treatment_Full)) +
    # Clustered Bars and Error Bars
    geom_bar(stat = "identity", position = position_dodge(width = 0.8), color = "black", alpha = 0.7, width = 0.7) +
    geom_errorbar(
      aes(ymin = pmax(0, response - SE), ymax = response + SE),
      position = position_dodge(width = 0.8),
      width = 0.2
    ) +
    
    # TEXT LAYER 1: Individual Tukey letters (placed only if significant, dodged)
    geom_text(
      aes(label = Bar_Letter, y = response + SE),
      position = position_dodge(width = 0.8),
      vjust = -0.5,
      size = 3.2,
      fontface = "bold"
    ) +
    
    # TEXT LAYER 2: Single "n.s." label (undodged; naturally centers on the x-axis category)
    geom_text(
      aes(label = Cluster_NS, y = Cluster_Max_Y),
      vjust = -0.8,
      size = 3.5,
      fontface = "bold"
    ) +
    
    # Aesthetics & Theme
    scale_fill_manual(values = color_map) +
    scale_y_continuous(expand = expansion(mult = c(0.01, 0.22))) +
    labs(
      y = expression(Estimated~Marginal~Mean~Total~Inorganic~N~(kg~ha^-1)),
      x = "Soil Depth Horizon",
      fill = "Cover Crop Treatment"
    ) +
    theme_bw(base_family = "serif") +
    theme(
      plot.title = element_text(face = "bold", size = 12, hjust = 0.5),
      legend.position = "bottom",
      legend.title = element_text(face = "bold", size = 9),
      legend.text = element_text(size = 8),
      axis.text = element_text(color = "black", size = 9),
      panel.grid.major.x = element_blank()
    ) +
    guides(fill = guide_legend(ncol = 3))
}

# ==============================================================================
# 3. Regenerate the Standalone Objects
# ==============================================================================
S26_deptotN_plot_StPaul    <- S26_plot_site_deptotN("Saint Paul")
S26_deptotN_plot_Waseca    <- S26_plot_site_deptotN("Waseca")
S26_deptotN_plot_Lamberton <- S26_plot_site_deptotN("Lamberton")

# View a plot in RStudio
S26_deptotN_plot_StPaul  
S26_deptotN_plot_Waseca  
S26_deptotN_plot_Lamberton



#----S26 3-depth soil Nitrate/Nitrite-N----



#----EXPLORING THE DATA----
# ==============================================================================
# 1. Generate Descriptive Statistics Dataframe
# ==============================================================================
S26_Nit_summary <- S26_3depth_soilN %>%
  group_by(Site, Depth_cm, Treatment_Full) %>%
  summarize(
    n    = sum(!is.na(Nitrate_NitriteN_kgha)),
    mean = mean(Nitrate_NitriteN_kgha, na.rm = TRUE),
    sd   = sd(Nitrate_NitriteN_kgha, na.rm = TRUE),
    # Standard Error of the Mean: SE = SD / sqrt(n)
    se   = sd / sqrt(n),
    # Coefficient of Variation: CV% = (SD / Mean) * 100
    cv   = (sd / mean) * 100,
    .groups = "drop" )
print(S26_Nit_summary , n = 81)

#----SOIL total in-depth N EXPLORATORY BOXPLOT----
# ==============================================================================
# Refined Soil N Boxplot (Linear Scale, High-Contrast Facets)
# ==============================================================================
ggplot(S26_3depth_soilN, aes(x = Treatment_Full, y = Nitrate_NitriteN_kgha, fill = Treatment_Full)) +
  # Boxplot layer with clean lines and no underlying raw data points
  geom_boxplot(outlier.shape = 16, outlier.size = 1.5, alpha = 0.85, color = "black") +
  
  # Facet grid: Depth vertically (metric), Site horizontally
  facet_grid(Depth_cm ~ Site, scales = "free_y") +
  
  # Custom treatment color mapping
  scale_fill_manual(values = color_map) +
  
  # Y-axis label using corrected R mathematical expression syntax
  labs(
    x = "Cover Crop Treatment",
    y = expression(Nitrate/Nitrite-N~(kg~ha^-1))
  ) +
  
  # Set serif font family globally
  theme_bw(base_family = "serif") +
  
  # Visual adjustments for facet banners and labels
  theme(
    legend.position = "none",
    
    # Rotate and align long treatment names on the horizontal axis
    axis.text.x = element_text(angle = 45, hjust = 1, size = 9, color = "black"),
    axis.text.y = element_text(size = 9, color = "black"),
    axis.title  = element_text(face = "bold", size = 11, color = "black"),
    
    # Facet strips: Light grey background with a crisp, solid black border
    strip.background = element_rect(fill = "grey90", color = "black", linewidth = 0.7),
    strip.text       = element_text(face = "bold", size = 10, color = "black"),
    
    # Clean up background grid lines
    panel.grid.major = element_line(color = "grey95"),
    panel.grid.minor = element_blank()
  )


#----MODEL----

S26_SoilNit_MOD = lmer(Nitrate_NitriteN_kgha ~ Trt * Depth_cm * Site + (1|Site:Rep),
                       data = S26_3depth_soilN)

#----CHECKING MODEL----
# 1. Set up a 1-row, 2-column plotting grid
par(mfrow = c(1, 2), family = "serif")

# 2. Residuals vs. Fitted (Tests Homoskedasticity / Constant Variance)
plot(fitted(S26_SoilNit_MOD), residuals(S26_SoilNit_MOD),
     xlab = "Fitted Values", ylab = "Residuals",
     main = "Residuals vs. Fitted")
abline(h = 0, col = "red", lty = 2)

# 3. Normal Q-Q Plot (Tests Normality of Residuals)
qqnorm(residuals(S26_SoilNit_MOD), main = "Normal Q-Q Plot")
qqline(residuals(S26_SoilNit_MOD), col = "red", lty = 2)

# Reset plotting grid
par(mfrow = c(1, 1))

#----TRANSFORMING THE MODEL----
# 1. Fit the log-transformed model (using the cleaned data)
S26_SoilNit_logMOD <- lmer(
  log(Nitrate_NitriteN_kgha + 0.1) ~ Treatment_Full * Depth_cm * Site + (1|Site:Rep),
  data = S26_3depth_soilN)

# 2. Run diagnostics on the log-transformed model to verify correction
par(mfrow = c(1, 2), family = "serif")
plot(fitted(S26_SoilNit_logMOD), residuals(S26_SoilNit_logMOD),
     xlab = "Fitted Values", ylab = "Residuals", main = "Log-Model: Residuals vs. Fitted")
abline(h = 0, col = "red", lty = 2)

qqnorm(residuals(S26_SoilNit_logMOD), main = "Log-Model: Q-Q Plot")
qqline(residuals(S26_SoilNit_logMOD), col = "red", lty = 2)
par(mfrow = c(1, 1))


#----VARIANCE COMPONENTS----
# Extract and calculate the ICC
vc <- as.data.frame(VarCorr(S26_SoilNit_logMOD))
sigma2_block <- vc$vcov[vc$grp == "Site:Rep"]
sigma2_resid <- vc$vcov[vc$grp == "Residual"]

icc <- sigma2_block / (sigma2_block + sigma2_resid)
cat("Blocking accounted for", round(icc * 100, 1), "% of the total random variation.\n")

performance::icc(S26_SoilNit_logMOD)

#----MODEL SUMMARY AND ANOVA----
# 1. Generate the standard model parameter summary
summary(S26_SoilNit_logMOD)

# 2. Run Type III ANOVA using Satterthwaite ddf
S26_SoilNit_logANOVA <- anova(S26_SoilNit_logMOD, type = 3, ddf = "Satterthwaite")
print(S26_SoilNit_logANOVA)


#----EXTRACTING EMMEANS AND CLD, BACK-TRANSFORMING----

# ==============================================================================
# 1. Back-Transform using regrid() for Symmetric Standard Errors (response ± SE)
# ==============================================================================
# First, estimate marginal means on the log scale
S26_SoilNit_emm_log <- emmeans(
  S26_SoilNit_logMOD, 
  ~ Treatment_Full|Depth_cm * Site)

# Perform Tukey comparisons and get CLD letters on the log scale (where assumptions hold)
S26_SoilNit_cld <- cld(
  S26_SoilNit_emm_log, 
  alpha = 0.05, 
  Letters = letters, 
  adjust = "tukey", 
  decreasing = TRUE)



# Convert the grid to the response scale using regrid() for symmetric SEs
# Then merge the letters back based on all indexing factors
S26_SoilNit_plot_data <- regrid(S26_SoilNit_emm_log, transform = "response") %>%
  as.data.frame() %>%
  left_join(
    as.data.frame(S26_SoilNit_cld) %>% 
      select(Treatment_Full, Depth_cm, Site, .group), 
    by = c("Treatment_Full", "Depth_cm", "Site")
  ) %>%
  mutate(Letter = trimws(.group))

print(S26_SoilNit_plot_data)



#----WEIRD NEW PIPELINE FOR SITE-SPECIFIC GRAPHS----



# ==============================================================================
# 1. Simplified Data Wrangling (No complex indexing or middle-bar math)
# ==============================================================================
S26_SoilNit_plot_data <- S26_SoilNit_plot_data %>%
  group_by(Site, Depth_cm) %>%
  mutate(
    # Count unique letters in this site-depth cluster
    Is_Sig = n_distinct(Letter) > 1,
    
    # Layer 1: Individual letters (blanked out if the group is non-significant)
    Bar_Letter = if_else(Is_Sig, Letter, ""),
    
    # Layer 2: Single "n.s." label (placed only on the first row of a non-sig cluster)
    Cluster_NS = if_else(!Is_Sig & row_number() == 1, "n.s.", ""),
    
    # Find the maximum height in this cluster to float the "n.s." label safely
    Cluster_Max_Y = max(response + SE)
  ) %>%
  ungroup()

# ==============================================================================
# 2. Simplified Plotting Function (No complex positioning for n.s.)
# ==============================================================================
S26_plot_site_SoilNit<- function(site_name) {
  S26_SoilNit_plot_data %>%
    filter(Site == site_name) %>%
    
    ggplot(aes(x = Depth_cm, y = response, fill = Treatment_Full)) +
    # Clustered Bars and Error Bars
    geom_bar(stat = "identity", position = position_dodge(width = 0.8), color = "black", alpha = 0.7, width = 0.7) +
    geom_errorbar(
      aes(ymin = pmax(0, response - SE), ymax = response + SE),
      position = position_dodge(width = 0.8),
      width = 0.2
    ) +
    
    # TEXT LAYER 1: Individual Tukey letters (placed only if significant, dodged)
    geom_text(
      aes(label = Bar_Letter, y = response + SE),
      position = position_dodge(width = 0.8),
      vjust = -0.5,
      size = 3.2,
      fontface = "bold"
    ) +
    
    # TEXT LAYER 2: Single "n.s." label (undodged; naturally centers on the x-axis category)
    geom_text(
      aes(label = Cluster_NS, y = Cluster_Max_Y),
      vjust = -0.8,
      size = 3.5,
      fontface = "bold"
    ) +
    
    # Aesthetics & Theme
    scale_fill_manual(values = color_map) +
    scale_y_continuous(expand = expansion(mult = c(0.01, 0.22))) +
    labs(
      subtitle = paste(site_name),
      y = expression(bold("Estimated Mean Soil NO")[bold("3")]^bold("-")*bold
                     ("/NO")[bold("2")]^bold("-")~bold("(kg ha")^bold("-1")*bold(")")),
      x = NULL,
      fill = "Cover Crop Treatment"
    ) +
    theme_bw(base_family = "serif") +
    theme(
      axis.text.x = element_text(color = "black", size = 14, face = "bold"),
      axis.text.y = element_text(color = "black", size = 12),
      axis.title.y = element_text(size = 12, color = "black", face = "bold"),
      legend.position = "bottom",
      legend.box = "vertical",
      legend.spacing.y = unit(0.2, "cm"),
      legend.text = element_text(size = 9),
      legend.title = element_text(face = "bold", size = 10),
      plot.subtitle = element_text(face = "bold", size = 11)
    )
}

# ==============================================================================
# 3. Regenerate the Standalone Objects
# ==============================================================================
S26_SoilNit_plot_StPaul    <- S26_plot_site_SoilNit("Saint Paul")
S26_SoilNit_plot_Waseca    <- S26_plot_site_SoilNit("Waseca")
S26_SoilNit_plot_Lamberton <- S26_plot_site_SoilNit("Lamberton")

# View a plot in RStudio
S26_SoilNit_plot_StPaul  
S26_SoilNit_plot_Waseca  
S26_SoilNit_plot_Lamberton




# Suppress x-axis text/ticks on top two subplots (St. Paul & Waseca)
S26_SoilNit_plot_StPaul <- S26_SoilNit_plot_StPaul + 
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())

S26_SoilNit_plot_Waseca   <- S26_SoilNit_plot_Waseca   + 
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())

S26_SoilNit_plot_Lamberton <- S26_SoilNit_plot_Lamberton +
  labs(x = "Depth (cm)") +
  theme(
    axis.title.x = element_text(face = "bold", size = 12, margin = margin(t = 8, b = 5))
  )

# 2. Assemble stacked patchwork without using plot_annotation caption
S26_SoilNit_composite_figure <- (S26_SoilNit_plot_StPaul / S26_SoilNit_plot_Waseca / S26_SoilNit_plot_Lamberton) +
  plot_layout(guides = "collect") +
  plot_annotation(
    tag_levels = "a", 
    tag_prefix = "(", 
    tag_suffix = ")"
  ) &
  theme(
    legend.position = "bottom",
    legend.box.margin = margin(t = 5, r = 30, b = 5, l = 0),
    legend.justification = "center",
    plot.tag = element_text(face = "bold", size = 11, family = "serif"),
    plot.title = element_text(face = "bold", size = 13, family = "serif")
  )

# 3. Direct Disk Export
ggsave(
  filename = "S26_Composite_SoilNit.png",
  plot = S26_SoilNit_composite_figure,
  width = 8.5,
  height = 12,
  dpi = 300,
  device = "png"
)







