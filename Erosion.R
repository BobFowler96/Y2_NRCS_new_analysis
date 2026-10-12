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

#----F24 Erosion----

F24_Erosion = read.csv("F24_Erosion.csv")
S25_Erosion = read.csv("S25_Erosion.csv")
F25_Erosion = read.csv("F25_Erosion.csv")
S26_Erosion = read.csv("S26_Erosion.csv")

#----SETTING FACTOR LEVELS----

F24_Erosion = F24_Erosion %>% 
  mutate(
    Start_date = as.Date(Start_date, format = "%Y-%m-%d"),
    End_date = as.Date(End_date, format = "%Y-%m-%d"),
    Start_date_disp = format(Start_date, "%b-%Y"),
    End_date_disp = format(End_date, "%b-%Y"),
    Site = factor(Site, levels = c("Saint Paul", "Waseca", "Lamberton")),
    Texture = factor(Texture, levels = c("Waukegan silt loam", "Nicollet clay loam")),
    Rep = factor(Rep),
    Season = factor(Season),
    Season_Full = case_when(
      Season == "F24" ~ "Fall 2024"),
    Deployment = factor(Deployment),
    Trt = factor(Trt, levels = c("C", "R1", "R2", "R3", "R4", "O1", "O2", "O3", "O4")),
    Trt_type = factor(Trt_type),
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




S25_Erosion = S25_Erosion %>% 
  mutate(
    Start_date = as.Date(Start_date, format = "%Y-%m-%d"),
    End_date = as.Date(End_date, format = "%Y-%m-%d"),
    Start_date_disp = format(Start_date, "%b-%Y"),
    End_date_disp = format(End_date, "%b-%Y"),
    Site = factor(Site, levels = c("Saint Paul", "Waseca", "Lamberton")),
    Texture = factor(Texture, levels = c("Waukegan silt loam", "Nicollet clay loam", "Lam-Y1 clay loam")),
    Rep = factor(Rep),
    Season = factor(Season),
    Season_Full = case_when(
      Season == "S25" ~ "Spring 2025"),
    Deployment = factor(Deployment, levels = c("D1", "D2")),
    Trt = factor(Trt, levels = c("C", "R1", "R2", "R3", "R4", "O1", "O2", "O3", "O4")),
    Trt_type = factor(Trt_type),
    Treatment_Full = case_when(
      Trt == "C"  ~ "Fallow",
      Trt == "R1" ~ "Rye 10",
      Trt == "R2" ~ "Rye 20",
      Trt == "R3" ~ "Rye 40",
      Trt == "R4" ~ "Rye 80",
      Trt == "O1" ~ "Oat/Radish 10/1",
      Trt == "O2" ~ "Oat/Radish 20/2",
      Trt == "O3" ~ "Oat/Radish 40/4",
      Trt == "O4" ~ "Oat/Radish 60/6"
    ),
    Treatment_Full = factor(Treatment_Full, levels = c(
      "Fallow", 
      "Rye 10", "Rye 20", "Rye 40", "Rye 80", 
      "Oat/Radish 10/1", "Oat/Radish 20/2", "Oat/Radish 40/4", "Oat/Radish 60/6"
    )),
    Trt_type = factor(Trt_type, levels = c("No CC", "Cereal Rye", "Oat + Radish")))




F25_Erosion = F25_Erosion %>% 
  mutate(
    Start_date = as.Date(Start_date, format = "%Y-%m-%d"),
    End_date = as.Date(End_date, format = "%Y-%m-%d"),
    Start_date_disp = format(Start_date, "%b-%Y"),
    End_date_disp = format(End_date, "%b-%Y"),
    Site = factor(Site, levels = c("Saint Paul", "Waseca", "Lamberton")),
    Texture = factor(Texture, levels = c("Waukegan silt loam", "Webster clay loam", "Lam-Y2 clay loam")),
    Rep = factor(Rep),
    Season = factor(Season),
    Season_Full = case_when(
      Season == "F25" ~ "Fall 2025"
    ),
    Deployment = factor(Deployment),
    Trt = factor(Trt, levels = c("C", "R1", "R2", "R3", "R4", "O1", "O2", "O3", "O4")),
    Trt_type = factor(Trt_type),
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


S26_Erosion = S26_Erosion %>% 
  mutate(
    Start_date = as.Date(Start_date, format = "%Y-%m-%d"),
    End_date = as.Date(End_date, format = "%Y-%m-%d"),
    Start_date_disp = format(Start_date, "%b-%Y"),
    End_date_disp = format(End_date, "%b-%Y"),
    Site = factor(Site, levels = c("Saint Paul", "Waseca", "Lamberton")),
    Texture = factor(Texture, levels = c("Waukegan silt loam", "Webster clay loam", "Lam-Y2 clay loam")),
    Rep = factor(Rep),
    Season = factor(Season),
    Season_Full = case_when(
      Season == "S26" ~ "Spring 2026"
    ),
    Deployment = factor(Deployment),
    Trt = factor(Trt, levels = c("C", "R1", "R2", "R3", "R4", "O1", "O2", "O3", "O4")),
    Trt_type = factor(Trt_type),
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

#----Merging F24 Erosion and AGBM Data----


F24_ERXAGBM_dat <- F24_Erosion %>%
  left_join(
    F24_AGBM_CN, 
    by = c("Site", "Season","Season_Full", "Plot", "Trt", "Treatment_Full", "Trt_type", "Rep", "Project")
  ) %>% 
  mutate(
    across(c(AGBM_kgha, AGBM_lbac, AGBM_mgha), ~ replace_na(.x, 0)))

str(F24_ERXAGBM_dat)


S25_ERXAGBM_dat <- S25_Erosion %>%
  left_join(
    S25_AGBM_CN, 
    by = c("Site", "Season","Season_Full","Plot", "Trt", "Treatment_Full", "Trt_type", "Rep", "Project")
  ) %>% 
  mutate(
    across(c(AGBM_kgha, AGBM_lbac, AGBM_mgha), ~ replace_na(.x, 0)),
    # Recode Deployment to clear extension labels
    Deployment_Label = case_when(
      Deployment %in% c("D1", "Pre-Termination") ~ "Pre-Termination",
      Deployment %in% c("D2", "Post-Termination") ~ "Post-Termination",
      TRUE ~ as.character(Deployment)
    ),
    # Lock factor order for logical presentation
    Deployment_Label = factor(Deployment_Label, levels = c("Pre-Termination", "Post-Termination"))
  )




# Vertically stack the Fall 2024 and Spring 2025 datasets
F24XS25_ERXAGBM_dat <- bind_rows(F24_ERXAGBM_dat, S25_ERXAGBM_dat)

view (F24XS25_ERXAGBM_dat)
#----COMBINE F24 CC BIOMASS AND S25 EROSION----
S25EROXF24AGBM_dat <- S25_Erosion %>%
  left_join(
    F24_AGBM_CN, 
    by = c("Site", "Plot", "Trt", "Treatment_Full", "Trt_type", "Rep", "Project")
  ) %>% 
  mutate(
    across(c(AGBM_kgha, AGBM_lbac, AGBM_mgha), ~ replace_na(.x, 0)))

View(S25EROXF24AGBM_dat)

#----NEW EROSION DATAFRAMES WITH PRE AND POST TERM FACTOR, GETTING RID OF VOLUNTEER RYE IN LAM----
S25_ERXAGBM_dat <- S25_Erosion %>%
  left_join(
    S25_AGBM_CN, 
    by = c("Site", "Season", "Plot", "Trt", "Treatment_Full", "Trt_type", "Rep", "Project")
  ) %>% 
  # Exclude 'No CC' and 'Oat + Radish' ONLY at Lamberton
  filter(!(Site == "Lamberton" & Trt_type %in% c("No CC", "Oat + Radish"))) %>%
  mutate(
    across(c(AGBM_kgha, AGBM_lbac, AGBM_mgha), ~ replace_na(.x, 0)),
    # Recode Deployment to clear extension labels
    Deployment_Label = case_when(
      Deployment %in% c("D1", "Pre-Termination") ~ "Pre-Termination",
      Deployment %in% c("D2", "Post-Termination") ~ "Post-Termination",
      TRUE ~ as.character(Deployment)
    ),
    # Lock factor order for logical presentation
    Deployment_Label = factor(Deployment_Label, levels = c("Pre-Termination", "Post-Termination"))
  ) %>%
  droplevels()




#----F24 THRU S25 EROSION, TOOK OUT LAMBERTON VOLUNTEER RYE VALUES----

F24XS25_ERXAGBM_D1only <- F24XS25_ERXAGBM_dat %>%
  # Filter out D2 deployment in Spring 2025 (keeping all Fall 2024 and Spring 2025 D1)
  filter(!(Season == "S25" & Deployment == "D2")) %>%
  
  # Exclude 'No CC' and 'Oat + Radish' ONLY at Lamberton in Spring 2025
  filter(!(Site == "Lamberton" & Season == "S25" & Trt_type %in% c("No CC", "Oat + Radish"))) %>%
  
  # Recode Season for clear display
  mutate(
    Season_Disp = case_when(
      Season %in% c("F24", "Fall 2024") ~ "Fall 2024",
      Season %in% c("S25", "Spring 2025") ~ "Spring 2025",
      TRUE ~ as.character(Season)
    ),
    Season_Disp = factor(Season_Disp, levels = c("Fall 2024", "Spring 2025"))
  ) %>%
  droplevels()



# 1. Fit Mixed-Effects Model
# Note: Log-transform applied to handle right-skewness; added 1 if zeros exist.
SWCS_ER_PrePosT_mod <- lmer(log(ER_gm2day) ~ Treatment_Full * Deployment_Label + (1|Site:Rep),
                            data = S25_ERXAGBM_dat)

# Model summary & Type III ANOVA
summary(SWCS_ER_PrePosT_mod)
anova(SWCS_ER_PrePosT_mod, type = 3)

S25_ER_prepost_logANOVA <- anova(SWCS_ER_PrePosT_mod, type = 3, ddf = "Satterthwaite")
print(S25_ER_prepost_logANOVA)

# 2. Estimated Marginal Means (emmeans) partitioned by Deployment_Label
SWCS_ER_PrePosT_emm <- emmeans(SWCS_ER_PrePosT_mod, ~ Treatment_Full | Deployment_Label)

# 2. Get Tukey-adjusted CLD letters evaluated on the log scale
SWCS_ER_PrePosT_cld <- cld(
  SWCS_ER_PrePosT_emm, 
  alpha = 0.05, 
  Letters = letters, 
  adjust = "tukey", 
  decreasing = TRUE
)

SWCS_ER_PrePosT_cld

# 3. Back-transform emmeans to response scale (lb/ac) via Delta Method and merge CLD letters
SWCS_ER_PrePosT_plot_data <- regrid(SWCS_ER_PrePosT_emm, transform = "response") %>%
  as.data.frame() %>%
  left_join(
    as.data.frame(SWCS_ER_PrePosT_cld) %>% select(Treatment_Full, Deployment_Label, .group),
    by = c("Treatment_Full", "Deployment_Label")
  ) %>%
  mutate(Letter = trimws(.group))

# 4. Plot back-transformed emmeans with Delta-method ±1 SE error bars and CLD letters
ggplot(SWCS_ER_PrePosT_plot_data, aes(x = Treatment_Full, y = response, fill = Treatment_Full)) +
  geom_bar(stat = "identity", color = "black", alpha = 0.8) +
  geom_errorbar(
    aes(ymin = pmax(0, response - SE), ymax = response + SE), 
    width = 0.2, 
    linewidth = 0.6
  ) +
  geom_text(
    aes(label = Letter, y = response + SE), 
    vjust = -0.4, 
    size = 3.8, 
    family = "serif"
  ) +
  facet_wrap(~ Deployment_Label, scales = "fixed") +
  scale_fill_manual(values = color_map) +
  labs(
    title = "Spring 2025 Water-Borne Sediment Erosion by Treatment",
    x = "Cover Crop Treatment",
    y = expression(Water-Borne~Sediment~Erosion~(g~m^-2~day^-1)),
    caption = paste0(
      "Bars represent back-transformed estimated marginal means (emmeans).\n",
      "Error bars represent Delta-method approximated ±1 SE.\n",
      "Letters indicate significant differences within deployment timing (Tukey HSD, alpha = 0.05)."
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









# 2. Render Faceted Boxplot Visualization
ggplot(F24XS25_ERXAGBM_D1only, aes(x = Treatment_Full, y = ER_gm2day, fill = Treatment_Full)) +
  geom_boxplot(outlier.shape = 16, outlier.size = 2, alpha = 0.7) +
  geom_jitter(width = 0.15, alpha = 0.4, size = 1) +
  
  # Facet wrapped by Season Display with unified y-axis scale
  facet_wrap(~ Season_Disp, scales = "fixed") +
  
  # Unified Log10 Y-axis with comma labels
  scale_y_log10(labels = scales::comma_format()) +
  scale_fill_manual(values = color_map) +
  
  labs(
    title = "Water-Borne Sediment Erosion by Treatment",
    x = "Cover Crop Treatment",
    y = expression(Water-Borne~Sediment~Erosion~(g~m^-2~day^-1))
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



# 1. Fit Mixed-Effects Model on Log Scale
SWCS_ER_FallXSpring_mod <- lmer(
  log(ER_gm2day) ~ Treatment_Full * Season_Disp  + (1 | Site:Rep),
  data = F24XS25_ERXAGBM_D1only)

# Model Summary & Type III ANOVA
summary(SWCS_ER_FallXSpring_mod)
anova(SWCS_ER_FallXSpring_mod, type = 3)

# 2. Estimated Marginal Means (Log Scale) Partitioned by Season
SWCS_ER_FallXSpring_emm <- emmeans(SWCS_ER_FallXSpring_mod, ~ Treatment_Full | Season_Disp)

# 3. Compact Letter Display (Evaluated on Log Scale)
SWCS_ER_FallXSpring_cld <- cld(
  SWCS_ER_FallXSpring_emm, 
  alpha = 0.05, 
  Letters = letters, 
  adjust = "tukey", 
  decreasing = TRUE
)

# 4. Back-Transform Means & SE via Delta Method and Join CLD Letters
SWCS_ER_FallXSpring_plot_data <- regrid(SWCS_ER_FallXSpring_emm, transform = "response") %>%
  as.data.frame() %>%
  left_join(
    as.data.frame(SWCS_ER_FallXSpring_cld) %>% select(Treatment_Full, Season_Disp, .group),
    by = c("Treatment_Full", "Season_Disp")
  ) %>%
  mutate(Letter = trimws(.group))

# 5. Faceted Emmeans Plot (Response Scale)
ggplot(SWCS_ER_FallXSpring_plot_data, aes(x = Treatment_Full, y = response, fill = Treatment_Full)) +
  geom_bar(stat = "identity", color = "black", alpha = 0.8) +
  geom_errorbar(
    aes(ymin = pmax(0, response - SE), ymax = response + SE), 
    width = 0.2, 
    linewidth = 0.6
  ) +
  geom_text(
    aes(label = Letter, y = response + SE), 
    vjust = -0.4, 
    size = 3.8, 
    family = "serif"
  ) +
  facet_wrap(~ Season_Disp , scales = "fixed") +
  scale_fill_manual(values = color_map) +
  labs(
    title = "Water-Borne Sediment Erosion Across Seasons by Treatment",
    x = "Cover Crop Treatment",
    y = expression(Water-Borne~Sediment~Erosion~(g~m^-2~day^-1))
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



#----THAT ONE PLOT EVERYONE LIKES WITH THE g/m^2/day EROSION----
F24XS25_ERXAGBM_dat  %>%
  filter(!(Site == "Lamberton" & Trt_type %in% c("No CC", "Oat + Radish") &
             Season == "S25"))%>%
  ggplot(aes(x = AGBM_kgha, y = ER_gm2day)) +
  # Points mapped to Treatment Type Color, Season Shape, and Precipitation Size
  geom_point(aes(color = Treatment_Full, shape = Season, size = Precip_cumul_mm), alpha = 0.8) +
  
  # Single overall linear trendline across all observations
  geom_smooth(method = "lm", color = "black", se = TRUE, linewidth = 0.8) + 
  # Log10 scales with comma-formatted labels
  scale_y_log10(labels = scales::comma_format()) +
  scale_color_manual(values = color_map) +
  
  # Legend layout controls tailored for right-side placement
  guides(
    color = guide_legend(ncol = 1, title.position = "top"),
    shape = guide_legend(ncol = 1, title.position = "top"),
    size  = guide_legend(override.aes = list(shape = 16, alpha = 0.8, linetype = 0), ncol = 1, title.position = "top")
  ) +  
  
  # Revised expression-formatted axis titles (omitting ", log10 scale")
  labs(
    title = "Erosion vs. Aboveground Cover Crop Biomass",
    x = expression(Cover~Crop~Aboveground~Biomass~(kg~ha^-1)),
    y = expression(Water-Borne~Sediment~Erosion~(g~m^-2~d^-1)),
    color = "Treatment",
    shape = "Season",
    size = "Precipitation (mm)"
  ) +
  
  # Standardized Serif Theme Setup
  theme_bw(base_family = "serif") +  
  theme(
    plot.title = element_text(face = "bold", size = 12, hjust = 0.5),
    plot.subtitle = element_text(size = 10, hjust = 0.5),
    axis.text = element_text(color = "black", size = 9),
    axis.title = element_text(face = "bold", size = 10),
    strip.text = element_text(face = "bold", size = 11),
    legend.position = "right",
    legend.box = "vertical",
    legend.title = element_text(face = "bold", size = 9),
    legend.text = element_text(size = 10),
    legend.margin = margin(l = 0.2, unit = "cm"),
    panel.grid.minor = element_blank()
  )

#----EXPLORING THE DATA----


# ==============================================================================
# 1. DESCRIPTIVE STATISTICS TABLE
# ==============================================================================
# Summary metrics across the biomass and precipitation gradients by Site
F24_Erosion_summary <- F24_ERXAGBM_dat %>%
  group_by(Site) %>%
  summarize(
    n             = sum(!is.na(ER_gm2day)),
    Mean_Erosion  = mean(ER_gm2day, na.rm = TRUE),
    SD_Erosion    = sd(ER_gm2day, na.rm = TRUE),
    Mean_Biomass  = mean(AGBM_kgha, na.rm = TRUE),
    Max_Biomass   = max(AGBM_kgha, na.rm = TRUE),
    Precip_mm     = mean(Precip_cumul_mm, na.rm = TRUE), # Constant per site
    .groups = "drop"
  )

F24_Erosion_summary %>%
  mutate(
    Erosion_display = sprintf("%.3f ± %.3f", Mean_Erosion, (SD_Erosion / sqrt(n))),
    Biomass_range   = sprintf("%.0f (Max: %.0f)", Mean_Biomass, Max_Biomass)
  ) %>%
  select(Site, n, Erosion_display, Biomass_range, Precip_mm) %>%
  kable(
    format = "html",
    caption = "Summary of fall 2024 erosion rates, cover crop aboveground biomass, and cumulative precipitation across sites.",
    col.names = c("Site", "n", "Erosion (g/m²/day) ± SE", "Mean Biomass (kg/ha)", "Precipitation (mm)"),
    align = "lcccc"
  ) %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"), full_width = FALSE, position = "left")



# ==============================================================================
# 1. EXPANDED DESCRIPTIVE STATISTICS TABLE BY SITE AND CC TYPE
# ==============================================================================
F24_Erosion_detailed_summary <- F24_ERXAGBM_dat %>%
  group_by(Site, Trt_type) %>%
  summarize(
    n             = sum(!is.na(ER_gm2day)),
    Mean_Erosion  = mean(ER_gm2day, na.rm = TRUE),
    SD_Erosion    = sd(ER_gm2day, na.rm = TRUE),
    SE_Erosion    = SD_Erosion / sqrt(n),
    Mean_Biomass  = mean(AGBM_kgha, na.rm = TRUE),
    Min_Biomass   = min(AGBM_kgha, na.rm = TRUE),
    Max_Biomass   = max(AGBM_kgha, na.rm = TRUE),
    Precip_mm     = mean(Precip_cumul_mm, na.rm = TRUE),
    .groups       = "drop"
  )

F24_Erosion_detailed_summary %>%
  mutate(
    Erosion_display = sprintf("%.3f ± %.3f", Mean_Erosion, SE_Erosion),
    Biomass_range   = sprintf("%.0f (%.0f - %.0f)", Mean_Biomass, Min_Biomass, Max_Biomass)
  ) %>%
  select(Site, Trt_type, n, Erosion_display, Biomass_range, Precip_mm) %>%
  kable(
    format = "html",
    caption = "Detailed summary of fall 2024 erosion rates and cover crop aboveground biomass ranges grouped by site and functional cover crop type.",
    col.names = c("Site", "CC Type", "n", "Erosion (g/m²/day) ± SE", "Biomass Mean (Range) (kg/ha)", "Precipitation (mm)"),
    align = "llrccc"
  ) %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"), full_width = FALSE, position = "left") %>%
  collapse_rows(columns = 1, valign = "top")


#----EXPLORATORY PLOT----
ggplot(F24_ERXAGBM_dat, aes(x = AGBM_kgha, y = ER_gm2day)) +
  # Add a linear trendline per site to visually evaluate slope differences
  geom_smooth(method = "lm", formula = y ~ x, color = "black", linewidth = 0.8, se = FALSE) +
  # Points colored by continuous biomass using the viridis scale
  geom_point(aes(color = AGBM_kgha), size = 2.5, alpha = 0.8) +
  facet_wrap(~ Site, scales = "free_y") +
  scale_color_viridis_c(option = "viridis", name = expression("Biomass (kg ha"^{-1}*")")) +
  labs(
    x = expression("Aboveground Cover Crop Biomass (kg ha"^{-1}*")"),
    y = expression("Erosion Rate (g m"^{-2}*~day^{-1}*")"),
    title = "Exploratory Regression: Sediment Erosion vs. Cover Crop Biomass"
  ) +
  theme_bw(base_family = "serif") +
  theme(
    plot.title     = element_text(face = "bold", size = 12, hjust = -0.1),
    legend.position = "right",
    legend.title    = element_text(face = "bold", size = 11),
    strip.text      = element_text(face = "bold", size = 13),
    axis.text       = element_text(color = "black", size = 11)
  )


# ==============================================================================
# CONDENSED TWO-TRENDLINE EXPLORATORY GRAPH
# ==============================================================================
ggplot(F24_ERXAGBM_dat, aes(x = AGBM_kgha, y = ER_gm2day, color = Site, fill = Site)) +
  # Linear OLS trendlines per site with standard error bands
  geom_smooth(method = "lm", formula = y ~ x, linewidth = 1.2, alpha = 0.15) +
  # Points sized by continuous biomass to show weight across the gradient
  geom_point(aes(size = AGBM_kgha), alpha = 0.7) +
  # High-contrast Viridis scale applied to the discrete Site factor
  scale_color_viridis_d(option = "inferno", begin = 0.4, end = 0.7) +
  scale_fill_viridis_d(option = "inferno", begin = 0.4, end = 0.7) +
  scale_size_continuous(name = expression(bold("Biomass (kg ha"^{-1}*")"))) +
  labs(
    x = expression("Aboveground Cover Crop Biomass (kg ha"^{-1}*")"),
    y = expression("Erosion Rate (g m"^{-2}*~day^{-1}*")"),
    title = "Exploratory Slope Evaluation: Sediment Erosion vs. Cover Crop Biomass",
    color = "Site",
    fill = "Site"
  ) +
  theme_bw(base_family = "serif") +
  theme(
    plot.title     = element_text(face = "bold", size = 12, hjust = 0.5),
    legend.position = "right",
    legend.title    = element_text(face = "bold", size = 9),
    axis.text       = element_text(color = "black", size = 9),
    panel.grid.minor = element_blank()
  )


#----BETTER GRAPH WITH POINT SIZE MODIFICATION----

# ==============================================================================
# REFINED TWO-TRENDLINE EXPLORATORY GRAPH WITH LEGEND AND SCALE TWEAKS
# ==============================================================================
ggplot(F24_ERXAGBM_dat, aes(x = AGBM_kgha, y = ER_gm2day, color = Site, fill = Site)) +
  # Linear OLS trendlines per site with standard error bands
  geom_smooth(method = "lm", formula = y ~ x, linewidth = 1.2, alpha = 0.15) +
  # Points sized by continuous biomass using strict baseline and ceiling dimensions
  geom_point(aes(size = AGBM_kgha), alpha = 0.7) +
  # Truncated Viridis scale: samples colors from a specific internal range (e.g., 0.15 to 0.85)
  scale_color_viridis_d(option = "viridis", begin = 0.10, end = 0.50) +
  scale_fill_viridis_d(option = "viridis", begin = 0.10, end = 0.50) +
  # Explicitly controlling the minimum and maximum physical display size of points
  scale_size_continuous(
    name = expression(bold("Biomass (kg ha"^{-1}*")")),
    range = c(1, 8)
  ) +
  labs(
    x = expression("Aboveground Cover Crop Biomass (kg ha"^{-1}*")"),
    y = expression("Erosion Rate (g m"^{-2}*~day^{-1}*")"),
    title = "Exploratory Slope Evaluation: Sediment Erosion vs. Cover Crop Biomass",
    color = expression(bold("Site")),
    fill = expression(bold("Site"))
  ) +
  theme_bw(base_family = "serif") +
  theme(
    plot.title       = element_text(face = "bold", size = 12, hjust = 0.5),
    legend.position  = "right",
    axis.text        = element_text(color = "black", size = 9),
    panel.grid.minor = element_blank()
  )


#----FITTING THE MODEL----

# ==============================================================================
# 2. CONTINUOUS MIXED-EFFECTS MODEL
# ==============================================================================
# Fit the full interaction model using Satterthwaite ddf
# Formula: erosion ~ biomass * precipitation * Site + (1|Site:Rep)
F24.ErosionXBM.met.MOD <- lmer(
  ER_gm2day ~ AGBM_kgha + (1|Site:Rep),
  data = F24_ERXAGBM_dat)


#----CHECKING NON-TRANSFORMED MODEL----
# 1. Set up a 1-row, 2-column plotting grid
par(mfrow = c(1, 2), family = "serif")

# 2. Residuals vs. Fitted (Tests Homoskedasticity / Constant Variance)
plot(fitted(F24.ErosionXBM.met.MOD), residuals(F24.ErosionXBM.met.MOD),
     xlab = "Fitted Values", ylab = "Residuals",
     main = "Residuals vs. Fitted")
abline(h = 0, col = "red", lty = 2)

# 3. Normal Q-Q Plot (Tests Normality of Residuals)
qqnorm(residuals(F24.ErosionXBM.met.MOD), main = "Normal Q-Q Plot")
qqline(residuals(F24.ErosionXBM.met.MOD), col = "red", lty = 2)

# Reset plotting grid
par(mfrow = c(1, 1))


#----TRANSFORMING THE MODEL----

F24.ErosionXBM.met.LOG_MOD <- lmer(
  log(ER_gm2day) ~ AGBM_kgha + (1|Site:Rep),
  data = F24_ERXAGBM_dat)


#----RECHECKING----
par(mfrow = c(1, 2), family = "serif")

# Residuals vs Fitted: Verify if the funnel shape has been resolved
plot(fitted(F24.ErosionXBM.met.LOG_MOD), residuals(F24.ErosionXBM.met.LOG_MOD),
     xlab = "Fitted Values (Log Scale)", ylab = "Residuals",
     main = "Log-Model: Residuals vs. Fitted")
abline(h = 0, col = "red", lty = 2)

# Normal Q-Q Plot: Verify if the positive tail quantiles pull back onto the line
qqnorm(residuals(F24.ErosionXBM.met.LOG_MOD), main = "Log-Model: Normal Q-Q Plot")
qqline(residuals(F24.ErosionXBM.met.LOG_MOD), col = "red", lty = 2)

par(mfrow = c(1, 1))


#----VARIANCE COMPONENTS----
# Extract and calculate the ICC
vc <- as.data.frame(VarCorr(F24.ErosionXBM.met.LOG_MOD))
sigma2_block <- vc$vcov[vc$grp == "Site:Rep"]
sigma2_resid <- vc$vcov[vc$grp == "Residual"]

icc <- sigma2_block / (sigma2_block + sigma2_resid)
cat("Blocking accounted for", round(icc * 100, 1), "% of the total random variation.\n")

performance::icc(F24.ErosionXBM.met.LOG_MOD)


F24_ErosionXBiomass_Anova <- anova(F24.ErosionXBM.met.LOG_MOD, type = 3, ddf = "Satterthwaite")

#----ANOVA TABLE----
# Format and export the ANOVA Table
library(lme4)
library(lmerTest)
library(emmeans)
library(multcomp)
library(multcompView)
library(ggeffects)
library(tidyverse)
library(knitr)
library(kableExtra)
library(performance)

# ==============================================================================
# 1. MODEL ESTIMATION & EXPORTABLE ANOVA TABLE PIPELINE
# ==============================================================================

# Fit the linear log-response mixed model
F24.ErosionXBM.met.LOG_MOD <- lmer(
  log(ER_gm2day) ~ AGBM_kgha + (1 | Site:Rep),
  data = F24_ERXAGBM_dat)

view(F24_ERXAGBM_dat)

#----MODEL SUMMARY, EXPONENTIATING INTERCEPT AND SLOPE TO GET PER KG BIOMASS % CHANGE----

summary(F24.ErosionXBM.met.LOG_MOD)

MOD_Intercept = exp(0.253)
MOD_Intercept
Mod_Slope = exp(-0.00021)
Mod_Slope
Unit_perc_change = (Mod_Slope - 1) * 100
Unit_perc_change
Unit_effect_perc = Unit_perc_change * 1000 #kg/ha CC biomass in this case
Unit_effect_perc 


# Generate Type III ANOVA with Satterthwaite ddf and pipe into HTML layout
F24.ErosionXBM_Anova<- anova(F24.ErosionXBM.met.LOG_MOD, type = 3, ddf = "Satterthwaite") %>%
  as.data.frame() %>%
  tibble::rownames_to_column("Source") %>%
  mutate(
    Source = gsub("AGBM_kgha", "Shoot Biomass", Source),
    across(c(`Sum Sq`, `Mean Sq`, `F value`), ~ round(.x, 2)),
    NumDF  = round(NumDF, 0),
    DenDF  = round(DenDF, 2)
  ) %>%
  mutate(
    `p-value` = case_when(
      is.na(`Pr(>F)`)   ~ "NS",
      `Pr(>F)` < 0.001  ~ "<b>&lt; 0.001</b>",
      `Pr(>F)` < 0.05   ~ sprintf("<b>%.4f</b>", `Pr(>F)`),
      TRUE              ~ sprintf("%.4f", `Pr(>F)`)
    )
  ) %>%
  select(Source, `Sum Sq`, `Mean Sq`, NumDF, DenDF, `F value`, `p-value`) %>%
  kable(
    format = "html",
    caption = "Type III Analysis of Variance Table for log-transformed erosion rate (g m⁻² day⁻¹) in fall 2024. Data analyzed via mixed model with random blocks: log(Erosion) ~ Biomass + (1|Site:Rep).",
    col.names = c("Source of Variation", "Sum Sq", "Mean Sq", "Num Df", "Den Df", "F value", "p-value"),
    align = "lcccccc",
    escape = FALSE
  ) %>%
  kable_styling(
    bootstrap_options = c("striped", "hover", "condensed"),
    full_width = FALSE,
    position = "left"
  ) %>%
  row_spec(0, bold = TRUE, color = "black")

# Print the generated HTML table to viewer
F24.ErosionXBM_Anova

# ==============================================================================
# 2. FIXED EFFECTS SLOPE EXTRACTION & EQUATION GENERATION
# ==============================================================================

# Extract parameters to map mathematically inside the plot space
fe            <- fixef(F24.ErosionXBM.met.LOG_MOD)
intercept_val <- round(fe[1], 4)
slope_val     <- round(fe[2], 5)
r2_m          <- round(performance::r2(F24.ErosionXBM.met.LOG_MOD)$R2_marginal, 2)
r2_c          <- round(performance::r2(F24.ErosionXBM.met.LOG_MOD)$R2_conditional, 2)

eq_text <- paste0(
  "log(Erosion) = ", intercept_val, " + (", slope_val, ") × Biomass\n",
  "Marginal R² = ", r2_m,
  "\nConditional R² = ", r2_c
)

# ==============================================================================
# 3. CONTINUOUS PREDICTION GRID GENERATION
# ==============================================================================

# Extract continuous trend grid matching the native log tracking scale
pred_log <- ggpredict(F24.ErosionXBM.met.LOG_MOD, terms = "AGBM_kgha [all]", back_transform = FALSE) %>%
  as.data.frame()

# ==============================================================================
# 4. EXPORTABLE VISUALIZATION: LINEAR LOG-RESPONSE TRAJECTORY
# ==============================================================================

 ggplot() +
  # Raw data observation matrix points colored by Site environment
  geom_point(
    data = F24_ERXAGBM_dat, 
    aes(x = AGBM_kgha, y = log(ER_gm2day), color = Site), 
    alpha = 0.6, 
    size = 3
  ) +
  # Universal slope 95% confidence interval ribbon
  geom_ribbon(
    data = pred_log, 
    aes(x = x, ymin = conf.low, ymax = conf.high), 
    alpha = 0.15, 
    fill = "black"
  ) +
  # Universal fixed effect regression vector line
  geom_line(
    data = pred_log, 
    aes(x = x, y = predicted), 
    linewidth = 1.2, 
    color = "black"
  ) +
  # Empirical equation annotation positioned inside the upper right frame area
  annotate(
    "text", 
    x = max(F24_ERXAGBM_dat$AGBM_kgha) * 0.95, 
    y = max(log(F24_ERXAGBM_dat$ER_gm2day)) * 0.95, 
    label = eq_text, 
    family = "serif", 
    fontface = "bold.italic", 
    hjust = 1, 
    vjust = 1, 
    size = 4
  ) +
  # High-contrast color mapping profile
  scale_color_viridis_d(option = "viridis", begin = 0.10, end = 0.50) +
  labs(
    x = expression("Aboveground Cover Crop Biomass (kg ha"^{-1}*")"),
    y = expression("log(Erosion Rate [g m"^{-2}*~day^{-1}*"])"),
    title = "Fall 2024 Linear Log-Response Erosion Mitigation Model",
    color = expression(bold("Site")),
    caption = paste0(
      "Trendline represents the predicted fixed-effect linear profile from the mixed model.\n",
      "Shaded bounding region represents the 95% Confidence Interval.\n",
      "Random environmental variation across blocks is accounted for by the (1|Site:Rep) intercept design."
    )
  ) +
  theme_bw(base_family = "serif") +
  theme(
    plot.title       = element_text(face = "bold", size = 12, hjust = 0.5),
    legend.position  = "right",
    axis.text        = element_text(color = "black", size = 9),
    panel.grid.minor = element_blank()
  )
















#----S25 ERoSION ~ BIOMASS MODEL, BETTER WORKFLOW FOR GETTING R^2 AND SLOPES----



# 1. Fit Interaction Model
S25.ErosionXBM.met.LOG_MOD <- lmer(
  log(ER_gm2day) ~ AGBM_kgha * Precip_cumul_mm + (1 | Site:Rep),
  data = S25_ERXAGBM_dat
)

# 2. Extract Coefficients, R-Squared, and Build Exact Formula
fe <- fixef(S25.ErosionXBM.met.LOG_MOD)
b0 <- round(fe["(Intercept)"], 4)
b1 <- round(fe["AGBM_kgha"], 6)
b2 <- round(fe["Precip_cumul_mm"], 5)
b3 <- round(fe["AGBM_kgha:Precip_cumul_mm"], 8)

r2_val <- performance::r2(S25.ErosionXBM.met.LOG_MOD)
r2_m   <- round(r2_val$R2_marginal, 2)
r2_c   <- round(r2_val$R2_conditional, 2)

eq_text <- paste0(
  "log(Erosion) = ", b0, " + (", b1, ")BM + (", b2, ")P + (", b3, ")BM×P\n",
  "Marginal R² = ", r2_m, " | Conditional R² = ", r2_c
)

# 3. Type III ANOVA Table
S25.ErosionXBM_Anova <- anova(S25.ErosionXBM.met.LOG_MOD, type = 3, ddf = "Satterthwaite") %>%
  as.data.frame() %>%
  tibble::rownames_to_column("Source") %>%
  mutate(
    Source = gsub("AGBM_kgha", "Shoot Biomass", Source),
    Source = gsub("Precip_cumul_mm", "Precipitation", Source),
    across(c(`Sum Sq`, `Mean Sq`, `F value`), ~ round(.x, 2)),
    NumDF  = round(NumDF, 0),
    DenDF  = round(DenDF, 2),
    `p-value` = case_when(
      is.na(`Pr(>F)`)   ~ "NS",
      `Pr(>F)` < 0.001 ~ "<b>&lt; 0.001</b>",
      `Pr(>F)` < 0.05  ~ sprintf("<b>%.4f</b>", `Pr(>F)`),
      TRUE            ~ sprintf("%.4f", `Pr(>F)`)
    )
  ) %>%
  select(Source, `Sum Sq`, `Mean Sq`, NumDF, DenDF, `F value`, `p-value`) %>%
  kable(
    format = "html",
    caption = "Type III ANOVA for log-transformed erosion rate (g m⁻² day⁻¹) in spring 2025: log(Erosion) ~ Biomass * Precip + (1|Site:Rep).",
    col.names = c("Source of Variation", "Sum Sq", "Mean Sq", "Num Df", "Den Df", "F value", "p-value"),
    align = "lcccccc",
    escape = FALSE
  ) %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"), full_width = FALSE, position = "left") %>%
  row_spec(0, bold = TRUE, color = "black")

S25.ErosionXBM_Anova

# 4. Predictions holding Precipitation at its Mean Value
mean_precip <- mean(S25_ERXAGBM_dat$Precip_cumul_mm, na.rm = TRUE)

pred_log <- emmeans(
  S25.ErosionXBM.met.LOG_MOD,
  ~ AGBM_kgha,
  at = list(
    AGBM_kgha = seq(min(S25_ERXAGBM_dat$AGBM_kgha), max(S25_ERXAGBM_dat$AGBM_kgha), length.out = 100),
    Precip_cumul_mm = mean_precip
  )
) %>% as.data.frame()

# 5. Visualizing Response Line across Biomass
ggplot() +
  geom_point(
    data = S25_ERXAGBM_dat, 
    aes(x = AGBM_kgha, y = log(ER_gm2day), color = Site), 
    alpha = 0.6, 
    size = 3
  ) +
  geom_ribbon(
    data = pred_log, 
    aes(x = AGBM_kgha, ymin = lower.CL, ymax = upper.CL), 
    alpha = 0.15, 
    fill = "black"
  ) +
  geom_line(
    data = pred_log, 
    aes(x = AGBM_kgha, y = emmean), 
    linewidth = 1.2, 
    color = "black"
  ) +
  annotate(
    "text", 
    x = max(S25_ERXAGBM_dat$AGBM_kgha) * 0.98, 
    y = max(log(S25_ERXAGBM_dat$ER_gm2day)) * 0.98, 
    label = eq_text, 
    family = "serif", 
    fontface = "bold.italic", 
    hjust = 1, 
    vjust = 1, 
    size = 4
  ) +
  scale_color_viridis_d(option = "viridis", begin = 0.10, end = 0.50) +
  labs(
    x = expression("Aboveground Cover Crop Biomass (kg ha"^{-1}*")"),
    y = expression("log(Erosion Rate [g m"^{-2}*~day^{-1}*"])"),
    title = "Spring 2025 Erosion Mitigation Model (Biomass x Precipitation)",
    color = expression(bold("Site")),
    caption = paste0(
      "Trendline represents marginal linear prediction across biomass evaluated at mean precipitation (", round(mean_precip, 1), " mm).\n",
      "Shaded bounding region represents the 95% Confidence Interval.\n",
      "Random environmental variation across blocks is accounted for by the (1|Site:Rep) intercept design."
    )
  ) +
  theme_bw(base_family = "serif") +
  theme(
    plot.title       = element_text(face = "bold", size = 12, hjust = 0.5),
    legend.position  = "right",
    axis.text        = element_text(color = "black", size = 9),
    panel.grid.minor = element_blank()
  )





#----S25 EROSION BY BIOMASS QUADRATIC----

# 1. Fit Quadratic Mixed Model (AGBM_kgha as linear and quadratic fixed effects)
S25.ErosionXBM.QUAD_MOD <- lmer(
  log(ER_gm2day) ~ AGBM_kgha + I(AGBM_kgha^2) + (1 | Site:Rep),
  data = S25_ERXAGBM_dat
)

# 2. Extract Parameters, Equations, and R-Squared Values
fe <- fixef(S25.ErosionXBM.QUAD_MOD)
b0 <- round(fe["(Intercept)"], 4)
b1 <- round(fe["AGBM_kgha"], 6)
b2 <- round(fe["I(AGBM_kgha^2)"], 9)

r2_val <- performance::r2(S25.ErosionXBM.QUAD_MOD)
r2_m   <- round(r2_val$R2_marginal, 2)
r2_c   <- round(r2_val$R2_conditional, 2)

eq_text <- paste0(
  "log(Erosion) = ", b0, " + (", b1, ")X + (", b2, ")X²\n",
  "Marginal R² = ", r2_m, " | Conditional R² = ", r2_c
)

# 3. Generate Prediction Grid along the Curve (Response/Log Scale)
pred_grid <- emmeans(
  S25.ErosionXBM.QUAD_MOD,
  ~ AGBM_kgha,
  at = list(AGBM_kgha = seq(min(S25_ERXAGBM_dat$AGBM_kgha, na.rm = TRUE), 
                            max(S25_ERXAGBM_dat$AGBM_kgha, na.rm = TRUE), 
                            length.out = 100))
) %>% as.data.frame()

# 4. Visualization: Quadratic Log-Response Curve
ggplot() +
  geom_point(
    data = S25_ERXAGBM_dat,
    aes(x = AGBM_kgha, y = log(ER_gm2day), color = Site),
    alpha = 0.6,
    size = 3
  ) +
  geom_ribbon(
    data = pred_grid,
    aes(x = AGBM_kgha, ymin = lower.CL, ymax = upper.CL),
    alpha = 0.15,
    fill = "black"
  ) +
  geom_line(
    data = pred_grid,
    aes(x = AGBM_kgha, y = emmean),
    linewidth = 1.2,
    color = "black"
  ) +
  annotate(
    "text",
    x = max(S25_ERXAGBM_dat$AGBM_kgha, na.rm = TRUE) * 0.98,
    y = max(log(S25_ERXAGBM_dat$ER_gm2day), na.rm = TRUE) * 0.98,
    label = eq_text,
    family = "serif",
    fontface = "bold.italic",
    hjust = 1,
    vjust = 1,
    size = 4
  ) +
  scale_color_viridis_d(option = "viridis", begin = 0.10, end = 0.50) +
  labs(
    x = expression("Aboveground Cover Crop Biomass (kg ha"^{-1}*")"),
    y = expression("log(Erosion Rate [g m"^{-2}*~day^{-1}*"])"),
    title = "Spring 2025 Quadratic Log-Response Erosion Model",
    color = expression(bold("Site")),
    caption = paste0(
      "Trendline represents the predicted fixed-effect quadratic response profile.\n",
      "Shaded region represents the 95% Confidence Interval.\n",
      "Random environmental variation across blocks is accounted for by the (1|Site:Rep) intercept design."
    )
  ) +
  theme_bw(base_family = "serif") +
  theme(
    plot.title       = element_text(face = "bold", size = 12, hjust = 0.5),
    legend.position  = "right",
    axis.text        = element_text(color = "black", size = 9),
    panel.grid.minor = element_blank()
  )








#---Erosion prediction model- Fall and Spring together, O and C not taken out (dead biomass)----

# Erosion ~ biomass * Precip + (1|Site/Rep), dat = F24XS25_ERXAGBM_dat 

SWCS_ER_add_mod <- lmer(
  log(ER_gm2day) ~ log(AGBM_kgha + 1) + Precip_cumul_mm + (1 |Site), 
  data = F24XS25_ERXAGBM_dat)

# Summary and ANOVA
summary(SWCS_ER_add_mod)
anova(SWCS_ER_add_mod, type = 3)

# 2. Extract Marginal Trend (Slope) for Cumulative Precipitation
# Gives the log-scale slope for precipitation held at average biomass
precip_slope <- emtrends(SWCS_ER_add_mod, ~ 1, var = "Precip_cumul_mm")
summary(precip_slope)

# 3. Extract Marginal Trend (Elasticity) for Biomass
biomass_slope <- emtrends(SWCS_ER_add_mod, ~ 1, var = "log(AGBM_kgha + 1)")
summary(biomass_slope)

# 4. Comparative Prediction Grid across Biomass and Rain Spectrum
# Shows predicted response-scale erosion (lb/ac) demonstrating how rain outweighs biomass
er_add_grid <- emmeans(
  SWCS_ER_add_mod,
  ~ AGBM_kgha * Precip_cumul_mm,
  at = list(
    AGBM_kgha = c(0, 500, 2000, 5000, 10000),
    Precip_cumul_mm = c(0, 10, 50, 100, 200)))

#CC_AGBM_kgha_S25 = c(0, 500, 2000, 5000, 10000),
#Precip_cumul_mm = c(0, 10, 50, 100, 200)

# 2. Back-transform to response scale (lb/ac) via Delta Method
er_add_resp <- regrid(er_add_grid, transform = "response") %>%
  as.data.frame() %>%
  mutate(
    Biomass_kgha = round(AGBM_kgha),
    Biomass_Factor = factor(Biomass_kgha, levels = c(0, 500, 2000, 5000, 10000))
  )

# Print fixed back-transformed prediction grid
print(er_add_resp)

# 3. Plot Response Curves (Extension & Thesis Figure)
ggplot(er_add_resp, aes(x = Precip_cumul_mm, y = response, color = Biomass_Factor, group = Biomass_Factor)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 3) +
  geom_errorbar(
    aes(ymin = pmax(0, response - SE), ymax = response + SE), 
    width = 5, 
    linewidth = 0.6
  ) +
  scale_color_viridis_d(option = "inferno",begin = 0.15, end = 0.90,
                        name = expression(Cover~Crop~Biomass~(kg~ha^-1))) +
  labs(
    title = "Model-Predicted Water-Borne Sediment Erosion",
    subtitle = "Interacting Effects of Cumulative Precipitation and Cover Crop Biomass",
    x = "Cumulative Precipitation (mm)",
    y = expression(Water-Borne~Sediment~Erosion~(g~m^-2~day^-1)),
    caption = paste0(
      "Lines represent back-transformed estimated marginal means.\n",
      "Error bars represent Delta-method approximated ±1 SE."
    )
  ) +
  theme_bw(base_family = "serif") +
  theme(
    legend.position = "bottom",
    axis.text = element_text(color = "black"),
    axis.title = element_text(size = 11),
    panel.grid.minor = element_blank()
  )










































#---- The below code is for a future model where I might parse trends by some categorical variable----


# 2. Extract slope values and perform Tukey HSD comparisons on the slopes
slope_trends <- emtrends(F24.Future_Erosion_Mod, var = "AGBM_kgha", by = "Site")
slope_cld    <- cld(slope_trends, alpha = 0.05, Letters = letters, adjust = "tukey", decreasing = TRUE) %>%
  as.data.frame() %>%
  mutate(Letter_Label = paste0("Slope letter: ", trimws(.group)))

# 3. Generate back-transformed exponential decay lines fixed to a chosen precipitation level (e.g., 50 mm)
pred_future_back <- ggpredict(F24.Future_Erosion_Mod, terms = c("AGBM_kgha [all]", "Site"), condition = c(Precip_cumul_mm = 50)) %>%
  as.data.frame() %>%
  rename(Site = group) %>%
  left_join(slope_cld, by = "Site") # Merge slope letters directly into prediction grid


# 4. Multi-Slope Plot: Faceted Arithmetic Scale Presentation with CLD Labels
ggplot(pred_future_back, aes(x = x, y = predicted, group = Site, fill = Site, color = Site)) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.15, color = NA) +
  geom_line(linewidth = 1.2) +
  # Map raw data points within their respective facets
  geom_point(data = F24_ERXAGBM_dat, aes(x = AGBM_kgha, y = ER_gm2day, color = Site), inherit.aes = FALSE, alpha = 0.5, size = 2) +
  # Dynamically place the emtrends CLD letter inside each environment's facet panel
  geom_text(aes(x = Inf, y = Inf, label = Letter_Label), hjust = 1.1, vjust = 1.5, color = "black", family = "serif", fontface = "bold", inherit.aes = FALSE) +
  facet_wrap(~ Site, scales = "free_y") +
  scale_color_viridis_d(option = "viridis", begin = 0.15, end = 0.85) +
  scale_fill_viridis_d(option = "viridis", begin = 0.15, end = 0.85) +
  labs(
    x = expression("Aboveground Cover Crop Biomass (kg ha"^{-1}*")"),
    y = expression("Erosion Rate (g m"^{-2}*~day^{-1}*")"),
    title = "Site-Specific Sediment Mitigation Curves (Conditioned at 50 mm Precipitation)",
    caption = "Trendlines reflect back-transformed exponential decay profiles.\nLetters indicate significant differences among slope parameters (Tukey HSD, alpha = 0.05)."
  ) +
  theme_bw(base_family = "serif") +
  theme(
    plot.title = element_text(face = "bold", size = 12, hjust = 0.5),
    legend.position = "none",
    strip.text = element_text(face = "bold", size = 11),
    panel.grid.minor = element_blank()
  )



# Setup representative precipitation levels (e.g., 2, 4, and 6 inches)
# and a baseline cover crop biomass level (e.g., 1000 kg/ha) for slope evaluation
precip_levels <- c(2, 4, 6)
biomass_baseline <- 1000

# 1. Fit the complex three-way interaction mixed model
F24_complex_mod <- lmer(
  log(Nitrate_NitriteN_kgha) ~ CC_biomass_kgha * precipitation * Site + (1|Site:Rep), 
  data = F24_3depth_soilN
)

# 2. Estimate the physical slopes (dY/dX) on the response scale (kg/ha N per kg/ha biomass)
# evaluated at specific precipitation levels and the biomass baseline
slopes_response <- emtrends(
  F24_complex_mod, 
  ~ Site | precipitation, 
  var = "CC_biomass_kgha",
  at = list(precipitation = precip_levels, CC_biomass_kgha = biomass_baseline),
  transform = "response"
)

# 3. Generate pairwise comparisons and CLD letters
# (adjust = "tukey" compares Sites within each precipitation level)
slopes_cld <- cld(
  slopes_response, 
  alpha = 0.05, 
  Letters = letters, 
  adjust = "tukey", 
  decreasing = TRUE
)

# 4. Clean and format the data for plotting
plot_data_slopes <- as.data.frame(slopes_response) %>%
  left_join(
    as.data.frame(slopes_cld) %>% 
      select(Site, precipitation, .group), 
    by = c("Site", "precipitation")
  ) %>%
  mutate(
    Letter = trimws(.group),
    # Create clean facet labels for precipitation levels
    Precip_Label = factor(
      precipitation, 
      levels = precip_levels,
      labels = paste("Precipitation:", precip_levels, "in")
    )
  )

# 5. Plot the back-transformed physical slopes with CLD letters
ggplot(plot_data_slopes, aes(x = Site, y = CC_biomass_kgha.trend, fill = Site)) +
  geom_bar(stat = "identity", color = "black", alpha = 0.7, width = 0.6) +
  geom_errorbar(
    aes(ymin = CC_biomass_kgha.trend - SE, ymax = CC_biomass_kgha.trend + SE), 
    width = 0.15
  ) +
  geom_text(
    aes(label = Letter, y = CC_biomass_kgha.trend + SE), 
    vjust = -0.5, 
    size = 4, 
    fontface = "bold"
  ) +
  facet_wrap(~ Precip_Label) +
  scale_fill_viridis_d(option = "mako") +
  labs(
    y = expression("Change in Soil Nitrate-N (kg ha"^{-1}*" per kg ha"^{-1}*" CC Biomass)"),
    x = "Site",
    caption = paste0(
      "Slopes represent back-transformed derivatives (dY/dX) evaluated at ", biomass_baseline, " kg/ha CC biomass.\n",
      "Error bars represent Delta-method approximated ±1 SE.\n",
      "Letters indicate significant differences among sites within each precipitation level (Tukey HSD, alpha = 0.05)."
    )
  ) +
  theme_bw(base_family = "serif") +
  theme(
    legend.position = "none",
    axis.text.x = element_text(angle = 45, hjust = 1, color = "black", size = 10),
    axis.text.y = element_text(color = "black", size = 10),
    strip.text = element_text(face = "bold", size = 11),
    panel.grid.major.x = element_blank()
  )

#-----------------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------------
# Fit the three-way interaction model by filtering data inline for Deployment 1
#erosion_mod_three_way <- lmer(
  #Erosion ~ CC_biomass * Site * Season + (1 | Site:Rep), 
  #data = subset(erosion_site_year, Deployment == "D1" & Season %in% c("F24", "S25")))





