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

#----Spring 2025 (S25)----

#----READING IN THE DATA----



#----S25 LYSIMETER DATA----

S25_Lysimeter = read.csv("S25_Lysimeter.csv")


#----SETTING FACTOR LEVELS----

library(dplyr)

# Clean and set site-specific deployment levels for Spring 2025 Lysimeter data
S25_Lysimeter <- S25_Lysimeter %>% 
  mutate(
    Start_date      = as.Date(Start_date, format = "%Y-%m-%d"),
    End_date        = as.Date(End_date, format = "%Y-%m-%d"),
    Start_date_disp = format(Start_date, "%d-%b"),
    End_date_disp   = format(End_date, "%d-%b"),
    Site            = factor(Site, levels = c("Saint Paul", "Waseca")),
    Rep             = factor(Rep),
    Season          = factor(Season),
    Season_Full = case_when(
      Season == "S25" ~ "Spring 2025"),
    # Set site-specific Deployment levels: Saint Paul (D1-D10) vs. Waseca (D1-D12)
    Deployment      = case_when(
      Site == "Saint Paul" ~ factor(Deployment, levels = paste0("D", 1:10)),
      Site == "Waseca"     ~ factor(Deployment, levels = paste0("D", 1:12))),
    Trt             = factor(Trt, levels = c("C", "R1", "R4")),
    Treatment_Full  = case_when(
      Trt == "C"  ~ "Fallow",
      Trt == "R1" ~ "Rye 10",
      Trt == "R4" ~ "Rye 80",
      TRUE        ~ NA_character_),
    Treatment_Full  = factor(Treatment_Full, levels = c("Fallow", "Rye 10", "Rye 80")),
    Trt_type        = factor(Trt_type, levels = c("No CC", "Cereal Rye"))
  )

#----MERGING THE LYSIMETER DATA WITH THE BIOMASS AND N DATA----

# 1. Create independent spring Soil N + Biomass object retaining Control plots
S25_SoiNXAGBM_ctrl_dat <- left_join(
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
    BMN_kgha_S25     = N_kgha,
    BMN_lbac_S25     = N_lbac,
    CN_rat_S25       = CN_rat
  ) %>%
  group_by(Site) %>%
  mutate(
    # Pull site-level harvest date from non-control plots
    Date_BM_harvest_S25 = coalesce(Date_BM_harvest_S25, Date_BM_harvest_S25[!is.na(Date_BM_harvest_S25)][1]),
    # Explicitly set missing cover crop biomass and biomass N metrics to 0
    across(c(CC_AGBM_kgha_S25, CC_AGBM_lbac_S25, CC_AGBM_mgha_S25, BMN_kgha_S25, BMN_lbac_S25), ~ coalesce(.x, 0))
    # Note: CN_rat_S25 naturally remains NA for control plots
  ) %>%
  ungroup() %>%
  select(
    Site, Rep, Plot, Trt, Treatment_Full, 
    Date_N_samp_S25, Date_BM_harvest_S25,
    Total_N_kgha_S25, Total_N_lbac_S25,
    CC_AGBM_kgha_S25, CC_AGBM_lbac_S25,CC_AGBM_mgha_S25, BMN_kgha_S25, BMN_lbac_S25, CN_rat_S25
  )

# 2. One-to-many merge: Filtered automatically by plots present in S25_Lysimeter
S25_LysXSoiNXAGBM_dat <- S25_Lysimeter %>%
  left_join(
    S25_SoiNXAGBM_ctrl_dat,
    by = c("Site", "Rep", "Plot", "Trt", "Treatment_Full")
  )


#----Merging F24 biomass with S25 lysimeter----

S25LysXF24AGBM = S25_Lysimeter %>%
  left_join(
    F24_AGBM_CN,
    by = c("Site", "Rep", "Plot", "Trt", "Treatment_Full", "Trt_type")
  )

S25LysXF24AGBM $AGBM_kgha[is.na(S25LysXF24AGBM $AGBM_kgha)] <- 0
S25LysXF24AGBM $AGBM_lbac[is.na(S25LysXF24AGBM $AGBM_lbac)] <- 0
S25LysXF24AGBM $AGBM_mgha[is.na(S25LysXF24AGBM $AGBM_mgha)] <- 0
S25LysXF24AGBM $N_kgha[is.na(S25LysXF24AGBM $N_kgha)] <- 0
S25LysXF24AGBM $N_lbac[is.na(S25LysXF24AGBM $N_lbac)] <- 0

S25LysXF24AGBM  <- dplyr::select(S25LysXF24AGBM ,-Project.y,-Season.y, -Season_Full.y,
                                       -Date,-PercentN_tot,-PercentC_tot,
                                 -Nmult, -Cmult, -C_kgha, -C_lbac) 
S25LysXF24AGBM<- S25LysXF24AGBM %>%
  rename(BMN_lbac = N_lbac, BMN_kgha = N_kgha, AGBM_lbac_F24 = AGBM_lbac, AGBM_kgha_F24 = AGBM_kgha,
         AGBM_mgha_F24 = AGBM_mgha, Season = Season.x, Project = Project.x)



view(S25LysXF24AGBM)
#----MAKING THE DATAFRAME WITH THE LYSIMETER ANALYSIS----
#Step 1: Clean and build tracking IDs
S25_Lys_Analysis_dat <- S25_LysXSoiNXAGBM_dat %>%
  mutate(
    # Create the unique ID for every single cup in the field
    Lysimeter_ID = paste(Site, Plot, Lysimeter, sep = "_"),
    # Assign the correct termination date based on the Site column
    Termination_date = case_when(
      Site == "Saint Paul" ~ as.Date("2025-05-16"),
      Site == "Waseca"     ~ as.Date("2025-05-14"),
      TRUE                 ~ as.Date(NA) # Safety net for typos
    ),
    # Calculate continuous timeline relative to each site's specific date
    Days_Since_Term = as.numeric(End_date - Termination_date)
  ) %>%
  # Crucial for the AR correlation models: sort chronologically within cups
  arrange(Site, Rep, Lysimeter_ID, Days_Since_Term)







#----MAKING AN AVERAGE VALUE FOR THE TREATMENTS WITHIN SITE----
S25_LysXBM_meansforplot_Nmean <- S25_LysXSoiNXAGBM_dat %>%
  
  # 1) Average lysimeters within plot × deployment
  group_by(Site, Treatment_Full, Plot, Deployment) %>%
  summarise(
    waterN = mean(Nitrate_NitriteN_mgL, na.rm = TRUE),
    CC_AGBM_mgha_S25 = first(CC_AGBM_mgha_S25),
    .groups = "drop") %>%
  
  # 2) Average across deployments → plot means
  group_by(Site, Treatment_Full, Plot) %>%
  summarise(
    waterN_mean = mean(waterN, na.rm = TRUE),
    CC_AGBM_mgha_S25 = first(CC_AGBM_mgha_S25),
    .groups = "drop")


#Graph with combined avg biomass and lysimeter nitrate (both sites)====
ggplot(S25_LysXBM_meansforplot_Nmean,
       aes(x = CC_AGBM_kgha_S25,
           y = waterN_mean,
           color = Treatment_Full,
           shape = Site)) +
  
  geom_point(alpha = 0.8, size = 4) + 
  geom_smooth(aes(group = Treatment_Full, color = Treatment_Full), method = "lm", se = FALSE, size = 1) +
  scale_color_viridis_d(option = "G", end = 0.9) +
  labs(
    title = "Spring 2025 Porewater Nitrate by Spring CC Shoot Biomass",
    x = expression(Cover~Crop~Biomass~(kg~ha^-1)),
    y = expression(Mean~Porewater~NO[3]^-{}/NO[2]^-{}~(mg~L^-1)),
    color = "Treatment",
    shape = "Site") +
  theme_bw() +
  theme(legend.position = "right")

#----MAKING AN AVERAGE VALUE FOR AGBM AND LYSNI FOR ANNA HISTOGRAM----
S25_LysXBM_meansforplot_NXBM_Mean <- S25_LysXSoiNXAGBM_dat %>%
  
  # 1) Average lysimeters within plot × deployment
  group_by(Site, Treatment_Full, Plot, Deployment) %>%
  summarise(
    waterN = mean(Nitrate_NitriteN_mgL, na.rm = TRUE),
    AGBM_mgha = mean(CC_AGBM_mgha_S25),
    .groups = "drop") %>%
  
  # 2) Average across deployments → plot means
  group_by(Site, Treatment_Full) %>%
  summarise(
    waterN_mean = mean(waterN, na.rm = TRUE),
    AGBM_mgha_mean = mean(AGBM_mgha ),
    .groups = "drop")

library(dplyr)
library(ggplot2)

# --- 1. Data Preparation ---
S25_LysXBM_meansforplot_NXBM_Mean <- S25_LysXSoiNXAGBM_dat %>%
  group_by(Site, Treatment_Full, Plot) %>%
  summarise(
    waterN_mean = mean(Nitrate_NitriteN_mgL, na.rm = TRUE),
    AGBM_mgha_mean = mean(CC_AGBM_mgha_S25, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  filter(Treatment_Full %in% c("Rye 80", "Rye 10", "Fallow")) %>%
  mutate(Treatment_Full = factor(Treatment_Full, levels = c("Rye 80", "Rye 10", "Fallow")))

# Treatment-level means for bars and line
S25_LysXBM_meansforplot_NXBM_Overall_mean <- S25_LysXBM_meansforplot_NXBM_Mean %>%
  group_by(Site, Treatment_Full) %>%
  summarise(
    AGBM_trt_mean = mean(AGBM_mgha_mean, na.rm = TRUE),
    waterN_trt_mean = mean(waterN_mean, na.rm = TRUE),
    .groups = "drop")



# --- 1. Compute Dynamic Scale Factor Per Plot Run ---
# Calculate maximum values across the dataset to find the transformation ratio
max_bm  <- max(S25_LysXBM_meansforplot_NXBM_Overall_mean$AGBM_trt_mean, na.rm = TRUE)
max_no3 <- max(S25_LysXBM_meansforplot_NXBM_Overall_mean$waterN_trt_mean, na.rm = TRUE)

# Multiplier to scale nitrate up to biomass magnitude
scale_factor <- max_bm / max_no3






# --- 2. Plotting with Shared Dual Y-Axes ---
ggplot(S25_LysXBM_meansforplot_NXBM_Overall_mean, aes(x = Treatment_Full)) +
  # Cover Crop Biomass Bars (Left Y-Axis)
  geom_bar(
    aes(y = AGBM_trt_mean, fill = Treatment_Full),
    stat = "identity", color = "black", alpha = 0.7, width = 0.55
  ) +
  
  # Porewater Nitrate Line & Points (Mapped to Primary Height)
  geom_line(
    aes(y = waterN_trt_mean * scale_factor, group = 1),
    color = "firebrick", linewidth = 1
  ) +
  geom_point(
    aes(y = waterN_trt_mean * scale_factor),
    color = "firebrick", size = 2.5
  ) +
  
  # Facet by Site with Shared/Fixed Scales
  facet_wrap(~ Site) +
  scale_fill_manual(values = color_map) +
  
  # Dual Y-Axes Setup
  scale_y_continuous(
    name = expression("Cover Crop Biomass ("*kg~ha^-1*")"),
    sec.axis = sec_axis(~ . / scale_factor, name = expression("Porewater "*NO[3]*"-"*N~"(mg/L)"))
  ) +
  
  # Labels & Styling
  labs(
    x = "Treatment",
  ) +
  theme_bw(base_family = "serif") +
  theme(
    legend.position = "none",
    axis.text.x = element_text(angle = 45, hjust = 1, color = "black"),
    strip.text = element_text(face = "bold", size = 11),
    axis.title.y.right = element_text(color = "black", face = "bold"),
    axis.text.y.right  = element_text(color = "black")
  )


#----One plot by Site----#

S25_LysXBM_meansforplot_NXBMXSite_Overallmean <- S25_LysXBM_meansforplot_NXBM_Mean %>%
  group_by(Treatment_Full) %>%
  summarise(
    AGBM_trt_mean = mean(AGBM_mgha_mean, na.rm = TRUE),
    waterN_trt_mean = mean(waterN_mean, na.rm = TRUE),
    .groups = "drop"
  )


ggplot(S25_LysXBM_meansforplot_NXBMXSite_Overallmean, aes(x = Treatment_Full)) +
  # Cover Crop Biomass Bars (Left Y-Axis)
  geom_bar(
    aes(y = AGBM_trt_mean, fill = Treatment_Full),
    stat = "identity", color = "black", alpha = 0.7, width = 0.55
  ) +
  
  # Porewater Nitrate Line & Points (Mapped to Primary Height)
  geom_line(
    aes(y = waterN_trt_mean * scale_factor, group = 1),
    color = "firebrick", linewidth = 1
  ) +
  geom_point(
    aes(y = waterN_trt_mean * scale_factor),
    color = "firebrick", size = 2.5
  ) +
  
  # Facet by Site with Shared/Fixed Scales
  scale_fill_manual(values = color_map) +
  
  # Dual Y-Axes Setup
  scale_y_continuous(
    name = expression("Cover Crop Biomass ("*kg~ha^-1*")"),
    sec.axis = sec_axis(~ . / scale_factor, name = expression("Porewater "*NO[3]*"-"*N~"(mg/L)"))
  ) +
  
  # Labels & Styling
  labs(
    x = "Treatment",
  ) +
  theme_bw(base_family = "serif") +
  theme(
    legend.position = "none",
    axis.text.x = element_text(angle = 45, hjust = 1, color = "black"),
    strip.text = element_text(face = "bold", size = 11),
    axis.title.y.right = element_text(color = "black", face = "bold"),
    axis.text.y.right  = element_text(color = "black")
  )

#----S25 OVERALL SEASONAL NITRATE TREND----

# 1. Aggregate lysimeter observations by Site, Treatment, and Date
summary_S25_lysim <- S25_Lys_Analysis_dat %>%
  filter(!is.na(Nitrate_NitriteN_mgL), !is.na(Days_Since_Term)) %>%
  group_by(Site, Trt, Treatment_Full, End_date, Days_Since_Term) %>%
  summarise(
    mean_val = mean(Nitrate_NitriteN_mgL, na.rm = TRUE),
    se_val   = sd(Nitrate_NitriteN_mgL, na.rm = TRUE) / sqrt(n()),
    .groups  = "drop"
  )

# 2. Render Faceted Porewater Nitrate Time-Series Plot
ggplot(summary_S25_lysim, aes(x = Days_Since_Term, y = mean_val, color = Treatment_Full, shape = Site, group = Treatment_Full)) +
  # LOESS smooth line per treatment
  geom_smooth(method = "loess", se = FALSE, linewidth = 0.8) +
  # Mean points
  geom_point(size = 2.5, alpha = 0.9) +
  # ±1 SE Error bars
  geom_errorbar(
    aes(ymin = mean_val - se_val, ymax = mean_val + se_val),
    width = 1.5, linewidth = 0.4, alpha = 0.7
  ) +
  # Reference line for cover crop termination (Day 0)
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray40", linewidth = 0.6) +
  # Custom treatment color palette
  scale_color_manual(values = color_map) +
  # Format axes and titles
  labs(
    title = "Porewater Nitrate Dynamics Relative to Cover Crop Termination",
    x = "Days Relative to Termination",
    y = expression(Subsurface~Porewater~NO[3]*"-"*N~(mg/L)),
    color = "Treatment",
    shape = "Site"
  ) +
  theme_bw(base_family = "serif") +
  theme(
    legend.position = "right",
    legend.box = "vertical",
    legend.title = element_text(face = "bold", size = 9),
    legend.text = element_text(size = 10),
    axis.text = element_text(color = "black", size = 9),
    axis.title = element_text(face = "bold", size = 10),
    strip.text = element_text(face = "bold", size = 11),
    panel.grid.minor = element_blank()
  )








#----Lysimeter N by Treatment * Site----
# lmer(Nitrate_NitriteN_mgL ~ Trt * Site  + (1 | Site:Rep) + (1 | Site:Rep:Plot) + (1 | Lysimeter_ID), data = S25_Lys_Analysis_dat
lys_mod <- lmer(
  log(Nitrate_NitriteN_mgL) ~ Treatment_Full * Site  + (1 | Site:Rep:Plot) + (1 | Lysimeter_ID),
  data = S25_Lys_Analysis_dat)


# 2. View Summary and Type III ANOVA
summary(lys_mod)
anova(lys_mod, type = 3)

# 3. Extract Estimated Marginal Means partitioned by Site
lys_emm <- emmeans(lys_mod, ~ Treatment_Full| Site)

# 4. Generate Compact Letter Display (CLD) within each Site
lys_cld <- cld(
  lys_emm,
  alpha = 0.05,
  Letters = letters,
  adjust = "tukey",
  decreasing = TRUE
)

# View CLD output table
as.data.frame(lys_cld) %>%
  mutate(Letter = trimws(.group))



# 1. Back-transform emmeans to the response scale using regrid()
lys_emm_response <- regrid(lys_emm, transform = "response")

# 2. Extract compact letter display on response scale and detect CLD column
lys_cld_df <- as.data.frame(cld(lys_emm_response, alpha = 0.05, Letters = letters, adjust = "tukey", decreasing = TRUE))
group_col <- intersect(c(".group", "group"), names(lys_cld_df))[1]

# 3. Assemble clean plot dataframe with back-transformed emmeans, symmetric SEs, and trimmed letters
lys_plot_data <- as.data.frame(lys_emm_response) %>%
  left_join(
    lys_cld_df %>% select(Treatment_Full, Site, Letter_raw = all_of(group_col)),
    by = c("Treatment_Full", "Site")
  ) %>%
  mutate(Letter = trimws(Letter_raw)) %>%
  select(Site, Treatment_Full, response, SE, df, lower.CL, upper.CL, Letter)


lys_plot_data 






#m_regional_lmer <- lmer(Nitrate_NitriteN_mgL ~ biomass * precipitation * days_since termination + (1 | Site:Rep) + (1 | Site:Rep:Plot) + (1 | Lysimeter_ID), data = S25_Lys_Analysis_dat
lys_cont_mod <- lmer(
  Nitrate_NitriteN_mgL ~ CC_AGBM_kgha_S25 * Precip_cumul_mm * Days_Since_Term + 
    (1 | Site:Rep:Plot) + (1 | Lysimeter_ID),
  data = S25_Lys_Analysis_dat)

summary(lys_cont_mod)
anova(lys_cont_mod, type = 3)

# 2. Marginal Biomass Slopes (Elasticity / Mitigation Rate across Time & Rain)
biomass_slopes <- emtrends(
  lys_cont_mod, 
  ~ Days_Since_Term * Precip_cumul_mm, 
  var = "CC_AGBM_kgha_S25",
  at = list(
    Days_Since_Term = c(-14, 0, 14, 28, 50, 90),
    Precip_cumul_mm = c(0, 10, 20, 50, 100, 150, 200)))

summary(biomass_slopes)

# 3. Model Prediction Grid Across Key Benchmark Conditions
# Model is on response scale (no log transformation required)
lys_pred_grid <- emmeans(
  lys_cont_mod,
  ~ CC_AGBM_kgha_S25 * Precip_cumul_mm * Days_Since_Term,
  at = list(
    CC_AGBM_kgha_S25 = c(0, 500, 2000, 5000, 10000),
    Precip_cumul_mm = c(0, 10, 50, 100, 200),
    Days_Since_Term = c(-14, 0, 30, 90) # Selected timepoints for clean plotting
  )
) %>%
  as.data.frame() %>%
  mutate(
    Biomass_kgha = round(CC_AGBM_kgha_S25),
    Biomass_Factor = factor(Biomass_kgha, levels = c(0, 500, 2000, 5000, 10000)),
    Day_Label = factor(
      paste(Days_Since_Term, "Days Post-Termination"),
      levels = paste(c(-14, 0, 30, 90), "Days Post-Termination")))

print(lys_pred_grid)

# 4. Faceted Response Plot Across Timepoints
ggplot(lys_pred_grid, aes(x = Precip_cumul_mm, y = emmean, color = Biomass_Factor, group = Biomass_Factor)) +
  geom_line(linewidth = 1.1) +
  geom_point(size = 2.5) +
  geom_errorbar(
    aes(ymin = pmax(0, emmean - SE), ymax = emmean + SE), 
    width = 0.25, 
    linewidth = 0.5
  ) +
  facet_wrap(~ Day_Label, ncol = 2) +
  scale_color_viridis_d(
    option = "inferno", 
    begin = 0.15, 
    end = 0.80, 
    name = expression(Cover~Crop~Biomass~(kg~ha^-1))
  ) +
  labs(
    title = "Model-Predicted Subsurface Porewater Nitrate-N Dynamics",
    x = "Cumulative Precipitation (mm)",
    y = expression(Porewater~NO[3]*"-"*N~(mg~L^-1)),
    caption = "Points represent estimated marginal means; error bars represent ±1 SE derived from lmer model variance."
  ) +
  theme_bw(base_family = "serif") +
  theme(
    legend.position = "bottom",
    axis.text = element_text(color = "black"),
    axis.title = element_text(face = "bold"),
    strip.text = element_text(face = "bold", size = 10),
    panel.grid.minor = element_blank()
  )


S25LysXF24AGBM  <- S25LysXF24AGBM  %>%
  mutate(
    # Create the unique ID for every single cup in the field
    Lysimeter_ID = paste(Site, Plot, Lysimeter, sep = "_"),
    # Assign the correct termination date based on the Site column
    Termination_date = case_when(
      Site == "Saint Paul" ~ as.Date("2025-05-16"),
      Site == "Waseca"     ~ as.Date("2025-05-14"),
      TRUE                 ~ as.Date(NA) # Safety net for typos
    ),
    # Calculate continuous timeline relative to each site's specific date
    Days_Since_Term = as.numeric(End_date - Termination_date)
  ) %>%
  # Crucial for the AR correlation models: sort chronologically within cups
  arrange(Site, Rep, Lysimeter_ID, Days_Since_Term)

#----F24 BIOMASS EFFECTS ON EROSION AND LEACHING, F24 BIOMASS IS IMPORTANT FOR NITRATE LEACHING----
# S25LysXF24AGBM    S25porewater N ~ F24CC biomass (1 | Site:Rep) + (1 | Site:Rep:Plot) 

F24BM.S25_lysni.mod = lmer(log(Nitrate_NitriteN_mgL) ~ log(AGBM_kgha_F24 + 1) +
                             (1 | Site:Rep:Plot) + (1|Lysimeter_ID), data = S25LysXF24AGBM ) 


summary(F24BM.S25_lysni.mod)
anova(F24BM.S25_lysni.mod, type = 3)




#----S25 lysimeter by F24 C/N----
F24CN.S25_lysni.mod = lmer(log(Nitrate_NitriteN_mgL) ~ log(CN_rat) +
                             (1 | Site:Rep:Plot) + (1|Lysimeter_ID), data = S25LysXF24AGBM ) 


summary(F24CN.S25_lysni.mod)
anova(F24CN.S25_lysni.mod, type = 3)



#----S25lysimeter by F24 CC BMNitrogen----
F24BMN.S25_lysni.mod = lmer(log(Nitrate_NitriteN_mgL) ~ BMN_kgha +
                              (1 | Site:Rep:Plot) + (1|Lysimeter_ID), data = S25LysXF24AGBM ) 


summary(F24BMN.S25_lysni.mod)
anova(F24BMN.S25_lysni.mod, type = 3)



#----S25 lysimster by S25 C/N

S25CN.S25_lysni.mod = lmer(log(Nitrate_NitriteN_mgL) ~ log(CN_rat_S25) +
                             (1 | Site:Rep:Plot) + (1|Lysimeter_ID), data = S25_Lys_Analysis_dat ) 


summary(S25CN.S25_lysni.mod)
anova(S25CN.S25_lysni.mod, type = 3)


#----S25 lysimeter by S25 BMNitrogen

S25BMN.S25_lysni.mod = lmer(log(Nitrate_NitriteN_mgL) ~ BMN_kgha_S25 +
                              (1 | Site:Rep:Plot) + (1|Lysimeter_ID), data = S25_Lys_Analysis_dat ) 


summary(S25BMN.S25_lysni.mod )
anova(S25BMN.S25_lysni.mod , type = 3)


