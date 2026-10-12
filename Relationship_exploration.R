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





#----EROSION WORK----

library(scales) 

ggplot(F24XS25_ERXAGBM_dat, aes(x = AGBM_lbac, y = ER_lbac, color = Trt_type)) +
  geom_point(alpha = 0.6, size = 2) +
  geom_smooth(method = "lm", se = TRUE, alpha = 0.2) +
  facet_wrap(~ Season, nrow = 1, scales = "fixed") +
  # Log10 Y-axis transformation with clean tick labels
  scale_y_log10(
    labels = scales::label_log(digits = 2),
    expand = expansion(mult = c(0.05, 0.15))
  ) +
  scale_color_manual(values = color_map_type) +
  scale_fill_manual(values = color_map_type) +
  # Standard log tick marks on the left axis
  annotation_logticks(sides = "l") +
  labs(
    x = expression(Cover~Crop~Aboveground~Biomass~(lb~ac^-1)),
    y = expression(Water-Borne~Sediment~Erosion~(lb~ac^-1))
  ) +
  theme_bw(base_family = "serif") +
  theme(
    legend.position = "none",
    axis.text = element_text(color = "black", size = 10),
    axis.title = element_text(face = "bold", size = 11),
    strip.text = element_text(face = "bold", size = 11),
    panel.grid.minor = element_blank()
  )




ggplot(F24XS25_ERXAGBM_dat, aes(x = AGBM_lbac, y = ER_lbac)) +
  # Points mapped to Site, Size, and Treatment Color
  geom_point(aes(color = Treatment_Full, shape = Site, size = Precip_cumul_in), alpha = 0.8) +
  
  # Separate linear trendlines per Treatment (se = FALSE keeps plot uncluttered)
  geom_smooth(aes(color = Trt_type, group = Trt_type), method = "lm", se = FALSE, linewidth = 0.8) + 
  
  # Preserved exact log10 scales with comma labels
  scale_x_log10(labels = scales::comma_format()) +
  scale_y_log10(labels = scales::comma_format()) +
  scale_color_manual(values = color_map) +
  
  # Legend layout controls
  guides(
    color = guide_legend(nrow = 2, title.position = "top"),
    shape = guide_legend(title.position = "top"),
    size  = guide_legend(override.aes = list(shape = 16, alpha = 0.8, linetype = 0), title.position = "top")
  ) +  
  
  # Formal expression-formatted scientific axis titles
  labs(
    title = "Erosion vs. Aboveground Cover Crop Biomass",
    x = expression(Cover~Crop~Aboveground~Biomass~(lb~ac^-1)*","~log[10]~scale),
    y = expression(Water-Borne~Sediment~Erosion~(lb~ac^-1)*","~log[10]~scale),
    color = "Treatment",
    shape = "Site",
    size = "Precipitation (in)"
  ) +
  
  # Standardized Serif Theme Setup
  theme_bw(base_family = "serif") +  
  theme(
    plot.title = element_text(face = "bold", size = 12, hjust = 0.5),
    plot.subtitle = element_text(size = 10, hjust = 0.5),
    axis.text = element_text(color = "black", size = 9),
    axis.title = element_text(face = "bold", size = 10),
    strip.text = element_text(face = "bold", size = 11),
    legend.position = "bottom",
    legend.box = "vertical",
    legend.title = element_text(face = "bold", size = 9),
    legend.text = element_text(size = 8),
    legend.margin = margin(t = 0, unit = "cm"),
    panel.grid.minor = element_blank()
  )




  
library(ggplot2)
library(scales)

#----UPDATED EROSION PLOT----




F24XS25_ERXAGBM_dat  %>%
  filter(!(Site == "Lamberton" & Trt_type %in% c("No CC", "Oat + Radish") &
             Season == "S25"))%>%
ggplot(aes(x = AGBM_kgha, y = ER_kgha)) +
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
    y = expression(Water-Borne~Sediment~Erosion~(kg~ha^-1)),
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




ggplot(S25_ERXAGBM_dat, aes(x = Treatment_Full, y = ER_gm2day, fill = Treatment_Full)) +
  geom_boxplot(outlier.shape = 16, outlier.size = 2, alpha = 0.7) +
  geom_jitter(width = 0.15, alpha = 0.4, size = 1) +
  
  # Facet wrapped by recoded deployment with UNIFIED y-axis scale
  facet_wrap(~ Deployment_Label, scales = "fixed") +
  
  # Unified Log10 Y-axis with comma labels
  scale_y_log10(labels = scales::comma_format()) +
  scale_fill_manual(values = color_map) +
  
  labs(
    title = "Spring 2025 Water-Borne Sediment Erosion by Treatment",
    x = "Cover Crop Treatment",
    y = expression(Water-Borne~Sediment~Erosion~(g~m^-2~d^-1))
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











#----F24 BIOMASS BY S25 NITRATE AVERAGED----

# 1. Aggregate Spring 2025 Porewater Nitrate by Site, Plot
# TRY ADDING DEPLOYMENT
F24BM_X_S25Lys <- S25LysXF24AGBM %>%
  filter(!is.na(Nitrate_NitriteN_mgL)) %>% # Replace NO3_mgL with your exact nitrate column name
  group_by(Site, Rep, Plot, Trt, Treatment_Full, Trt_type,Deployment, AGBM_mgha_F24) %>%
  summarise(
    mean_NO3_mgL = mean(Nitrate_NitriteN_mgL, na.rm = TRUE),
    .groups = "drop")

# 2. Render Scatterplot with Trendlines
ggplot(F24BM_X_S25Lys, aes(x = AGBM_mgha_F24, y = mean_NO3_mgL, color = Treatment_Full)) +
  geom_point(aes(shape = Site), alpha = 0.8, size = 2.5) +
  # Per-treatment linear regression trendlines
  geom_smooth(method = "lm", se = FALSE, linewidth = 0.8) +
  scale_color_manual(values = color_map) +
  labs(
    title = "Spring Subsurface Porewater Nitrate vs. Fall Cover Crop Biomass",
    x = expression(Fall~2024~Aboveground~Biomass~(kg~ha^-1)),
    y = expression(Spring~2025~Subsurface~Porewater~NO[3]*"-"*N~(mg/L)),
    color = "Treatment",
    shape = "Site"
  ) +
  theme_bw(base_family = "serif") +
  theme(
    legend.position = "right",
    legend.box = "vertical",
    legend.title = element_text(face = "bold", size = 9),
    legend.text = element_text(size = 8),
    axis.text = element_text(color = "black", size = 9),
    axis.title = element_text(face = "bold", size = 10),
    strip.text = element_text(face = "bold", size = 11),
    panel.grid.minor = element_blank()
  )


S25BM_X_S25Lys_avg <- S25_LysXSoiNXAGBM_dat %>%
  filter(!is.na(Nitrate_NitriteN_mgL)) %>% # Replace NO3_mgL with your exact nitrate column name
  group_by(Site, Rep, Plot, Trt, Treatment_Full, Trt_type, Deployment, CC_AGBM_mgha_S25) %>%
  summarise(
    mean_NO3_mgL = mean(Nitrate_NitriteN_mgL, na.rm = TRUE),
    .groups = "drop")


ggplot(S25BM_X_S25Lys_avg, aes(x = CC_AGBM_mgha_S25, y = mean_NO3_mgL, color = Treatment_Full)) +
  geom_point(aes(shape = Site), alpha = 0.8, size = 2.5) +
  # Per-treatment linear regression trendlines
  geom_smooth(method = "lm", se = FALSE, linewidth = 0.8) +
  scale_color_manual(values = color_map) +
  labs(
    title = "Spring Subsurface Porewater Nitrate vs. Spring Cover Crop Biomass",
    x = expression(Spring~2025~Aboveground~Biomass~(kg~ha^-1)),
    y = expression(Spring~2025~Subsurface~Porewater~NO[3]*"-"*N~(mg/L)),
    color = "Treatment",
    shape = "Site"
  ) +
  theme_bw(base_family = "serif") +
  theme(
    legend.position = "right",
    legend.box = "vertical",
    legend.title = element_text(face = "bold", size = 9),
    legend.text = element_text(size = 8),
    axis.text = element_text(color = "black", size = 9),
    axis.title = element_text(face = "bold", size = 10),
    strip.text = element_text(face = "bold", size = 11),
    panel.grid.minor = element_blank()
  )

#----LOOKING INTO C/N AND N RELATIONSHIPS S25----


# biomass N content by treatment
ggplot(S25_AGBM_CN_clean, aes(x = Treatment_Full, y = N_kgha, fill = Treatment_Full)) +
  geom_boxplot(outlier.shape = 16, outlier.size = 2, alpha = 0.7) +
  geom_jitter(width = 0.15, alpha = 0.4, size = 1) +
  
  # Facet wrapped by recoded deployment with UNIFIED y-axis scale
  facet_wrap(~ Site) +
  scale_fill_manual(values = color_map) +
  
  labs(
    title = "Spring 2025 Biomass N Uptake by Treatment",
    x = "Cover Crop Treatment",
    y = expression(CC~Biomass~N~(kg~ha^-1))
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


ggplot(S25_AGBM_CN_clean, aes(x = Treatment_Full, y = CN_rat, fill = Treatment_Full)) +
  geom_boxplot(outlier.shape = 16, outlier.size = 2, alpha = 0.7) +
  geom_jitter(width = 0.15, alpha = 0.4, size = 1) +
  
  # Facet wrapped by recoded deployment with UNIFIED y-axis scale
  facet_wrap(~ Site) +
  scale_fill_manual(values = color_map) +
  
  labs(
    title = "Spring 2025 Biomass C/N Ratio by Treatment",
    x = "Cover Crop Treatment",
    y = expression(CC~Biomass~C/N~Ratio)) +
  theme_bw(base_family = "serif") +
  theme(
    legend.position = "none",
    axis.text.x = element_text(angle = 45, hjust = 1, face = "bold", color = "black"),
    axis.text.y = element_text(color = "black"),
    axis.title = element_text(face = "bold"),
    strip.text = element_text(face = "bold", size = 11),
    panel.grid.minor = element_blank()
  )


ggplot(S25_SoiNXAGBM_dat, aes(x = Treatment_Full, y = Total_N_lbac_S25, fill = Treatment_Full)) +
  geom_boxplot(outlier.shape = 16, outlier.size = 2, alpha = 0.7) +
  geom_jitter(width = 0.15, alpha = 0.4, size = 1) +
  
  # Facet wrapped by recoded deployment with UNIFIED y-axis scale
  facet_wrap(~ Site) +
  scale_fill_manual(values = color_map) +
  
  labs(
    title = "Spring 2025 Soil N by Treatment",
    x = "Cover Crop Treatment",
    y = expression("Total Soil N (lb ac"^{-1}*")")) +
  theme_bw(base_family = "serif") +
  theme(
    legend.position = "none",
    axis.text.x = element_text(angle = 45, hjust = 1, face = "bold", color = "black"),
    axis.text.y = element_text(color = "black"),
    axis.title = element_text(face = "bold"),
    strip.text = element_text(face = "bold", size = 11),
    panel.grid.minor = element_blank()
  )



#----This is possibly groundbreaking----


#----S25 biomass C:N by S25 BMN----

# Filter out Lamberton and generate scatterplot with overall trendline
ggplot(data = S25_SoiNXAGBM_dat %>% filter(Site != "Lamberton"), 
       aes(x = BMN_kgha_S25, y = CN_rat_S25 )) +
  # Map point color to Treatment_Full
  geom_point(aes(color = Treatment_Full), size = 2.5, alpha = 0.8) +
  # Keep trendline global across all points
  geom_smooth(method = "lm", color = "black", se = TRUE, linewidth = 0.8) +
  # Apply your custom color palette to the color aesthetic
  scale_color_manual(values = color_map) +
  labs(
    title = "S25 biomass C:N by S25 BMN",
    x = expression("Biomass N Uptake (lb ac"^{-1}*")"),
    y = "Cover Crop Shoot C:N Ratio",
    color = "Treatment"
  ) +
  theme_bw(base_family = "serif") +
  theme(
    panel.grid.minor = element_blank(),
    axis.text = element_text(size = 10),
    axis.title = element_text(size = 11),
    legend.title = element_text(face = "bold", size = 10),
    legend.position = "right"
  )








#----S25 biomass C:N by S25 Biomass----

ggplot(data = S25_SoiNXAGBM_dat %>% filter(Site != "Lamberton"), 
       aes(x = CC_AGBM_mgha_S25, y = CN_rat_S25)) +
  # Map point color to Treatment_Full
  geom_point(aes(color = Treatment_Full), size = 2.5, alpha = 0.8) +
  # Keep trendline global across all points
  geom_smooth(method = "lm", color = "black", se = TRUE, linewidth = 0.8) +
  # Apply your custom color palette to the color aesthetic
  scale_color_manual(values = color_map) +
  labs(
    x = expression("CC Biomass (kg ha"^{-1}*")"),
    y = "Cover Crop Shoot C:N Ratio",
    color = "Treatment"
  ) +
  theme_bw(base_family = "serif") +
  theme(
    panel.grid.minor = element_blank(),
    axis.text = element_text(size = 10, face = "bold"),
    axis.title = element_text(size = 11),
    legend.title = element_text(face = "bold", size = 10),
    legend.position = "right"
  )


#----S25 BMN by S25 Biomass----

ggplot(data = S25_SoiNXAGBM_dat %>% filter(Site != "Lamberton"), 
       aes(x = CC_AGBM_kgha_S25, y = BMN_kgha_S25)) +
  # Map point color to Treatment_Full
  geom_point(aes(color = Treatment_Full), size = 2.5, alpha = 0.8) +
  # Keep trendline global across all points
  geom_smooth(method = "lm", color = "black", se = TRUE, linewidth = 0.8) +
  # Apply your custom color palette to the color aesthetic
  scale_color_manual(values = color_map) +
  labs(
    x = expression("Cover Crop Shoot Biomass (kg ha"^{-1}*")"),
    y = expression("CC Biomass N (kg ha"^{-1}*")"),
    color = "Treatment"
  ) +
  theme_bw(base_family = "serif") +
  theme(
    panel.grid.minor = element_blank(),
    axis.text = element_text(size = 10),
    axis.title = element_text(size = 11),
    legend.title = element_text(face = "bold", size = 10),
    legend.position = "right"
  )



#----S25 total soil N by S25 Biomass----

ggplot(data = S25_SoiNXAGBM_dat %>% filter(Site != "Lamberton"), 
       aes(x = CC_AGBM_kgha_S25, y = Total_N_kgha_S25)) +
  # Map point color to Treatment_Full
  geom_point(aes(color = Treatment_Full), size = 2.5, alpha = 0.8) +
  # Keep trendline global across all points
  geom_smooth(method = "lm", color = "black", se = TRUE, linewidth = 0.8) +
  # Apply your custom color palette to the color aesthetic
  scale_color_manual(values = color_map) +
  labs(
    x = expression("Cover Crop Shoot Biomass (kg ha"^{-1}*")"),
    y = expression("Total Soil N (kg ha"^{-1}*")"),
    color = "Treatment"
  ) +
  theme_bw(base_family = "serif") +
  theme(
    panel.grid.minor = element_blank(),
    axis.text = element_text(size = 10),
    axis.title = element_text(size = 11),
    legend.title = element_text(face = "bold", size = 10),
    legend.position = "right"
  )

#----S25 soil total N by S25 BMN----

ggplot(data = S25_SoiNXAGBM_dat %>% filter(Site != "Lamberton"), 
       aes(x = BMN_kgha_S25, y = Total_N_kgha_S25)) +
  # Map point color to Treatment_Full
  geom_point(aes(color = Treatment_Full), size = 2.5, alpha = 0.8) +
  # Keep trendline global across all points
  geom_smooth(method = "lm", color = "black", se = TRUE, linewidth = 0.8) +
  # Apply your custom color palette to the color aesthetic
  scale_color_manual(values = color_map) +
  labs(
    x = expression("Cover Crop Biomass N (kg ha"^{-1}*")"),
    y = expression("Total Soil N (kg ha"^{-1}*")"),
    color = "Treatment"
  ) +
  theme_bw(base_family = "serif") +
  theme(
    panel.grid.minor = element_blank(),
    axis.text = element_text(size = 10),
    axis.title = element_text(size = 11, face = "bold"),
    legend.title = element_text(face = "bold", size = 10),
    legend.position = "right"
  )



#----S25 soil total N by S25 C:N ratio----

ggplot(data = S25_SoiNXAGBM_dat %>% filter(Site != "Lamberton"), 
       aes(x = CN_rat_S25, y = Total_N_kgha_S25)) +
  # Map point color to Treatment_Full
  geom_point(aes(color = Treatment_Full), size = 2.5, alpha = 0.8) +
  # Keep trendline global across all points
  geom_smooth(method = "lm", color = "black", se = TRUE, linewidth = 0.8) +
  # Apply your custom color palette to the color aesthetic
  scale_color_manual(values = color_map) +
  labs(
    x = expression("Cover Crop C:N Ratio"),
    y = expression("Total Soil N (kg ha"^{-1}*")"),
    color = "Treatment"
  ) +
  theme_bw(base_family = "serif") +
  theme(
    panel.grid.minor = element_blank(),
    axis.text = element_text(size = 10),
    axis.title = element_text(size = 11, face = "bold"),
    legend.title = element_text(face = "bold", size = 10),
    legend.position = "right"
  )



#----lysimeter relationships----

#----S25 porewater N by S25 soil total N----

ggplot(data = S25_LysXSoiNXAGBM_dat,aes(x = Total_N_lbac_S25, y = Nitrate_NitriteN_mgL) ) +
  # Map point color to Treatment_Full
  geom_point(aes(color = Treatment_Full), size = 2.5, alpha = 0.8) +
  # Keep trendline global across all points
  geom_smooth(method = "lm", color = "black", se = TRUE, linewidth = 0.8) +
  # Apply your custom color palette to the color aesthetic
  scale_color_manual(values = color_map) +
  labs(
    x = expression("Total Soil N (lb ac"^{-1}*")"),
    y = expression("Soil Porewater N (Mg L"^{-1}*")"),
    color = "Treatment"
  ) +
  theme_bw(base_family = "serif") +
  theme(
    panel.grid.minor = element_blank(),
    axis.text = element_text(size = 10),
    axis.title = element_text(size = 11, face = "bold"),
    legend.title = element_text(face = "bold", size = 10),
    legend.position = "right"
  )


#----S25 porewater N by S25 Biomass C:N ratio----

ggplot(data = S25_LysXSoiNXAGBM_dat, aes(x = CN_rat_S25, y = Nitrate_NitriteN_mgL)) +
  # Map point color to Treatment_Full and shape to Site locally
  geom_point(aes(color = Treatment_Full, shape = Site), size = 2.5, alpha = 0.8) +
  # Single global linear trendline across all sites and treatments
  geom_smooth(method = "lm", color = "black", se = TRUE, linewidth = 0.8) +
  # Apply custom palette for treatment colors
  scale_color_manual(values = color_map) +
  # Explicit solid shapes for high visual contrast across sites
  scale_shape_manual(values = c(16, 17, 15)) + 
  labs(
    title = "Spring 2025 Soil Porewater N by Spring Biomass C:N Ratio",
    x = "Cover Crop C:N Ratio",
    y = expression("Soil Porewater NO"[3]*"-"*N ~ "(mg L"^{-1}*")"),
    color = "Treatment",
    shape = "Site"
  ) +
  theme_bw(base_family = "serif") +
  theme(
    panel.grid.minor = element_blank(),
    axis.text = element_text(size = 10),
    axis.title = element_text(size = 11, face = "bold"),
    legend.title = element_text(face = "bold", size = 10),
    legend.position = "right"
  )




#----S25 porewater N by S25 CC biomass N----

ggplot(data = S25_LysXSoiNXAGBM_dat, aes(x = CC_AGBM_kgha_S25, y = Nitrate_NitriteN_mgL)) +
  # Map point color to Treatment_Full and shape to Site locally
  geom_point(aes(color = Treatment_Full, shape = Site), size = 2.5, alpha = 0.8) +
  # Single global linear trendline across all sites and treatments
  geom_smooth(method = "lm", color = "black", se = TRUE, linewidth = 0.8) +
  # Apply custom palette for treatment colors
  scale_color_manual(values = color_map) +
  # Explicit solid shapes for high visual contrast across sites
  scale_shape_manual(values = c(16, 17, 15)) + 
  labs(
    x = expression("CC Biomass N (kg ha"^{-1}*")"),
    y = expression("Soil Porewater NO"[3]*"-"*N ~ "(mg L"^{-1}*")"),
    color = "Treatment",
    shape = "Site"
  ) +
  theme_bw(base_family = "serif") +
  theme(
    panel.grid.minor = element_blank(),
    axis.text = element_text(size = 10),
    axis.title = element_text(size = 11, face = "bold"),
    legend.title = element_text(face = "bold", size = 10),
    legend.position = "right"
  )




#----S25 porewater N by S25 CC C:N compressed----

S25_LysXSoiNXAGBM_dat %>%
  # 1. Group by plot and metadata factors that remain constant per plot
  group_by(Plot, Site, Treatment_Full,  CN_rat_S25) %>%
  # 2. Compute plot-level mean nitrate concentration
  summarize(
    mean_Nitrate_mgL = mean(Nitrate_NitriteN_mgL, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  # 3. Plot aggregated plot-level means
  ggplot(aes(x = CN_rat_S25, y = mean_Nitrate_mgL)) +
  geom_point(aes(color = Treatment_Full, shape = Site), size = 2.5, alpha = 0.8) +
  geom_smooth(method = "lm", color = "black", se = TRUE, linewidth = 0.8) +
  scale_color_manual(values = color_map) +
  scale_shape_manual(values = c(16, 17, 15)) + 
  labs(
    x = "Cover Crop C:N Ratio",
    y = expression("Mean Soil Porewater NO"[3]*"-"*N ~ "(mg L"^{-1}*")"),
    color = "Treatment",
    shape = "Site"
  ) +
  theme_bw(base_family = "serif") +
  theme(
    panel.grid.minor = element_blank(),
    axis.text = element_text(size = 10),
    axis.title = element_text(size = 11, face = "bold"),
    legend.title = element_text(face = "bold", size = 10),
    legend.position = "right"
  )












SWCS_CNrat_mod <- lm(log(CN_rat) ~ Treatment_Full * Site, data = S25_AGBM_CN_clean)

# ANOVA summary
anova(SWCS_CNrat_mod)

performance::check_homogeneity(SWCS_CNrat_mod)
performance::check_residuals(SWCS_CNrat_mod)
performance::check_normality(SWCS_CNrat_mod)
library("DHARMa")
# Extract Estimated Marginal Means partitioned by Site
CN_emm <- emmeans(SWCS_CNrat_mod, ~ Treatment_Full | Site)

# Generate Compact Letter Display
CN_cld <- cld(
  CN_emm, 
  alpha = 0.05, 
  Letters = letters, 
  adjust = "tukey", 
  decreasing = TRUE
)

# Clean CLD data for plotting
CN_plot_data <- as.data.frame(CN_cld) %>%
  mutate(Letter = trimws(.group))

# Plotting on non-transformed scale
ggplot(CN_plot_data, aes(x = reorder(Treatment_Full, -emmean), y = emmean, fill = Treatment_Full)) +
  geom_bar(stat = "identity", color = "black", alpha = 0.8) +
  geom_errorbar(
    aes(ymin = emmean - SE, ymax = emmean + SE), 
    width = 0.2
  ) +
  geom_text(
    aes(label = Letter, y = emmean + SE), 
    vjust = -0.5, 
    size = 3.8, 
    family = "serif"
  ) +
  facet_wrap(~ Site, scales = "free_y") +
  scale_fill_viridis_d(option = "mako") +
  labs(
    x = "Cover Crop Treatment",
    y = expression(Cover~Crop~Biomass~C:N~Ratio),
    caption = paste0(
      "Bars represent estimated marginal means (emmeans).\n",
      "Error bars represent ±1 SE.\n",
      "Letters indicate significant differences within site (Tukey HSD, alpha = 0.05)."
    )
  ) +
  theme_bw(base_family = "serif") +
  theme(
    legend.position = "none",
    axis.text.x = element_text(angle = 45, hjust = 1, face = "bold", color = "black"),
    axis.text.y = element_text(color = "black"),
    axis.title = element_text(face = "bold"),
    strip.text = element_text(face = "bold", size = 11)
  )








SWCS_CNrat_modF24 <- lm(log(CN_rat) ~ Treatment_Full * Site, data = F24_AGBM_CN)

# ANOVA summary
anova(SWCS_CNrat_modF24)

# Extract Estimated Marginal Means partitioned by Site
F24CN_emm <- emmeans(SWCS_CNrat_modF24, ~ Treatment_Full | Site)

# Generate Compact Letter Display
F24CN_cld <- cld(
  F24CN_emm, 
  alpha = 0.05, 
  Letters = letters, 
  adjust = "tukey", 
  decreasing = TRUE
)

# Clean CLD data for plotting
F24CN_plot_data <- as.data.frame(F24CN_cld) %>%
  mutate(Letter = trimws(.group))

# Plotting on non-transformed scale
ggplot(F24CN_plot_data , aes(x = Treatment_Full, y = emmean, fill = Treatment_Full)) +
  geom_bar(stat = "identity", color = "black", alpha = 0.8) +
  geom_errorbar(
    aes(ymin = emmean - SE, ymax = emmean + SE), 
    width = 0.2
  ) +
  geom_text(
    aes(label = Letter, y = emmean + SE), 
    vjust = -0.5, 
    size = 3.8, 
    family = "serif"
  ) +
  facet_wrap(~ Site, scales = "free_y") +
  scale_fill_viridis_d(option = "mako") +
  labs(
    x = "Cover Crop Treatment",
    y = expression(Cover~Crop~Biomass~C:N~Ratio),
    caption = paste0(
      "Bars represent estimated marginal means (emmeans).\n",
      "Error bars represent ±1 SE.\n",
      "Letters indicate significant differences within site (Tukey HSD, alpha = 0.05)."
    )
  ) +
  theme_bw(base_family = "serif") +
  theme(
    legend.position = "none",
    axis.text.x = element_text(angle = 45, hjust = 1, face = "bold", color = "black"),
    axis.text.y = element_text(color = "black"),
    axis.title = element_text(face = "bold"),
    strip.text = element_text(face = "bold", size = 11)
  )







SWCS_Nuptake_mod <- lmer(log(N_kgha) ~ Treatment_Full * Site + (1|Site:Rep), data = S25_AGBM_CN_clean)

# ANOVA summary
anova(SWCS_Nuptake_mod, type = 3)

# Extract Estimated Marginal Means partitioned by Site
SWCS_Nuptake_emm <- emmeans(SWCS_Nuptake_mod , ~ Treatment_Full | Site)

# Generate Compact Letter Display
SWCS_Nuptake_cld <- cld(
  SWCS_Nuptake_emm, 
  alpha = 0.05, 
  Letters = letters, 
  adjust = "tukey", 
  decreasing = TRUE
)

SWCS_Nuptake_cld




SWCS_F24Nuptake_mod <- lmer(log(N_kgha) ~ Treatment_Full * Site + (1|Site:Rep), data = F24_AGBM_CN)

# ANOVA summary
anova(SWCS_F24Nuptake_mod, type = 3)

# Extract Estimated Marginal Means partitioned by Site
SWCS_F24Nuptake_emm <- emmeans(SWCS_F24Nuptake_mod , ~ Treatment_Full | Site)

# Generate Compact Letter Display
SWCS_F24Nuptake_cld <- cld(
  SWCS_F24Nuptake_emm, 
  alpha = 0.05, 
  Letters = letters, 
  adjust = "tukey", 
  decreasing = TRUE
)

SWCS_F24Nuptake_cld








#----F24 biomass C:N by F24 BMN----
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
    BMN_kgha_F24 = N_kgha,
    BMN_lbac_F24 = N_lbac,
    CN_rat_F24 = CN_rat
  ) %>%
  select(
    Site, Rep, Plot, Trt, Treatment_Full, 
    Date_N_samp_F24, Date_BM_harvest_F24,
    Total_N_kgha_F24, Total_N_lbac_F24,
    CC_AGBM_kgha_F24, CC_AGBM_lbac_F24, BMN_kgha_F24, BMN_lbac_F24, CN_rat_F24
  )

#----F24 CC Biomass N (BMN) by F24 C:N Ratio

ggplot(data = F24_SoiNXAGBM_dat, 
       aes(x = CN_rat_F24, y = BMN_kgha_F24  )) +
  # Map point color to Treatment_Full
  geom_point(aes(color = Treatment_Full), size = 2.5, alpha = 0.8) +
  # Keep trendline global across all points
  geom_smooth(method = "lm", color = "black", se = TRUE, linewidth = 0.8) +
  # Apply your custom color palette to the color aesthetic
  scale_color_manual(values = color_map) +
  labs(
    title = "F24 biomass C:N by F24 BMN",
    x = "Cover Crop Shoot C:N Ratio",
    y = expression("Biomass N Uptake (kg ha"^{-1}*")"),
    color = "Treatment"
  ) +
  theme_bw(base_family = "serif") +
  theme(
    panel.grid.minor = element_blank(),
    axis.text = element_text(size = 10),
    axis.title = element_text(size = 11),
    legend.title = element_text(face = "bold", size = 10),
    legend.position = "right"
  )

view(F24_SoiNXAGBM_dat)
F24_SoiNXAGBM_dat  %>%
  filter(!(Trt %in% c("O1", "O2", "O3", "O4")))%>%

  ggplot( aes(x = CN_rat_F24, y = BMN_kgha_F24  )) +
  # Map point color to Treatment_Full
  geom_point(aes(color = Treatment_Full), size = 2.5, alpha = 0.8) +
  # Keep trendline global across all points
  geom_smooth(method = "lm", color = "black", se = TRUE, linewidth = 0.8) +
  # Apply your custom color palette to the color aesthetic
  scale_color_manual(values = color_map) +
  labs(
    title = "F24 biomass C:N by F24 BMN",
    x = "Cover Crop Shoot C:N Ratio",
    y = expression("Biomass N Uptake (kg ha"^{-1}*")"),
    color = "Treatment"
  ) +
  theme_bw(base_family = "serif") +
  theme(
    panel.grid.minor = element_blank(),
    axis.text = element_text(size = 10),
    axis.title = element_text(size = 11),
    legend.title = element_text(face = "bold", size = 10),
    legend.position = "right"
  )

#----F24 CC C:N Ratio by F24 CC Biomass----

ggplot(data = F24_SoiNXAGBM_dat, 
       aes(x = CC_AGBM_kgha_F24, y = CN_rat_F24  )) +
  # Map point color to Treatment_Full
  geom_point(aes(color = Treatment_Full), size = 2.5, alpha = 0.8) +
  # Keep trendline global across all points
  geom_smooth(method = "lm", color = "black", se = TRUE, linewidth = 0.8) +
  # Apply your custom color palette to the color aesthetic
  scale_color_manual(values = color_map) +
  labs(
    title = "F24 Biomass C:N by F24 CC Biomass",
    x = expression("F24 Cover Crop Biomass (kg ha"^{-1}*")"),
    y = "F24 Biomass C:N Ratio",
    color = "Treatment"
  ) +
  theme_bw(base_family = "serif") +
  theme(
    panel.grid.minor = element_blank(),
    axis.text = element_text(size = 10),
    axis.title = element_text(size = 11),
    legend.title = element_text(face = "bold", size = 10),
    legend.position = "right"
  )



F24_SoiNXAGBM_dat  %>%
  filter(!(Trt %in% c("O1", "O2", "O3", "O4")))%>%
  ggplot( 
         aes(x = CC_AGBM_kgha_F24, y = CN_rat_F24  )) +
  # Map point color to Treatment_Full
  geom_point(aes(color = Treatment_Full), size = 2.5, alpha = 0.8) +
  # Keep trendline global across all points
  geom_smooth(method = "lm", color = "black", se = TRUE, linewidth = 0.8) +
  # Apply your custom color palette to the color aesthetic
  scale_color_manual(values = color_map) +
  labs(
    title = "F24 Biomass C:N by F24 CC Biomass",
    x = expression("F24 Cover Crop Biomass (kg ha"^{-1}*")"),
    y = "F24 Biomass C:N Ratio",
    color = "Treatment"
  ) +
  theme_bw(base_family = "serif") +
  theme(
    panel.grid.minor = element_blank(),
    axis.text = element_text(size = 10),
    axis.title = element_text(size = 11),
    legend.title = element_text(face = "bold", size = 10),
    legend.position = "right"
  )


#----F24 CC Biomass N by F24 CC Biomass----

ggplot(data = F24_SoiNXAGBM_dat, 
       aes(x = CC_AGBM_kgha_F24, y = BMN_kgha_F24  )) +
  # Map point color to Treatment_Full
  geom_point(aes(color = Treatment_Full), size = 2.5, alpha = 0.8) +
  # Keep trendline global across all points
  geom_smooth(method = "lm", color = "black", se = TRUE, linewidth = 0.8) +
  # Apply your custom color palette to the color aesthetic
  scale_color_manual(values = color_map) +
  labs(
    title = "F24 Biomass N Uptake by F24 CC Biomass",
    x = expression("F24 Cover Crop Biomass (kg ha"^{-1}*")"),
    y = expression("F24 CC Biomass N (kg ha"^{-1}*")"),
    color = "Treatment"
  ) +
  theme_bw(base_family = "serif") +
  theme(
    panel.grid.minor = element_blank(),
    axis.text = element_text(size = 10),
    axis.title = element_text(size = 11),
    legend.title = element_text(face = "bold", size = 10),
    legend.position = "right"
  )



F24_SoiNXAGBM_dat  %>%
  filter(!(Trt %in% c("O1", "O2", "O3", "O4")))%>%
  ggplot( 
         aes(x = CC_AGBM_kgha_F24, y = BMN_kgha_F24  )) +
  # Map point color to Treatment_Full
  geom_point(aes(color = Treatment_Full), size = 2.5, alpha = 0.8) +
  # Keep trendline global across all points
  geom_smooth(method = "lm", color = "black", se = TRUE, linewidth = 0.8) +
  # Apply your custom color palette to the color aesthetic
  scale_color_manual(values = color_map) +
  labs(
    title = "F24 Biomass N Uptake by F24 CC Biomass",
    x = expression("F24 Cover Crop Biomass (kg ha"^{-1}*")"),
    y = expression("F24 CC Biomass N (kg ha"^{-1}*")"),
    color = "Treatment"
  ) +
  theme_bw(base_family = "serif") +
  theme(
    panel.grid.minor = element_blank(),
    axis.text = element_text(size = 10),
    axis.title = element_text(size = 11),
    legend.title = element_text(face = "bold", size = 10),
    legend.position = "right"
  )
#----F24 Soil N by F24 CC Biomass----

ggplot(data = F24_SoiNXAGBM_dat, 
       aes(x = CC_AGBM_kgha_F24, y = Total_N_kgha_F24 )) +
  # Map point color to Treatment_Full
  geom_point(aes(color = Treatment_Full), size = 2.5, alpha = 0.8) +
  # Keep trendline global across all points
  geom_smooth(method = "lm", color = "black", se = TRUE, linewidth = 0.8) +
  # Apply your custom color palette to the color aesthetic
  scale_color_manual(values = color_map) +
  labs(
    title = "F24 Soil N by F24 CC Biomass",
    x = expression("F24 Cover Crop Biomass (kg ha"^{-1}*")"),
    y = expression("F24 Soil N (kg ha"^{-1}*")"),
    color = "Treatment"
  ) +
  theme_bw(base_family = "serif") +
  theme(
    panel.grid.minor = element_blank(),
    axis.text = element_text(size = 10),
    axis.title = element_text(size = 11),
    legend.title = element_text(face = "bold", size = 10),
    legend.position = "right"
  )


F24_SoiNXAGBM_dat  %>%
  filter(!(Trt %in% c("O1", "O2", "O3", "O4")))%>%
  ggplot( 
         aes(x = CC_AGBM_kgha_F24, y = Total_N_kgha_F24 )) +
  # Map point color to Treatment_Full
  geom_point(aes(color = Treatment_Full), size = 2.5, alpha = 0.8) +
  # Keep trendline global across all points
  geom_smooth(method = "lm", color = "black", se = TRUE, linewidth = 0.8) +
  # Apply your custom color palette to the color aesthetic
  scale_color_manual(values = color_map) +
  labs(
    title = "F24 Soil N by F24 CC Biomass",
    x = expression("F24 Cover Crop Biomass (kg ha"^{-1}*")"),
    y = expression("F24 Soil N (kg ha"^{-1}*")"),
    color = "Treatment"
  ) +
  theme_bw(base_family = "serif") +
  theme(
    panel.grid.minor = element_blank(),
    axis.text = element_text(size = 10),
    axis.title = element_text(size = 11),
    legend.title = element_text(face = "bold", size = 10),
    legend.position = "right"
  )





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
    CC_N_diff_kgha   = BMN_kgha_S25 - BMN_kgha_F24,
    CC_N_diff_lbac   = BMN_lbac_S25 - BMN_lbac_F24,
    CN_rat_perc_chan = ((CN_rat_S25 - CN_rat_F24) / CN_rat_F24) * 100,
    
    # Over-winter Temporal Interval (Spring minus Fall for soil cores and biomass)
    SoilN_interval_days  = as.numeric(difftime(as.Date(Date_N_samp_S25), as.Date(Date_N_samp_F24), units = "days")),
    BM_harvest_interval_days = as.numeric(difftime(as.Date(Date_BM_harvest_S25), as.Date(Date_BM_harvest_F24), units = "days"))
  )

F24XS25_SoiNXAGBM_diff_dat <- F24XS25_SoiNXAGBM_diff_dat %>%
  filter(!(Site == "Lamberton" & Trt %in% c("C", "O1", "O2", "O3", "O4")))





#----F24 total soil N ~ F24 BMN (rye only)----

ggplot(data = F24_SoiNXAGBM_dat, 
       aes(x = BMN_kgha_F24, y = Total_N_kgha_F24 )) +
  # Map point color to Treatment_Full
  geom_point(aes(color = Treatment_Full), size = 2.5, alpha = 0.8) +
  # Keep trendline global across all points
  geom_smooth(method = "lm", color = "black", se = TRUE, linewidth = 0.8) +
  # Apply your custom color palette to the color aesthetic
  scale_color_manual(values = color_map) +
  labs(
    title = "F24 Total Soil N by F24 BMN",
    x = expression("CC Biomass N (kg ha"^{-1}*")"),
    y = expression("Total Soil N (kg ha"^{-1}*")"),
    color = "Treatment"
  ) +
  theme_bw(base_family = "serif") +
  theme(
    panel.grid.minor = element_blank(),
    axis.text = element_text(size = 10),
    axis.title = element_text(size = 11),
    legend.title = element_text(face = "bold", size = 10),
    legend.position = "right"
  )



F24_SoiNXAGBM_dat  %>%
  filter(!(Trt %in% c("O1", "O2", "O3", "O4")))%>%
  ggplot(
         aes(x = BMN_kgha_F24, y = Total_N_kgha_F24 )) +
  # Map point color to Treatment_Full
  geom_point(aes(color = Treatment_Full), size = 2.5, alpha = 0.8) +
  # Keep trendline global across all points
  geom_smooth(method = "lm", color = "black", se = TRUE, linewidth = 0.8) +
  # Apply your custom color palette to the color aesthetic
  scale_color_manual(values = color_map) +
  labs(
    title = "F24 Total Soil N by F24 BMN",
    x = expression("CC Biomass N (kg ha"^{-1}*")"),
    y = expression("Total Soil N (kg ha"^{-1}*")"),
    color = "Treatment"
  ) +
  theme_bw(base_family = "serif") +
  theme(
    panel.grid.minor = element_blank(),
    axis.text = element_text(size = 10),
    axis.title = element_text(size = 11),
    legend.title = element_text(face = "bold", size = 10),
    legend.position = "right"
  )
#----S25 total Soil N ~ F24 BMN (rye only)----

ggplot(data = F24XS25_SoiNXAGBM_diff_dat, 
       aes(x = BMN_kgha_F24, y = Total_N_kgha_S25 )) +
  # Map point color to Treatment_Full
  geom_point(aes(color = Treatment_Full), size = 2.5, alpha = 0.8) +
  # Keep trendline global across all points
  geom_smooth(method = "lm", color = "black", se = TRUE, linewidth = 0.8) +
  # Apply your custom color palette to the color aesthetic
  scale_color_manual(values = color_map) +
  labs(
    title = "F24 Total Soil N by F24 BMN",
    x = expression("F24 CC Biomass N (kg ha"^{-1}*")"),
    y = expression("S25 Total Soil N (kg ha"^{-1}*")"),
    color = "Treatment"
  ) +
  theme_bw(base_family = "serif") +
  theme(
    panel.grid.minor = element_blank(),
    axis.text = element_text(size = 10),
    axis.title = element_text(size = 11),
    legend.title = element_text(face = "bold", size = 10),
    legend.position = "right"
  )







#----S25 total Soil N ~ F24 BMN (rye only)----

ggplot(data = F24XS25_SoiNXAGBM_diff_dat, 
       aes(x = BMN_kgha_F24, y = Total_N_kgha_S25 )) +
  # Map point color to Treatment_Full
  geom_point(aes(color = Treatment_Full), size = 2.5, alpha = 0.8) +
  # Keep trendline global across all points
  geom_smooth(method = "lm", color = "black", se = TRUE, linewidth = 0.8) +
  # Apply your custom color palette to the color aesthetic
  scale_color_manual(values = color_map) +
  labs(
    title = "S25 Total Soil N by F24 BMN",
    x = expression("F24 CC Biomass N (kg ha"^{-1}*")"),
    y = expression("S25 Total Soil N (kg ha"^{-1}*")"),
    color = "Treatment"
  ) +
  theme_bw(base_family = "serif") +
  theme(
    panel.grid.minor = element_blank(),
    axis.text = element_text(size = 10),
    axis.title = element_text(size = 11),
    legend.title = element_text(face = "bold", size = 10),
    legend.position = "right"
  )



#----S25 CC biomass N ~ F24 Total Soil N (rye only)----

ggplot(data = F24XS25_SoiNXAGBM_diff_dat, 
       aes(x = Total_N_kgha_F24, y =  BMN_kgha_S25  )) +
  # Map point color to Treatment_Full
  geom_point(aes(color = Treatment_Full), size = 2.5, alpha = 0.8) +
  # Keep trendline global across all points
  geom_smooth(method = "lm", color = "black", se = TRUE, linewidth = 0.8) +
  # Apply your custom color palette to the color aesthetic
  scale_color_manual(values = color_map) +
  labs(
    title = "S25 CC Biomass N by F24 Soil N",
    x = expression("F24 Total Soil N (kg ha"^{-1}*")"),
    y = expression("S25 CC Biomass N (kg ha"^{-1}*")"),
    color = "Treatment"
  ) +
  theme_bw(base_family = "serif") +
  theme(
    panel.grid.minor = element_blank(),
    axis.text = element_text(size = 10),
    axis.title = element_text(size = 11),
    legend.title = element_text(face = "bold", size = 10),
    legend.position = "right"
  )





#----S25 CC biomass  ~ F24 Total Soil N (rye only)----

ggplot(data = F24XS25_SoiNXAGBM_diff_dat, 
       aes(x = Total_N_kgha_F24, y =  CC_AGBM_kgha_S25  )) +
  # Map point color to Treatment_Full
  geom_point(aes(color = Treatment_Full), size = 2.5, alpha = 0.8) +
  # Keep trendline global across all points
  geom_smooth(method = "lm", color = "black", se = TRUE, linewidth = 0.8) +
  # Apply your custom color palette to the color aesthetic
  scale_color_manual(values = color_map) +
  labs(
    title = "S25 CC Biomass by F24 Soil N",
    x = expression("F24 Total Soil N (kg ha"^{-1}*")"),
    y = expression("S25 CC Biomass  (kg ha"^{-1}*")"),
    color = "Treatment"
  ) +
  theme_bw(base_family = "serif") +
  theme(
    panel.grid.minor = element_blank(),
    axis.text = element_text(size = 10),
    axis.title = element_text(size = 11),
    legend.title = element_text(face = "bold", size = 10),
    legend.position = "right"
  )





S25_AGBM_CN_novolunteer
view(F24_AGBM_CN)


F24_AGBM_CN  %>%
  filter(!(Trt %in% c("O1", "O2", "O3", "O4")))%>%
  
  ggplot( aes(x = CN_rat, y = C_kgha  )) +
  # Map point color to Treatment_Full
  geom_point(aes(color = Treatment_Full), size = 2.5, alpha = 0.8) +
  # Keep trendline global across all points
  geom_smooth(method = "lm", color = "black", se = TRUE, linewidth = 0.8) +
  # Apply your custom color palette to the color aesthetic
  scale_color_manual(values = color_map) +
  labs(
    title = "F24 biomass C:N by F24 BMC",
    x = "Cover Crop Shoot C:N Ratio",
    y = expression("Biomass C Uptake (kg ha"^{-1}*")"),
    color = "Treatment"
  ) +
  theme_bw(base_family = "serif") +
  theme(
    panel.grid.minor = element_blank(),
    axis.text = element_text(size = 10),
    axis.title = element_text(size = 11),
    legend.title = element_text(face = "bold", size = 10),
    legend.position = "right"
  )



ggplot(data = S25_AGBM_CN_novolunteer,  aes(x = CN_rat, y = C_kgha  )) +
  # Map point color to Treatment_Full
  geom_point(aes(color = Treatment_Full), size = 2.5, alpha = 0.8) +
  # Keep trendline global across all points
  geom_smooth(method = "lm", color = "black", se = TRUE, linewidth = 0.8) +
  # Apply your custom color palette to the color aesthetic
  scale_color_manual(values = color_map) +
  labs(
    title = "S25 biomass C:N by S25 BMC",
    x = "Cover Crop Shoot C:N Ratio",
    y = expression("Biomass C Uptake (kg ha"^{-1}*")"),
    color = "Treatment"
  ) +
  theme_bw(base_family = "serif") +
  theme(
    panel.grid.minor = element_blank(),
    axis.text = element_text(size = 10),
    axis.title = element_text(size = 11),
    legend.title = element_text(face = "bold", size = 10),
    legend.position = "right"
  )




#----Remaining models----


#S25EROXF24AGBM_dat S25erosion  ~ F24CC biomass (1|Site/Rep)


F24BM.S25ER.mod = lmer(ER_gm2day ~ AGBM_mgha + (1|Site:Rep), data = S25EROXF24AGBM_dat)

summary(F24BM.S25ER.mod)
anova(F24BM.S25ER.mod, type = 3)





view(F24_AGBM_CN)
view(S25_AGBM_CN_clean)

#----F24 biomass ~ GDDs * Treatment type----

F24_GDDXAGBM_MOD <- lmer(AGBM_kgha ~ accu_GDD_C * Trt_type + (1 | Site:Rep), data = F24_AGBM_CN)

summary(F24_GDDXAGBM_MOD)
anova(F24_GDDXAGBM_MOD, type = 3)

# 2. Extract Slopes (emtrends) for accu_GDD_C per Site
# Note: Syntax uses formula format `~ Site` instead of pipe `|`
F24_GDDXAGBM_emm <- emtrends(F24_GDDXAGBM_MOD, ~ Trt_type, var = "accu_GDD_C")

# 3. Generate Compact Letter Display comparing slopes across Sites
F24_GDDXAGBM_cld <- cld(
  F24_GDDXAGBM_emm,
  alpha = 0.05,
  Letters = letters,
  adjust = "tukey",
  decreasing = TRUE
) %>%
  mutate(
    .group = trimws(.group),
    `accu_GDD_C.trend` = round(`accu_GDD_C.trend`, 4),
    SE = round(SE, 4)
  )

# Display compact table of site slopes and pairwise comparison letters
F24_GDDXAGBM_cld





#----S25 biomass ~ GDDs ----

S25_GDDXAGBM_MOD <- lmer(AGBM_kgha ~ accu_GDD_C + (1|Site:Rep),
                         data = S25_AGBM_CN_novolunteer)

summary(S25_GDDXAGBM_MOD )
anova(S25_GDDXAGBM_MOD)

# 2. Extract Slopes (emtrends) for accu_GDD_C per Site
# Note: Syntax uses formula format `~ Site` instead of pipe `|`
S25_GDDXAGBM_emm <- emtrends(S25_GDDXAGBM_MOD, var = "accu_GDD_C")
S25_GDDXAGBM_emm
# 3. Generate Compact Letter Display comparing slopes across Sites
S25_GDDXAGBM_cld <- cld(
  S25_GDDXAGBM_emm,
  alpha = 0.05,
  Letters = letters,
  adjust = "tukey",
  decreasing = TRUE
) %>%
  mutate(
    .group = trimws(.group),
    `accu_GDD_C.trend` = round(`accu_GDD_C.trend`, 4),
    SE = round(SE, 4)
  )

# Display compact table of site slopes and pairwise comparison letters
S25_GDDXAGBM_cld



