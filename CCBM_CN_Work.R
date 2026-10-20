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
library("ggpattern")
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

rm(list = c("plot_site_nitrate", ))


#---- Fall 2024 (F24) - Spring 2025 (S25)----

#----READING IN THE DATA----

#----F24 CC AGBM & C/N----

F24_AGBM_CN = read.csv("F24_CCBM_CN.csv")
S25_AGBM_CN = read.csv("S25_CCBM_CN.csv")
F25_AGBM_CN = read.csv("F25_CCBM_CN.csv")
S26_AGBM_CN = read.csv("S26_CCBM_CN.csv")

F24_AGBM_CN = F24_AGBM_CN  %>%
  mutate(
    Date = as.Date(Date, format = "%Y-%m-%d"),
    Site = factor(Site, levels = c("Saint Paul", "Waseca", "Lamberton")),
    Rep = factor(Rep),
    Trt = factor(Trt, levels = c("R1", "R2", "R3", "R4", "O1", "O2", "O3", "O4")),
    Trt_type = factor(Trt_type),
    Season_Full = case_when(
      Season == "F24" ~ "Fall 2024"
    ),
    Treatment_Full = case_when(
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
      "Rye 10", "Rye 20", "Rye 40", "Rye 80", 
      "Oat/Radish 10/1", "Oat/Radish 20/2", "Oat/Radish 40/4", "Oat/Radish 60/6"
    )),
    Trt_type = factor(Trt_type, levels = c("Cereal Rye", "Oat + Radish")))
  


S25_AGBM_CN = S25_AGBM_CN %>% 
  mutate(
    Date = as.Date(Date, format = "%Y-%m-%d"),
    Site = factor(Site, levels = c("Saint Paul", "Waseca", "Lamberton")),
    Rep = factor(Rep),
    Trt = factor(Trt, levels = c("C", "R1", "R2", "R3", "R4", "O1", "O2", "O3", "O4")),
    Trt_type = factor(Trt_type),
    Season_Full = case_when(
      Season == "S25" ~ "Spring 2025"
    ),
    Treatment_Full = case_when(
      Trt == "C" ~ "Fallow",
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
     "Fallow", "Rye 10", "Rye 20", "Rye 40", "Rye 80", 
      "Oat/Radish 10/1", "Oat/Radish 20/2", "Oat/Radish 40/4", "Oat/Radish 60/6"
    )),
    Trt_type = factor(Trt_type, levels = c( "No CC", "Cereal Rye", "Oat + Radish"))
  )



S25_AGBM_CN_novolunteer = S25_AGBM_CN %>%
filter(!(Site == "Lamberton" & Season == "S25" & Trt_type %in% c("No CC", "Oat + Radish")))

F25_AGBM_CN = F25_AGBM_CN %>% 
  mutate(
    Date = as.Date(Date, format = "%Y-%m-%d"),
    Site = factor(Site, levels = c("Saint Paul", "Waseca", "Lamberton")),
    Rep = factor(Rep),
    Trt = factor(Trt, levels = c("C", "R1", "R2", "R3", "R4", "O1", "O2", "O3", "O4")),
    Trt_type = factor(Trt_type),
    Season_Full = case_when(
      Season == "F25" ~ "Fall 2025"
    ),
    Treatment_Full = case_when(
      Trt == "C" ~ "Fallow",
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
      "Fallow", "Rye 10", "Rye 20", "Rye 40", "Rye 80", 
      "Oat/Radish 10/1", "Oat/Radish 20/2", "Oat/Radish 40/4", "Oat/Radish 60/6"
    )),
    Trt_type = factor(Trt_type, levels = c( "No CC", "Cereal Rye", "Oat + Radish"))
  )


S26_AGBM_CN = S26_AGBM_CN %>% 
  mutate(
    Date = as.Date(Date, format = "%Y-%m-%d"),
    Site = factor(Site, levels = c("Saint Paul", "Waseca", "Lamberton")),
    Rep = factor(Rep),
    Trt = factor(Trt, levels = c("C", "R1", "R2", "R3", "R4", "O1", "O2", "O3", "O4")),
    Trt_type = factor(Trt_type),
    Season_Full = case_when(
      Season == "S26" ~ "Spring 2026"
    ),
    Treatment_Full = case_when(
      Trt == "C" ~ "Fallow",
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
      "Fallow", "Rye 10", "Rye 20", "Rye 40", "Rye 80", 
      "Oat/Radish 10/1", "Oat/Radish 20/2", "Oat/Radish 40/4", "Oat/Radish 60/6"
    )),
    Trt_type = factor(Trt_type, levels = c( "No CC", "Cereal Rye", "Oat + Radish"))
  )






S25_AGBM_CN_clean <- S25_AGBM_CN %>%
  filter(!(Site == "Lamberton" & Trt_type %in% c("No CC", "Fallow", "Control", "Oat + Radish"))) %>%
  mutate(across(c(AGBM_kgha, AGBM_lbac, AGBM_mgha), ~ replace_na(.x, 0))) %>%
  droplevels()

S25_AGBM_clean <- S25_AGBM_CN %>%
  filter(!is.na(AGBM_lbac), !is.na(Treatment_Full), !is.na(Site), !is.na(Rep))


Y1_AGBMcomb_dat <- bind_rows(F24_AGBM_CN, S25_AGBM_CN_novolunteer)

Y1_AGBMcomb_dat = Y1_AGBMcomb_dat %>% mutate(
  Season_gen = case_when(
    Season == "F24" ~ "Fall",
    Season == "S25" ~ "Spring",
    TRUE        ~ NA_character_ ),
  Season_gen = factor(Season_gen, levels = c("Fall", "Spring")))



F25_AGBM_CN <- F25_AGBM_CN %>%
  mutate(across(c(C_kgha, C_lbac, N_kgha, AGBM_kgha, AGBM_mgha), as.numeric))

S26_AGBM_CN <- S26_AGBM_CN %>%
  mutate(across(c(C_kgha, C_lbac, N_kgha, AGBM_kgha, AGBM_mgha), as.numeric))

Y2_AGBMcomb_dat <- bind_rows(F25_AGBM_CN, S26_AGBM_CN)



Y2_AGBMcomb_dat = Y2_AGBMcomb_dat %>% mutate(
  Season_gen = case_when(
    Season == "F25" ~ "Fall",
    Season == "S26" ~ "Spring",
    TRUE        ~ NA_character_ ),
  Season_gen = factor(Season_gen, levels = c("Fall", "Spring")))


#----EXPLORING THE DATA----
# ==============================================================================
# 1. Generate Descriptive Statistics Dataframe
# ==============================================================================
F24_CCBM_summary <- F24_AGBM_CN %>%
  group_by(Site,Treatment_Full) %>%
  summarize(
    n    = sum(!is.na(AGBM_kgha)),
    mean = mean(AGBM_kgha, na.rm = TRUE),
    sd   = sd(AGBM_kgha, na.rm = TRUE),
    # Standard Error of the Mean: SE = SD / sqrt(n)
    se   = sd / sqrt(n),
    # Coefficient of Variation: CV% = (SD / Mean) * 100
    cv   = (sd / mean) * 100,
    .groups = "drop"
  )

# ==============================================================================
# 2. Export as Styled Table
# ==============================================================================
F24_CCBM_summary %>%
  mutate(
    # Format metrics for standard scientific display
    mean_se = sprintf("%.2f ± %.2f", mean, se),
    cv_pct  = sprintf("%.1f%%", cv)
  ) %>%
  select(Site, Treatment_Full, n, mean_se, cv_pct) %>%
  kable(
    format = "html",
    caption = "Descriptive statistics for Fall 2024 cover crop shoot biomass (kg/ha) by site 
    and cover crop treatment.",
    col.names = c("Site", "Treatment", "n", "Mean ± SE", "CV (%)"),
    align = "lclcc"
  ) %>%
  kable_styling(
    bootstrap_options = c("striped", "hover", "condensed"),
    full_width = FALSE,
    position = "left"
  ) %>%
  collapse_rows(columns = 1:2, valign = "top")



#----CC AGBM EXPLORATORY BOXPLOT----

ggplot(F24_AGBM_CN, aes(x = Trt, y = AGBM_kgha, fill = Treatment_Full)) +
  geom_boxplot() +
  facet_wrap(~ Site) +
  scale_fill_manual(values = color_map) + 
  theme_bw(base_family = "serif") +
  labs(
    x = "Cover Crop Treatment",
    y = expression(CC~Shoot~Biomass~(kg~ha^-1)),
    fill = "CC Type"
  ) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    strip.text = element_text(face = "bold"))


#----MODEL----

F24_CCBM_MOD = lmer(AGBM_mgha ~ Treatment_Full * Site + (1|Site:Rep),
                              data = F24_AGBM_CN)

#----CHECKING MODEL----
# 1. Set up a 1-row, 2-column plotting grid
par(mfrow = c(1, 2), family = "serif")

# 2. Residuals vs. Fitted (Tests Homoskedasticity / Constant Variance)
plot(fitted(F24_CCBM_MOD), residuals(F24_CCBM_MOD),
     xlab = "Fitted Values", ylab = "Residuals",
     main = "Residuals vs. Fitted")
abline(h = 0, col = "red", lty = 2)

# 3. Normal Q-Q Plot (Tests Normality of Residuals)
qqnorm(residuals(F24_CCBM_MOD), main = "Normal Q-Q Plot")
qqline(residuals(F24_CCBM_MOD), col = "red", lty = 2)

# Reset plotting grid
par(mfrow = c(1, 1))



#----TRANSFORMING THE MODEL----
# 1. Fit the log-transformed model (using the cleaned data)
F24_CCBM_logMOD <- lmer(
  log(AGBM_mgha) ~ Treatment_Full * Site + (1|Site:Rep),
  data = F24_AGBM_CN)

# 2. Run diagnostics on the log-transformed model to verify correction
par(mfrow = c(1, 2), family = "serif")
plot(fitted(F24_CCBM_logMOD), residuals(F24_CCBM_logMOD),
     xlab = "Fitted Values", ylab = "Residuals", main = "Log-Model: Residuals vs. Fitted")
abline(h = 0, col = "red", lty = 2)

qqnorm(residuals(F24_CCBM_logMOD), main = "Log-Model: Q-Q Plot")
qqline(residuals(F24_CCBM_logMOD), col = "red", lty = 2)
par(mfrow = c(1, 1))


#----VARIANCE COMPONENTS----
# Extract and calculate the ICC
vc <- as.data.frame(VarCorr(F24_CCBM_logMOD))
sigma2_block <- vc$vcov[vc$grp == "Site:Rep"]
sigma2_resid <- vc$vcov[vc$grp == "Residual"]

icc_block <- sigma2_block / (sigma2_block + sigma2_resid)
cat("Blocking accounted for", round(icc * 100, 1), "% of the total random variation.\n")

performance::icc(F24_CCBM_logMOD)




#----MODEL SUMMARY AND ANOVA----
# 1. Generate the standard model parameter summary
summary(F24_CCBM_logMOD)

#----EXPONENTIATE FOR TRANSFORMED MODELS

exp(fixef(F24_CCBM_logMOD))


# 2. Run Type III ANOVA using Satterthwaite ddf
F24_CCBM_logANOVA <- anova(F24_CCBM_logMOD, type = 3, ddf = "Satterthwaite")
print(F24_CCBM_logANOVA)



#----FORMATTED ANOVA AND EXPORT----


# ==============================================================================
# 1. Clean, Round, and Format ANOVA Table (with Safe HTML Entities)
# ==============================================================================
F24_CCBM_anova <- F24_CCBM_logANOVA  %>%
  as.data.frame() %>%
  tibble::rownames_to_column("Source") %>%
  # Clean up the Source names
  mutate(
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
F24_CCBM_anova %>%
  kable(
    format = "html",
    caption = "Type III Analysis of Variance with Satterthwaite's method for CC shoot biomass
    (log-transformed lb ac<sup>-1</sup>) in fall 2024. Bold p-values indicate statistical significance (alpha = 0.05). 
    Data was analyzed as a RCBD with the form: lmer(log(Shoot Biomass) ~ Treatment * Site + (1|Site:Rep)).",
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
F24_CCBM_emm_log <- emmeans(
  F24_CCBM_logMOD, 
  ~ Treatment_Full|Site)

F24_CCBM_emm_log %>% 
  summary(infer = TRUE, type = "response")

F24_CCBM_emm_log %>% 
  summary(infer = TRUE)

F24_CCBM_contrasts <- pairs(F24_CCBM_emm_log, adjust = "tukey")
print(F24_CCBM_contrasts)

# Perform Tukey comparisons and get CLD letters on the log scale (where assumptions hold)
F24_CCBM_cld <- cld(
  F24_CCBM_emm_log, 
  alpha = 0.05, 
  Letters = letters, 
  adjust = "tukey", 
  decreasing = TRUE)
F24_CCBM_cld
# Convert the grid to the response scale using regrid() for symmetric SEs
# Then merge the letters back based on all indexing factors
F24_CCBM_plot_data <- regrid(F24_CCBM_emm_log, transform = "response") %>%
  as.data.frame() %>%
  left_join(
    as.data.frame(F24_CCBM_cld ) %>% 
      select(Treatment_Full, Site, .group), 
    by = c("Treatment_Full", "Site")
  ) %>%
  mutate(Letter = trimws(.group))



F24_CCBM_plot_data


#----WEIRD NEW PIPELINE FOR SITE-SPECIFIC GRAPHS----



# ==============================================================================
# 1. Simplified Data Wrangling (No complex indexing or middle-bar math)
# ==============================================================================
F24_CCBM_plot_data <- F24_CCBM_plot_data %>%
  group_by(Site) %>%
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
F24_plot_site_CCBM <- function(site_name) {
  F24_CCBM_plot_data %>%
    filter(Site == site_name) %>%
    
    ggplot(aes(x = Treatment_Full, y = response, fill = Treatment_Full)) +
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
      fontface = "italic"
    ) +
    
    # Aesthetics & Theme
    scale_fill_manual(values = color_map) +
    scale_y_continuous(expand = expansion(mult = c(0.01, 0.22))) +
    labs(
      title = paste("Fall 2024 Cover Crop Shoot Biomass:", site_name),
      y = expression("Estimated Mean CC Biomass (Mg ha"^{-1}*")"),
      x = "Treatment",
      caption = paste0(
        "Bars represent back-transformed emmeans.\n",
        "Error bars represent Delta-method approximated ±1 SE.\n",
        "Letters indicate significant differences within each site (Tukey HSD, alpha = 0.05)."
      ),
      fill = "Cover Crop Treatment"
    ) +
    theme_bw(base_family = "serif") +
    theme(
      legend.position = "none", 
      axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
      strip.text = element_text(face = "bold", size = 10)
    )
}

# ==============================================================================
# 3. Regenerate the Standalone Objects
# ==============================================================================
F24_CCBM_plot_StPaul    <- F24_plot_site_CCBM("Saint Paul")
F24_CCBM_plot_Waseca    <- F24_plot_site_CCBM("Waseca")
F24_CCBM_plot_Lamberton <- F24_plot_site_CCBM("Lamberton")

# View a plot in RStudio
F24_CCBM_plot_StPaul
F24_CCBM_plot_Waseca  
F24_CCBM_plot_Lamberton











#----S25  SHOOT BIOMASS----

S25_AGBM_CN_novolunteer




#----EXPLORING THE DATA----
# ==============================================================================
# 1. Generate Descriptive Statistics Dataframe
# ==============================================================================
S25_CCBM_summary <-S25_AGBM_CN_novolunteer%>%
  group_by(Site,Treatment_Full) %>%
  summarize(
    n    = sum(!is.na(AGBM_kgha)),
    mean = mean(AGBM_kgha, na.rm = TRUE),
    sd   = sd(AGBM_kgha, na.rm = TRUE),
    # Standard Error of the Mean: SE = SD / sqrt(n)
    se   = sd / sqrt(n),
    # Coefficient of Variation: CV% = (SD / Mean) * 100
    cv   = (sd / mean) * 100,
    .groups = "drop"
  )

# ==============================================================================
# 2. Export as Styled Table
# ==============================================================================
S25_CCBM_summary %>%
  mutate(
    # Format metrics for standard scientific display
    mean_se = sprintf("%.2f ± %.2f", mean, se),
    cv_pct  = sprintf("%.1f%%", cv)
  ) %>%
  select(Site, Treatment_Full, n, mean_se, cv_pct) %>%
  kable(
    format = "html",
    caption = "Descriptive statistics for Srping 2025 cover crop shoot biomass (kg/ha) by site 
    and cover crop treatment.",
    col.names = c("Site", "Treatment", "n", "Mean ± SE", "CV (%)"),
    align = "lclcc"
  ) %>%
  kable_styling(
    bootstrap_options = c("striped", "hover", "condensed"),
    full_width = FALSE,
    position = "left"
  ) %>%
  collapse_rows(columns = 1:2, valign = "top")



#----CC AGBM EXPLORATORY BOXPLOT----

ggplot(S25_AGBM_CN_novolunteer, aes(x = Trt, y = AGBM_mgha, fill = Treatment_Full)) +
  geom_boxplot() +
  facet_wrap(~ Site) +
  scale_fill_manual(values = color_map) + 
  theme_bw(base_family = "serif") +
  labs(
    x = "Cover Crop Treatment",
    y = expression(CC~Shoot~Biomass~(kg~ha^-1)),
    fill = "CC Treatment"
  ) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    strip.text = element_text(face = "bold"))


#----MODEL----

S25_CCBM_MOD = lm(AGBM_mgha ~ Treatment_Full * Site,
                    data = S25_AGBM_CN_novolunteer)

#----CHECKING MODEL----
# 1. Set up a 1-row, 2-column plotting grid
par(mfrow = c(1, 2), family = "serif")

# 2. Residuals vs. Fitted (Tests Homoskedasticity / Constant Variance)
plot(fitted(S25_CCBM_MOD), residuals(S25_CCBM_MOD),
     xlab = "Fitted Values", ylab = "Residuals",
     main = "Residuals vs. Fitted")
abline(h = 0, col = "red", lty = 2)

# 3. Normal Q-Q Plot (Tests Normality of Residuals)
qqnorm(residuals(S25_CCBM_MOD), main = "Normal Q-Q Plot")
qqline(residuals(S25_CCBM_MOD), col = "red", lty = 2)

# Reset plotting grid
par(mfrow = c(1, 1))



S25_CCBM_logMOD <- lmer(
  log(AGBM_mgha) ~ Treatment_Full * Site + (1|Site:Rep),
  data = S25_AGBM_CN_novolunteer)

# 2. Run diagnostics on the log-transformed model to verify correction
par(mfrow = c(1, 2), family = "serif")
plot(fitted(S25_CCBM_logMOD), residuals(S25_CCBM_logMOD),
     xlab = "Fitted Values", ylab = "Residuals", main = "Log-Model: Residuals vs. Fitted")
abline(h = 0, col = "red", lty = 2)

qqnorm(residuals(S25_CCBM_logMOD), main = "Log-Model: Q-Q Plot")
qqline(residuals(S25_CCBM_logMOD), col = "red", lty = 2)
par(mfrow = c(1, 1))


#----MODEL SUMMARY AND ANOVA----
# 1. Generate the standard model parameter summary
summary(S25_CCBM_MOD)


# 2. Get Type III ANOVA table (standard classical F-tests for lm)
S25_CCBM_logANOVA <- anova(S25_CCBM_logMOD, type = 3, ddf = "Satterthwaite")
print(S25_CCBM_logANOVA)
# ==============================================================================
# FORMATTED ANOVA AND EXPORT
# ==============================================================================

# Clean, Round, and Format ANOVA Table
S25_CCBM_anova_table <- S25_CCBM_ANOVA %>%
  as.data.frame() %>%
  tibble::rownames_to_column("Source") %>%
  # Remove Intercept row if present and clean term names
  filter(Source != "(Intercept)") %>%
  mutate(
    Source = gsub("Treatment_Full|Trt", "Treatment", Source),
    Source = gsub(":", " × ", Source)
  ) %>%
  # Assign standard Residual DF to terms for table consistency
  mutate(
    NumDF = Df,
    DenDF = S25_CCBM_MOD$df.residual,
    `Sum Sq`  = round(`Sum Sq`, 2),
    `Mean Sq` = round(`Sum Sq` / NumDF, 2),
    DenDF     = round(DenDF, 0),
    `F value` = round(`F value`, 2)
  ) %>%
  # Format p-values using HTML-safe entities
  mutate(
    `p-value` = case_when(
      is.na(`Pr(>F)`)   ~ "NS",
      `Pr(>F)` < 0.001  ~ "<b>&lt; 0.001</b>",
      `Pr(>F)` < 0.05   ~ sprintf("<b>%.4f</b>", `Pr(>F)`),
      TRUE              ~ sprintf("%.4f", `Pr(>F)`)
    )
  ) %>%
  # Keep only display-ready columns
  select(Source, `Sum Sq`, `Mean Sq`, NumDF, DenDF, `F value`, `p-value`)

# Export as High-Contrast HTML Table
S25_CCBM_anova_table %>%
  kable(
    format = "html",
    caption = "Type III Analysis of Variance for CC shoot biomass (kg ha<sup>-1</sup>) in spring 2025. Bold p-values indicate statistical significance (alpha = 0.05). Data was analyzed as a linear model: lm(AGBM_kgha ~ Treatment * Site).",
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


#----EXTRACTING EMMEANS AND CLD, BACK-TRANSFORMING----

# ==============================================================================
# 1. Back-Transform using regrid() for Symmetric Standard Errors (response ± SE)
# ==============================================================================
# First, estimate marginal means on the log scale
S25_CCBM_emm <- emmeans(
  S25_CCBM_MOD, 
  ~ Treatment_Full|Site)

S25_CCBM_emm  %>% 
  summary(infer = TRUE)

S25_CCBM_contrasts <- pairs(S25_CCBM_emm, adjust = "tukey")
print(S25_CCBM_contrasts)

# Perform Tukey comparisons and get CLD letters on the log scale (where assumptions hold)
S25_CCBM_cld <- cld(
  S25_CCBM_emm, 
  alpha = 0.05, 
  Letters = letters, 
  adjust = "tukey", 
  decreasing = TRUE)

S25_CCBM_plot_data <- as.data.frame(S25_CCBM_emm) %>%
  left_join(as.data.frame(S25_CCBM_cld) %>% select(Treatment_Full, Site, .group),
            by = c("Treatment_Full", "Site")) %>%
  mutate(Letter = trimws(.group))


S25_CCBM_plot_data
#----WEIRD NEW PIPELINE FOR SITE-SPECIFIC GRAPHS----

#----Sized up ns labels and clculated midpoint between treatment bars because the ns was just above first emmean----

S25_CCBM_plot_data <- S25_CCBM_plot_data %>%
  group_by(Site) %>%
  mutate(
    # Check if any treatment separation exists in this site
    Is_Sig = n_distinct(Letter) > 1,
    
    # Layer 1: Bar letters (blanked out if non-significant)
    Bar_Letter = if_else(Is_Sig, Letter, ""),
    
    # Peak Y coordinate for floating annotations above tallest SE bar
    Cluster_Max_Y = max(emmean + SE, na.rm = TRUE),
    
    # Calculate numeric mid-point offset (e.g., 2 + 0.5 = 2.5 for 4 treatments)
    # This works dynamically for any even or odd number of treatments
    X_Midpoint = (n_distinct(Treatment_Full) + 1) / 2,
    
    # Layer 2: Single "n.s." label placed only on row_number() == 1
    Cluster_NS = if_else(!Is_Sig & row_number() == 1, "n.s.", "")
  ) %>%
  ungroup()


# ==============================================================================
# 2. Simplified Plotting Function
# ==============================================================================
S25_plot_site_CCBM <- function(site_name) {
  
  site_data <- S25_CCBM_plot_data %>% filter(Site == site_name)
  
  ggplot(site_data, aes(x = Treatment_Full, y = emmean, fill = Treatment_Full)) +
    # Clustered Bars and Error Bars
    geom_bar(stat = "identity", position = position_dodge(width = 0.8), color = "black", alpha = 0.7, width = 0.7) +
    geom_errorbar(
      aes(ymin = pmax(0, emmean - SE), ymax = emmean + SE),
      position = position_dodge(width = 0.8),
      width = 0.2
    ) +
    
    # TEXT LAYER 1: Individual Tukey letters (placed if site has significant differences)
    geom_text(
      aes(label = Bar_Letter, y = emmean + SE),
      position = position_dodge(width = 0.8),
      vjust = -0.5,
      size = 3.2,
      fontface = "bold"
    ) +
    
    # TEXT LAYER 2: Single "n.s." label centered over x = X_Midpoint (e.g., 2.5)
    geom_text(
      data = ~ filter(.x, Cluster_NS == "n.s."),
      aes(x = X_Midpoint, y = Cluster_Max_Y, label = Cluster_NS),
      vjust = -0.8,
      size = 5.5,
      fontface = "bold",
      inherit.aes = FALSE
    ) +
    
    # Aesthetics & Theme
    scale_fill_manual(values = color_map) +
    scale_y_continuous(expand = expansion(mult = c(0.01, 0.22))) +
    labs(
      title = paste("Spring 2025 Cover Crop Shoot Biomass:", site_name),
      y = expression("Estimated Mean CC Biomass (kg ha"^{-1}*")"),
      x = "Treatment",
      caption = paste0(
        "Bars represent estimated marginal mean CC shoot biomass.\n",
        "Error bars represent ±1 SE.\n",
        "Letters indicate significant differences within each site (Tukey HSD, alpha = 0.05)."
      ),
      fill = "Cover Crop Treatment"
    ) +
    theme_bw(base_family = "serif") +
    theme(
      legend.position = "none", 
      axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
      strip.text = element_text(face = "bold", size = 10)
    )
}


# ==============================================================================
# 3. Regenerate Standalone Objects (Unchanged)
# ==============================================================================
S25_CCBM_plot_StPaul    <- S25_plot_site_CCBM("Saint Paul")
S25_CCBM_plot_Waseca    <- S25_plot_site_CCBM("Waseca")
S25_CCBM_plot_Lamberton <- S25_plot_site_CCBM("Lamberton")

# View plots
S25_CCBM_plot_StPaul
S25_CCBM_plot_Waseca  
S25_CCBM_plot_Lamberton




#---- Y1 Biomass Combined Fall and spring biomass emmeans and plot----

# ==============================================================================
# 1. FALL 2024 EMMEANS, CLD (CAPITAL LETTERS) & REGRID
# ==============================================================================
F24_emm_log <- emmeans(F24_CCBM_logMOD, ~ Treatment_Full | Site)

F24_cld <- cld(
  F24_emm_log,
  alpha = 0.05,
  Letters = letters,  # Capital letters for Fall
  adjust = "tukey",
  decreasing = TRUE
)

F24_plot_data <- regrid(F24_emm_log, transform = "response") %>%
  as.data.frame() %>%
  left_join(
    as.data.frame(F24_cld) %>% select(Treatment_Full, Site, .group),
    by = c("Treatment_Full", "Site")
  ) %>%
  mutate(
    Letter = trimws(.group),
    Season_gen = "Fall"
  )
F24_plot_data
# ==============================================================================
# 2. SPRING 2025 EMMEANS, CLD (LOWERCASE LETTERS) & REGRID
# ==============================================================================
S25_emm_log <- emmeans(S25_CCBM_logMOD, ~ Treatment_Full | Site)

S25_cld <- cld(
  S25_emm_log,
  alpha = 0.05,
  Letters = LETTERS,  # Lowercase letters for Spring
  adjust = "tukey",
  decreasing = TRUE
)

S25_plot_data <- regrid(S25_emm_log, transform = "response") %>%
  as.data.frame() %>%
  left_join(
    as.data.frame(S25_cld) %>% select(Treatment_Full, Site, .group),
    by = c("Treatment_Full", "Site")
  ) %>%
  mutate(
    Letter = trimws(.group),
    Season_gen = "Spring"
  )
S25_plot_data
# ==============================================================================
# 3. MERGE DATASETS & WRANGLE N.S. WRAPPERS
# ==============================================================================


# ==============================================================================
# 1. COMBINED BACK-TRANSFORMED DATASET
# ==============================================================================
Y1_CCBM_plot_dat <- bind_rows(F24_plot_data, S25_plot_data) %>%
  mutate(
    Season_gen = factor(Season_gen, levels = c("Fall", "Spring"))
  )

# ==============================================================================
# 2. SITE PLOTTING FUNCTION
# ==============================================================================
Y1_plot_site_textured_CCBM <- function(site_name, data = Y1_CCBM_plot_dat) {
  
  data %>%
    filter(Site == site_name) %>%
    ggplot(aes(x = Treatment_Full, y = response, fill = Treatment_Full, pattern = Season_gen)) +
    # Clustered side-by-side textured bars
    geom_bar_pattern(
      stat = "identity",
      position = position_dodge(width = 0.8),
      width = 0.7,
      color = "black",
      alpha = 0.85,
      pattern_color = "black",
      pattern_fill = "black",
      pattern_angle = 45,
      pattern_density = 0.1,
      pattern_spacing = 0.025
    ) +
    # Error bars
    geom_errorbar(
      aes(ymin = pmax(0, response - SE), ymax = response + SE),
      position = position_dodge(width = 0.8),
      width = 0.25,
      linewidth = 0.5
    ) +
    # Tukey letters (Capital for Fall, Lowercase for Spring)
    geom_text(
      aes(label = Letter, y = response + SE),
      position = position_dodge(width = 0.8),
      vjust = -0.5,
      size = 3.8,
      fontface = "bold"
    ) +
    # Color Map & Pattern Mapping
    scale_fill_manual(values = color_map, name = "Cover Crop Treatment") +
    scale_pattern_manual(
      values = c("Fall" = "stripe", "Spring" = "none"),
      name = "Season"
    ) +
    # Legend Formatting: Wrap Treatment legend into 2 rows to fit width
    guides(
      fill = guide_legend(
        nrow = 2, 
        byrow = TRUE, 
        override.aes = list(pattern = "none")
      ),
      pattern = guide_legend(
        nrow = 1, 
        override.aes = list(fill = "white")
      )
    ) +
    scale_y_continuous(expand = expansion(mult = c(0, 0.18))) +
    labs(
      x = "Cover Crop Treatment",
      y = expression("Estimated Mean CC Biomass (Mg ha"^-1*")")
    ) +
    theme_bw(base_family = "serif") +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1, color = "black", size = 10),
      axis.text.y = element_text(color = "black", size = 10),
      axis.title = element_text(face = "bold"),
      legend.position = "bottom",
      legend.box = "vertical",       # Stacks Treatment and Season legends vertically
      legend.spacing.y = unit(0.2, "cm"),
      legend.text = element_text(size = 9),
      legend.title = element_text(face = "bold", size = 10),
      plot.title = element_text(face = "bold", size = 12)
    )
}

# ==============================================================================
# 3. GENERATE INDIVIDUAL SITE PLOTS
# ==============================================================================
Y1_CCBM_plot_StPaul    <- Y1_plot_site_textured_CCBM ("Saint Paul")
Y1_CCBM_plot_Waseca    <- Y1_plot_site_textured_CCBM ("Waseca")
Y1_CCBM_plot_Lamberton <- Y1_plot_site_textured_CCBM ("Lamberton")

# Display in RStudio
Y1_CCBM_plot_StPaul
Y1_CCBM_plot_Waseca
Y1_CCBM_plot_Lamberton





# ==============================================================================
# 1. SITE PLOTTING FUNCTION
# ==============================================================================
Y1_plot_site_textured_CCBM <- function(site_name, data = Y1_CCBM_plot_dat) {
  
  data %>%
    filter(Site == site_name) %>%
    ggplot(aes(x = Treatment_Full, y = response, fill = Treatment_Full, pattern = Season_gen)) +
    # Clustered side-by-side textured bars
    geom_bar_pattern(
      stat = "identity",
      position = position_dodge(width = 0.8),
      width = 0.7,
      color = "black",
      alpha = 0.85,
      pattern_color = "black",
      pattern_fill = "black",
      pattern_angle = 45,
      pattern_density = 0.1,
      pattern_spacing = 0.025
    ) +
    # Error bars
    geom_errorbar(
      aes(ymin = pmax(0, response - SE), ymax = response + SE),
      position = position_dodge(width = 0.8),
      width = 0.25,
      linewidth = 0.5
    ) +
    # Tukey letters (Capital for Fall, Lowercase for Spring)
    geom_text(
      aes(label = Letter, y = response + SE),
      position = position_dodge(width = 0.8),
      vjust = -0.5,
      size = 3.8,
      fontface = "bold"
    ) +
    # Color Map & Pattern Mapping
    scale_fill_manual(values = color_map, name = "Cover Crop Treatment") +
    scale_pattern_manual(
      values = c("Fall" = "stripe", "Spring" = "none"),
      name = "Season"
    ) +
    # Legend Formatting
    guides(
      fill = guide_legend(
        nrow = 2, 
        byrow = TRUE, 
        override.aes = list(pattern = "none")
      ),
      pattern = guide_legend(
        nrow = 1, 
        override.aes = list(fill = "white")
      )
    ) +
    scale_y_continuous(expand = expansion(mult = c(0, 0.18))) +
    labs(
      subtitle = paste(site_name),
      x = NULL,
      y = expression(bold("Estimated Mean CC Biomass (Mg ha")^bold("-1")*bold(")"))
    ) +
    theme_bw(base_family = "serif") +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1, color = "black", size = 12, face = "bold"),
      axis.text.y = element_text(color = "black", size = 12),
      axis.title.x = element_blank(),
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
# 2. GENERATE SUBPLOTS & ASSEMBLE PATCHWORK
# ==============================================================================
Y1_CCBM_plot_StPaul    <- Y1_plot_site_textured_CCBM("Saint Paul")
Y1_CCBM_plot_Waseca    <- Y1_plot_site_textured_CCBM("Waseca")
Y1_CCBM_plot_Lamberton <- Y1_plot_site_textured_CCBM("Lamberton")

# Suppress x-axis text/ticks on top two subplots (St. Paul & Waseca)
Y1_CCBM_plot_StPaul <- Y1_CCBM_plot_StPaul + 
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())

Y1_CCBM_plot_Waseca <- Y1_CCBM_plot_Waseca + 
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())

# Stack plots into patchwork
Y1_CCBM_composite_figure <- (Y1_CCBM_plot_StPaul / Y1_CCBM_plot_Waseca / Y1_CCBM_plot_Lamberton) +
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
  filename = "Y1_Composite_CCBM_By_Site.png",
  plot = Y1_CCBM_composite_figure,
  width = 8.5,
  height = 12,
  dpi = 300,
  device = "png"
)




















#----Y2 CCBM combined plot----


#----F25 model and anova----

F25_CCBM_MOD = lmer(AGBM_mgha ~ Treatment_Full * Site + (1|Site:Rep),
                  data = F25_AGBM_CN)

#----CHECKING MODEL----
# 1. Set up a 1-row, 2-column plotting grid
par(mfrow = c(1, 2), family = "serif")

# 2. Residuals vs. Fitted (Tests Homoskedasticity / Constant Variance)
plot(fitted(F25_CCBM_MOD), residuals(F25_CCBM_MOD),
     xlab = "Fitted Values", ylab = "Residuals",
     main = "Residuals vs. Fitted")
abline(h = 0, col = "red", lty = 2)

# 3. Normal Q-Q Plot (Tests Normality of Residuals)
qqnorm(residuals(F25_CCBM_MOD), main = "Normal Q-Q Plot")
qqline(residuals(F25_CCBM_MOD), col = "red", lty = 2)

# Reset plotting grid
par(mfrow = c(1, 1))



F25_CCBM_logMOD <- lmer(
  log(AGBM_mgha) ~ Treatment_Full * Site + (1|Site:Rep),
  data = F25_AGBM_CN)

# 2. Run diagnostics on the log-transformed model to verify correction
par(mfrow = c(1, 2), family = "serif")
plot(fitted(F25_CCBM_logMOD), residuals(F25_CCBM_logMOD),
     xlab = "Fitted Values", ylab = "Residuals", main = "Log-Model: Residuals vs. Fitted")
abline(h = 0, col = "red", lty = 2)

qqnorm(residuals(F25_CCBM_logMOD), main = "Log-Model: Q-Q Plot")
qqline(residuals(F25_CCBM_logMOD), col = "red", lty = 2)
par(mfrow = c(1, 1))


#----MODEL SUMMARY AND ANOVA----
# 1. Generate the standard model parameter summary
summary(F25_CCBM_MOD)


# 2. Get Type III ANOVA table (standard classical F-tests for lm)
F25_CCBM_logANOVA <- anova(F25_CCBM_logMOD, type = 3, ddf = "Satterthwaite")
print(F25_CCBM_logANOVA)




#----S26 model and anova----

S26_CCBM_MOD = lmer(AGBM_mgha ~ Treatment_Full * Site + (1|Site:Rep),
                  data = S26_AGBM_CN)

#----CHECKING MODEL----
# 1. Set up a 1-row, 2-column plotting grid
par(mfrow = c(1, 2), family = "serif")

# 2. Residuals vs. Fitted (Tests Homoskedasticity / Constant Variance)
plot(fitted(S26_CCBM_MOD), residuals(S26_CCBM_MOD),
     xlab = "Fitted Values", ylab = "Residuals",
     main = "Residuals vs. Fitted")
abline(h = 0, col = "red", lty = 2)

# 3. Normal Q-Q Plot (Tests Normality of Residuals)
qqnorm(residuals(S26_CCBM_MOD), main = "Normal Q-Q Plot")
qqline(residuals(S26_CCBM_MOD), col = "red", lty = 2)

# Reset plotting grid
par(mfrow = c(1, 1))



S26_CCBM_logMOD <- lmer(
  log(AGBM_mgha) ~ Treatment_Full * Site + (1|Site:Rep),
  data = S26_AGBM_CN)

# 2. Run diagnostics on the log-transformed model to verify correction
par(mfrow = c(1, 2), family = "serif")
plot(fitted(S26_CCBM_logMOD), residuals(S26_CCBM_logMOD),
     xlab = "Fitted Values", ylab = "Residuals", main = "Log-Model: Residuals vs. Fitted")
abline(h = 0, col = "red", lty = 2)

qqnorm(residuals(S26_CCBM_logMOD), main = "Log-Model: Q-Q Plot")
qqline(residuals(S26_CCBM_logMOD), col = "red", lty = 2)
par(mfrow = c(1, 1))


#----MODEL SUMMARY AND ANOVA----
# 1. Generate the standard model parameter summary
summary(S26_CCBM_MOD)


# 2. Get Type III ANOVA table (standard classical F-tests for lm)
S26_CCBM_logANOVA <- anova(S26_CCBM_logMOD, type = 3, ddf = "Satterthwaite")
print(S26_CCBM_logANOVA)




#----Y2 Biomass Combined Fall and spring biomass emmeans and plot----

# ==============================================================================
# 1. FALL 2024 EMMEANS, CLD (CAPITAL LETTERS) & REGRID
# ==============================================================================
F25_emm_log <- emmeans(F25_CCBM_logMOD, ~ Treatment_Full | Site)

F25_cld <- cld(
  F25_emm_log,
  alpha = 0.05,
  Letters = letters,  # Capital letters for Fall
  adjust = "tukey",
  decreasing = TRUE
)

F25_plot_data <- regrid(F25_emm_log, transform = "response") %>%
  as.data.frame() %>%
  left_join(
    as.data.frame(F25_cld) %>% select(Treatment_Full, Site, .group),
    by = c("Treatment_Full", "Site")
  ) %>%
  mutate(
    Letter = trimws(.group),
    Season_gen = "Fall"
  )
F25_plot_data
# ==============================================================================
# 2. SPRING 2025 EMMEANS, CLD (LOWERCASE LETTERS) & REGRID
# ==============================================================================
S26_emm_log <- emmeans(S26_CCBM_logMOD, ~ Treatment_Full | Site)

S26_cld <- cld(
  S26_emm_log,
  alpha = 0.05,
  Letters = LETTERS,  # Lowercase letters for Spring
  adjust = "tukey",
  decreasing = TRUE
)

S26_plot_data <- regrid(S26_emm_log, transform = "response") %>%
  as.data.frame() %>%
  left_join(
    as.data.frame(S26_cld) %>% select(Treatment_Full, Site, .group),
    by = c("Treatment_Full", "Site")
  ) %>%
  mutate(
    Letter = trimws(.group),
    Season_gen = "Spring"
  )
S26_plot_data
# ==============================================================================
# 3. MERGE DATASETS & WRANGLE N.S. WRAPPERS
# ==============================================================================


# ==============================================================================
# 1. COMBINED BACK-TRANSFORMED DATASET
# ==============================================================================
Y2_CCBM_plot_dat <- bind_rows(F25_plot_data, S26_plot_data) %>%
  mutate(
    Season_gen = factor(Season_gen, levels = c("Fall", "Spring"))
  )


# ==============================================================================
# 1. SITE PLOTTING FUNCTION
# ==============================================================================
Y2_plot_site_textured_CCBM <- function(site_name, data = Y2_CCBM_plot_dat) {
  
  data %>%
    filter(Site == site_name) %>%
    ggplot(aes(x = Treatment_Full, y = response, fill = Treatment_Full, pattern = Season_gen)) +
    # Clustered side-by-side textured bars
    geom_bar_pattern(
      stat = "identity",
      position = position_dodge(width = 0.8),
      width = 0.7,
      color = "black",
      alpha = 0.85,
      pattern_color = "black",
      pattern_fill = "black",
      pattern_angle = 45,
      pattern_density = 0.1,
      pattern_spacing = 0.025
    ) +
    # Error bars
    geom_errorbar(
      aes(ymin = pmax(0, response - SE), ymax = response + SE),
      position = position_dodge(width = 0.8),
      width = 0.25,
      linewidth = 0.5
    ) +
    # Tukey letters (Capital for Fall, Lowercase for Spring)
    geom_text(
      aes(label = Letter, y = response + SE),
      position = position_dodge(width = 0.8),
      vjust = -0.5,
      size = 3.8,
      fontface = "bold"
    ) +
    # Color Map & Pattern Mapping
    scale_fill_manual(values = color_map, name = "Cover Crop Treatment") +
    scale_pattern_manual(
      values = c("Fall" = "stripe", "Spring" = "none"),
      name = "Season"
    ) +
    # Legend Formatting
    guides(
      fill = guide_legend(
        nrow = 2, 
        byrow = TRUE, 
        override.aes = list(pattern = "none")
      ),
      pattern = guide_legend(
        nrow = 1, 
        override.aes = list(fill = "white")
      )
    ) +
    scale_y_continuous(expand = expansion(mult = c(0, 0.18))) +
    labs(
      subtitle = paste(site_name),
      x = NULL,
      y = expression(bold("Estimated Mean CC Biomass (Mg ha")^bold("-1")*bold(")"))
    ) +
    theme_bw(base_family = "serif") +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1, color = "black", size = 12, face = "bold"),
      axis.text.y = element_text(color = "black", size = 12),
      axis.title.x = element_blank(),
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
# 2. GENERATE SUBPLOTS & ASSEMBLE PATCHWORK
# ==============================================================================
Y2_CCBM_plot_StPaul    <- Y2_plot_site_textured_CCBM("Saint Paul")
Y2_CCBM_plot_Waseca    <- Y2_plot_site_textured_CCBM("Waseca")
Y2_CCBM_plot_Lamberton <- Y2_plot_site_textured_CCBM("Lamberton")

# Suppress x-axis text/ticks on top two subplots (St. Paul & Waseca)
Y2_CCBM_plot_StPaul <- Y2_CCBM_plot_StPaul + 
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())

Y2_CCBM_plot_Waseca <- Y2_CCBM_plot_Waseca + 
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())

# Stack plots into patchwork
Y2_CCBM_composite_figure <- (Y2_CCBM_plot_StPaul / Y2_CCBM_plot_Waseca / Y2_CCBM_plot_Lamberton) +
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
  filename = "Y2_Composite_CCBM_By_Site.png",
  plot = Y2_CCBM_composite_figure,
  width = 8.5,
  height = 12,
  dpi = 300,
  device = "png"
)









#----F24 CN Ratio----

view(F24_AGBM_CN)





#----CC AGBM EXPLORATORY BOXPLOT----

ggplot(F24_AGBM_CN, aes(x = Trt, y = CN_rat, fill = Treatment_Full)) +
  geom_boxplot() +
  facet_wrap(~ Site) +
  scale_fill_manual(values = color_map) + 
  theme_bw(base_family = "serif") +
  labs(
    x = "Cover Crop Treatment",
    y = expression(CC~Shoot~Biomass~C:N~Ratio),
    fill = "CC Type"
  ) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    strip.text = element_text(face = "bold"))


#----MODEL----

F24_CC_CNrat_MOD = lmer(CN_rat ~ Treatment_Full + (1|Site:Rep),
                    data = F24_AGBM_CN)

#----CHECKING MODEL----
# 1. Set up a 1-row, 2-column plotting grid
par(mfrow = c(1, 2), family = "serif")

# 2. Residuals vs. Fitted (Tests Homoskedasticity / Constant Variance)
plot(fitted(F24_CC_CNrat_MOD), residuals(F24_CC_CNrat_MOD),
     xlab = "Fitted Values", ylab = "Residuals",
     main = "Residuals vs. Fitted")
abline(h = 0, col = "red", lty = 2)

# 3. Normal Q-Q Plot (Tests Normality of Residuals)
qqnorm(residuals(F24_CC_CNrat_MOD), main = "Normal Q-Q Plot")
qqline(residuals(F24_CC_CNrat_MOD), col = "red", lty = 2)

# Reset plotting grid
par(mfrow = c(1, 1))



#----TRANSFORMING THE MODEL----
# 1. Fit the log-transformed model (using the cleaned data)
F24_CC_CNrat_logMOD <- lm(
  log(CN_rat) ~ Treatment_Full * Site,
  data = F24_AGBM_CN)

# 2. Run diagnostics on the log-transformed model to verify correction
par(mfrow = c(1, 2), family = "serif")
plot(fitted(F24_CC_CNrat_logMOD), residuals(F24_CC_CNrat_logMOD),
     xlab = "Fitted Values", ylab = "Residuals", main = "Log-Model: Residuals vs. Fitted")
abline(h = 0, col = "red", lty = 2)

qqnorm(residuals(F24_CC_CNrat_logMOD), main = "Log-Model: Q-Q Plot")
qqline(residuals(F24_CC_CNrat_logMOD), col = "red", lty = 2)
par(mfrow = c(1, 1))




#----MODEL SUMMARY AND ANOVA----
# 1. Generate the standard model parameter summary
summary(F24_CC_CNrat_logMOD)

#----EXPONENTIATE FOR TRANSFORMED MODELS

exp(fixef(F24_CC_CNrat_logMOD))


# 2. Run Type III ANOVA using Satterthwaite ddf
F24_CC_CNrat_ANOVA <- car::Anova(F24_CC_CNrat_logMOD , type = "III")
F24_CC_CNrat_ANOVA


F24_CC_CNrat_ANOVA  <- anova(F24_CC_CNrat_logMOD, type = 3, ddf = "Satterthwaite")
print(F24_CC_CNrat_ANOVA)



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

#----EXTRACTING EMMEANS AND CLD, BACK-TRANSFORMING----

# ==============================================================================
# 1. Back-Transform using regrid() for Symmetric Standard Errors (response ± SE)
# ==============================================================================
# First, estimate marginal means on the log scale
F24_CC_CNrat_emm_log <- emmeans(
  F24_CC_CNrat_logMOD, 
  ~ Treatment_Full | Site)

F24_CC_CNrat_emm_log %>% 
  summary(infer = TRUE, type = "response")

F24_CC_CNrat_emm_log %>% 
  summary(infer = TRUE)

F24_CC_CNrat_contrasts <- pairs(F24_CC_CNrat_emm_log, adjust = "tukey")
print(F24_CC_CNrat_contrasts)

# Perform Tukey comparisons and get CLD letters on the log scale (where assumptions hold)
F24_CC_CNrat_cld <- cld(
  F24_CC_CNrat_emm_log, 
  alpha = 0.05, 
  Letters = letters, 
  adjust = "tukey", 
  decreasing = TRUE)

# Convert the grid to the response scale using regrid() for symmetric SEs
# Then merge the letters back based on all indexing factors
F24_CC_CNrat_plot_data <- regrid(F24_CC_CNrat_emm_log, transform = "response") %>%
  as.data.frame() %>%
  left_join(
    as.data.frame(F24_CC_CNrat_cld) %>% 
      select(Treatment_Full, Site, .group), 
    by = c("Treatment_Full", "Site")
  ) %>%
  mutate(Letter = trimws(.group))



F24_CC_CNrat_plot_data



F24_CC_CNrat_plot_data %>%
  select(Site, Treatment_Full, response, SE, Letter) %>%
  {
    cat(paste(colnames(.), collapse = "\t"), "\n")
    apply(., 1, function(x) cat(paste(x, collapse = "\t"), "\n"))
  }
#----WEIRD NEW PIPELINE FOR SITE-SPECIFIC GRAPHS----



# ==============================================================================
# 1. Simplified Data Wrangling (No complex indexing or middle-bar math)
# ==============================================================================
F24_CC_CNrat_plot_data <- F24_CC_CNrat_plot_data %>%
  mutate(
    # Count unique letters in this site-depth cluster
    Is_Sig = n_distinct(Letter) > 1,
    
    # Layer 1: Individual letters (blanked out if the group is non-significant)
    Bar_Letter = if_else(Is_Sig, Letter, ""),
    
    # Layer 2: Single "n.s." label (placed only on the first row of a non-sig cluster)
    Cluster_NS = if_else(!Is_Sig & row_number() == 1, "n.s.", ""),
    # Calculate numeric mid-point offset (e.g., 2 + 0.5 = 2.5 for 4 treatments)
    # This works dynamically for any even or odd number of treatments
    X_Midpoint = (n_distinct(Treatment_Full) + 1) / 2,
    
    # Find the maximum height in this cluster to float the "n.s." label safely
    Cluster_Max_Y = max(response + SE)
  ) %>%
  ungroup()

# ==============================================================================
# 2. Simplified Plotting Function (No complex positioning for n.s.)
# ==============================================================================

  F24_CC_CNrat_plot_data %>%
    
    ggplot(aes(x = Treatment_Full, y = response, fill = Treatment_Full)) +
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
      data = ~ filter(.x, Cluster_NS == "n.s."),
      aes(x = X_Midpoint, y = Cluster_Max_Y, label = Cluster_NS),
      vjust = -0.8,
      size = 5.5,
      fontface = "bold",
      inherit.aes = FALSE
    ) +
    
    # Aesthetics & Theme
    scale_fill_manual(values = color_map) +
    scale_y_continuous(expand = expansion(mult = c(0.01, 0.22))) +
    labs(
      title = paste("Fall 2024 Cover Crop Shoot Biomass C:N Ratio"),
      y = expression("Estimated Mean CC C:N Ratio"),
      x = "Treatment",
      caption = paste0(
        "Bars represent back-transformed emmeans.\n",
        "Error bars represent Delta-method approximated ±1 SE.\n",
        "Letters indicate significant differences within each site (Tukey HSD, alpha = 0.05)."
      ),
      fill = "Cover Crop Treatment"
    ) +
    theme_bw(base_family = "serif") +
    theme(
      legend.position = "none", 
      axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
      strip.text = element_text(face = "bold", size = 10)
    )











#----S25 CC C:N Ratio----


#----CC AGBM EXPLORATORY BOXPLOT----

ggplot(S25_AGBM_CN_novolunteer, aes(x = Trt, y = CN_rat, fill = Treatment_Full)) +
  geom_boxplot() +
  facet_wrap(~ Site) +
  scale_fill_manual(values = color_map) + 
  theme_bw(base_family = "serif") +
  labs(
    x = "Cover Crop Treatment",
    y = expression(CC~Shoot~Biomass~C:N~Ratio),
    fill = "CC Type"
  ) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    strip.text = element_text(face = "bold"))


#----MODEL----

S25_CC_CNrat_MOD = lmer(CN_rat ~ Treatment_Full + (1|Site:Rep),
                        data = S25_AGBM_CN_novolunteer)

#----CHECKING MODEL----
# 1. Set up a 1-row, 2-column plotting grid
par(mfrow = c(1, 2), family = "serif")

# 2. Residuals vs. Fitted (Tests Homoskedasticity / Constant Variance)
plot(fitted(S25_CC_CNrat_MOD), residuals(S25_CC_CNrat_MOD),
     xlab = "Fitted Values", ylab = "Residuals",
     main = "Residuals vs. Fitted")
abline(h = 0, col = "red", lty = 2)

# 3. Normal Q-Q Plot (Tests Normality of Residuals)
qqnorm(residuals(S25_CC_CNrat_MOD), main = "Normal Q-Q Plot")
qqline(residuals(S25_CC_CNrat_MOD), col = "red", lty = 2)

# Reset plotting grid
par(mfrow = c(1, 1))






S25_CC_CNrat_logMOD = lm(log(CN_rat) ~ Treatment_Full * Site,
                        data = S25_AGBM_CN_novolunteer)

#----CHECKING MODEL----
# 1. Set up a 1-row, 2-column plotting grid
par(mfrow = c(1, 2), family = "serif")

# 2. Residuals vs. Fitted (Tests Homoskedasticity / Constant Variance)
plot(fitted(S25_CC_CNrat_logMOD), residuals(S25_CC_CNrat_logMOD),
     xlab = "Fitted Values", ylab = "Residuals",
     main = "Residuals vs. Fitted")
abline(h = 0, col = "red", lty = 2)

# 3. Normal Q-Q Plot (Tests Normality of Residuals)
qqnorm(residuals(S25_CC_CNrat_logMOD), main = "Normal Q-Q Plot")
qqline(residuals(S25_CC_CNrat_logMOD), col = "red", lty = 2)

# Reset plotting grid
par(mfrow = c(1, 1))



#----MODEL SUMMARY AND ANOVA----
# 1. Generate the standard model parameter summary
summary(S25_CC_CNrat_logMOD)

#----EXPONENTIATE FOR TRANSFORMED MODELS

exp(fixef(S25_CC_CNrat_logMOD))

#----ANOVA for lm models----

S25_CC_CNrat_ANOVA <- car::Anova(S25_CC_CNrat_logMOD , type = "III")
S25_CC_CNrat_ANOVA

S25_CC_CNrat_ANOVA  <- anova(S25_CC_CNrat_logMOD, type = 3, ddf = "Satterthwaite")
print(S25_CC_CNrat_ANOVA)


S25_CC_CNrat_ANOVA %>%
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
#----EXTRACTING EMMEANS AND CLD, BACK-TRANSFORMING----

# ==============================================================================
# 1. Back-Transform using regrid() for Symmetric Standard Errors (response ± SE)
# ==============================================================================
# First, estimate marginal means on the log scale
S25_CC_CNrat_emm <- emmeans(
  S25_CC_CNrat_logMOD, 
  ~ Treatment_Full | Site)

S25_CC_CNrat_emm %>% 
  summary(infer = TRUE)

S25_CC_CNrat_contrasts <- pairs(S25_CC_CNrat_emm, adjust = "tukey")
print(S25_CC_CNrat_contrasts)

# Perform Tukey comparisons and get CLD letters on the log scale (where assumptions hold)
S25_CC_CNrat_cld <- cld(
  S25_CC_CNrat_emm, 
  alpha = 0.05, 
  Letters = letters, 
  adjust = "tukey", 
  decreasing = TRUE)
S25_CC_CNrat_cld 
# Convert the grid to the response scale using regrid() for symmetric SEs
# Then merge the letters back based on all indexing factors
S25_CC_CNrat_plot_data <- regrid(S25_CC_CNrat_emm, transform = "response") %>%
  as.data.frame() %>%
  left_join(
    as.data.frame(S25_CC_CNrat_cld) %>% 
      select(Treatment_Full, Site, .group), 
    by = c("Treatment_Full", "Site")
  ) %>%
  mutate(Letter = trimws(.group))


S25_CC_CNrat_plot_data

S25_CC_CNrat_plot_data %>%
  select(Site, Treatment_Full, response, SE, Letter) %>%
  {
    cat(paste(colnames(.), collapse = "\t"), "\n")
    apply(., 1, function(x) cat(paste(x, collapse = "\t"), "\n"))
  }

#----WEIRD NEW PIPELINE FOR SITE-SPECIFIC GRAPHS----



# ==============================================================================
# 1. Simplified Data Wrangling (No complex indexing or middle-bar math)
# ==============================================================================
S25_CC_CNrat_plot_data <- S25_CC_CNrat_plot_data %>%
  mutate(
    # Count unique letters in this site-depth cluster
    Is_Sig = n_distinct(Letter) > 1,
    
    # Layer 1: Individual letters (blanked out if the group is non-significant)
    Bar_Letter = if_else(Is_Sig, Letter, ""),
    
    # Layer 2: Single "n.s." label (placed only on the first row of a non-sig cluster)
    Cluster_NS = if_else(!Is_Sig & row_number() == 1, "n.s.", ""),
    # Calculate numeric mid-point offset (e.g., 2 + 0.5 = 2.5 for 4 treatments)
    # This works dynamically for any even or odd number of treatments
    X_Midpoint = (n_distinct(Treatment_Full) + 1) / 2,
    
    # Find the maximum height in this cluster to float the "n.s." label safely
    Cluster_Max_Y = max(response + SE)
  ) %>%
  ungroup()

# ==============================================================================
# 2. Simplified Plotting Function (No complex positioning for n.s.)
# ==============================================================================

  S25_CC_CNrat_plot_data %>%

    ggplot(aes(x = Treatment_Full, y = response, fill = Treatment_Full)) +
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
      size = 5,
      fontface = "bold"
    ) +
    
    # TEXT LAYER 2: Single "n.s." label (undodged; naturally centers on the x-axis category)
    geom_text(
      data = ~ filter(.x, Cluster_NS == "n.s."),
      aes(x = X_Midpoint, y = Cluster_Max_Y, label = Cluster_NS),
      vjust = -0.8,
      size = 5.5,
      fontface = "bold",
      inherit.aes = FALSE
    ) +
    
    # Aesthetics & Theme
    scale_fill_manual(values = color_map) +
    scale_y_continuous(expand = expansion(mult = c(0.01, 0.22))) +
    labs(
      y = expression("Estimated Mean CC C:N Ratio"),
      x = "Treatment",
      fill = "Cover Crop Treatment"
    ) +
    theme_bw(base_family = "serif") +
    theme(
      legend.position = "none", 
      axis.text.x = element_text(angle = 45, hjust = 1, size = 10, face = "bold"),
      strip.text = element_text(face = "bold", size = 10)
    )







#----Y2 CN rat models----

#----F25 C:N Ratio----


#----MODEL----

F25_CC_CNrat_MOD = lmer(CN_rat ~ Treatment_Full * Site + (1|Site:Rep),
                        data = F25_AGBM_CN)

#----CHECKING MODEL----
# 1. Set up a 1-row, 2-column plotting grid
par(mfrow = c(1, 2), family = "serif")

# 2. Residuals vs. Fitted (Tests Homoskedasticity / Constant Variance)
plot(fitted(F25_CC_CNrat_MOD), residuals(F25_CC_CNrat_MOD),
     xlab = "Fitted Values", ylab = "Residuals",
     main = "Residuals vs. Fitted")
abline(h = 0, col = "red", lty = 2)

# 3. Normal Q-Q Plot (Tests Normality of Residuals)
qqnorm(residuals(F25_CC_CNrat_MOD), main = "Normal Q-Q Plot")
qqline(residuals(F25_CC_CNrat_MOD), col = "red", lty = 2)

# Reset plotting grid
par(mfrow = c(1, 1))



#----TRANSFORMING THE MODEL----
# 1. Fit the log-transformed model (using the cleaned data)
F25_CC_CNrat_logMOD <- lmer(
  log(CN_rat) ~ Treatment_Full * Site + (1|Site:Rep),
  data = F25_AGBM_CN)

# 2. Run diagnostics on the log-transformed model to verify correction
par(mfrow = c(1, 2), family = "serif")
plot(fitted(F25_CC_CNrat_logMOD), residuals(F25_CC_CNrat_logMOD),
     xlab = "Fitted Values", ylab = "Residuals", main = "Log-Model: Residuals vs. Fitted")
abline(h = 0, col = "red", lty = 2)

qqnorm(residuals(F25_CC_CNrat_logMOD), main = "Log-Model: Q-Q Plot")
qqline(residuals(F25_CC_CNrat_logMOD), col = "red", lty = 2)
par(mfrow = c(1, 1))




#----MODEL SUMMARY AND ANOVA----
# 1. Generate the standard model parameter summary
summary(F25_CC_CNrat_logMOD)

#----EXPONENTIATE FOR TRANSFORMED MODELS

exp(fixef(F25_CC_CNrat_logMOD))




F25_CC_CNrat_ANOVA  <- anova(F25_CC_CNrat_logMOD, type = 3, ddf = "Satterthwaite")
print(F25_CC_CNrat_ANOVA)

F25_CC_CNrat_ANOVA  %>%
  tidy() %>%
  select(
    Source = term,
    `Sum Sq` = sumsq,
    `Mean Sq` = meansq,
    NumDF = NumDF,
    DenDF = DenDF,
    `F value` = statistic,
    `Pr(>F)` = p.value
  ) %>%
  mutate(`Pr(>F)` = format(`Pr(>F)`, scientific = FALSE, digits = 4)) %>%
  {
    cat(paste(colnames(.), collapse = "\t"), "\n")
    apply(., 1, function(x) cat(paste(x, collapse = "\t"), "\n"))
  }

#----EXTRACTING EMMEANS AND CLD, BACK-TRANSFORMING----

# ==============================================================================
# 1. Back-Transform using regrid() for Symmetric Standard Errors (response ± SE)
# ==============================================================================
# First, estimate marginal means on the log scale
F25_CC_CNrat_emm_log <- emmeans(
  F25_CC_CNrat_logMOD, 
  ~ Treatment_Full| Site)

F25_CC_CNrat_emm_log %>% 
  summary(infer = TRUE, type = "response")

F25_CC_CNrat_emm_log %>% 
  summary(infer = TRUE)

F25_CC_CNrat_contrasts <- pairs(F25_CC_CNrat_emm_log, adjust = "tukey")
print(F25_CC_CNrat_contrasts)

# Perform Tukey comparisons and get CLD letters on the log scale (where assumptions hold)
F25_CC_CNrat_cld <- cld(
  F25_CC_CNrat_emm_log, 
  alpha = 0.05, 
  Letters = letters, 
  adjust = "tukey", 
  decreasing = TRUE)

# Convert the grid to the response scale using regrid() for symmetric SEs
# Then merge the letters back based on all indexing factors
F25_CC_CNrat_plot_data <- regrid(F25_CC_CNrat_emm_log, transform = "response") %>%
  as.data.frame() %>%
  left_join(
    as.data.frame(F25_CC_CNrat_cld) %>% 
      select(Treatment_Full, Site, .group), 
    by = c("Treatment_Full", "Site")
  ) %>%
  mutate(Letter = trimws(.group))



F25_CC_CNrat_plot_data

F25_CC_CNrat_plot_data %>%
  select(Site, Treatment_Full, response, SE, Letter) %>%
  {
    cat(paste(colnames(.), collapse = "\t"), "\n")
    apply(., 1, function(x) cat(paste(x, collapse = "\t"), "\n"))
  }





#----S26 CC C:N Ratio----


#----CC AGBM EXPLORATORY BOXPLOT----

ggplot(S26_AGBM_CN, aes(x = Trt, y = CN_rat, fill = Treatment_Full)) +
  geom_boxplot() +
  facet_wrap(~ Site) +
  scale_fill_manual(values = color_map) + 
  theme_bw(base_family = "serif") +
  labs(
    x = "Cover Crop Treatment",
    y = expression(CC~Shoot~Biomass~C:N~Ratio),
    fill = "CC Type"
  ) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    strip.text = element_text(face = "bold"))


#----MODEL----

S26_CC_CNrat_MOD = lmer(CN_rat ~ Treatment_Full * Site + (1|Site:Rep),
                        data = S26_AGBM_CN)

#----CHECKING MODEL----
# 1. Set up a 1-row, 2-column plotting grid
par(mfrow = c(1, 2), family = "serif")

# 2. Residuals vs. Fitted (Tests Homoskedasticity / Constant Variance)
plot(fitted(S26_CC_CNrat_MOD), residuals(S26_CC_CNrat_MOD),
     xlab = "Fitted Values", ylab = "Residuals",
     main = "Residuals vs. Fitted")
abline(h = 0, col = "red", lty = 2)

# 3. Normal Q-Q Plot (Tests Normality of Residuals)
qqnorm(residuals(S26_CC_CNrat_MOD), main = "Normal Q-Q Plot")
qqline(residuals(S26_CC_CNrat_MOD), col = "red", lty = 2)

# Reset plotting grid
par(mfrow = c(1, 1))






S26_CC_CNrat_logMOD = lmer(log(CN_rat) ~ Treatment_Full * Site + (1|Site:Rep),
                           data = S26_AGBM_CN)

#----CHECKING MODEL----
# 1. Set up a 1-row, 2-column plotting grid
par(mfrow = c(1, 2), family = "serif")

# 2. Residuals vs. Fitted (Tests Homoskedasticity / Constant Variance)
plot(fitted(S26_CC_CNrat_logMOD), residuals(S26_CC_CNrat_logMOD),
     xlab = "Fitted Values", ylab = "Residuals",
     main = "Residuals vs. Fitted")
abline(h = 0, col = "red", lty = 2)

# 3. Normal Q-Q Plot (Tests Normality of Residuals)
qqnorm(residuals(S26_CC_CNrat_logMOD), main = "Normal Q-Q Plot")
qqline(residuals(S26_CC_CNrat_logMOD), col = "red", lty = 2)

# Reset plotting grid
par(mfrow = c(1, 1))



#----MODEL SUMMARY AND ANOVA----
# 1. Generate the standard model parameter summary
summary(S26_CC_CNrat_logMOD)

#----EXPONENTIATE FOR TRANSFORMED MODELS

exp(fixef(S26_CC_CNrat_logMOD))

#----ANOVA for lm models----



S26_CC_CNrat_ANOVA  <- anova(S26_CC_CNrat_logMOD, type = 3, ddf = "Satterthwaite")
print(S26_CC_CNrat_ANOVA)





S26_CC_CNrat_ANOVA %>%
  tidy() %>%
  select(
    Source = term,
    `Sum Sq` = sumsq,
    `Mean Sq` = meansq,
    NumDF = NumDF,
    DenDF = DenDF,
    `F value` = statistic,
    `Pr(>F)` = p.value
  ) %>%
  mutate(`Pr(>F)` = format(`Pr(>F)`, scientific = FALSE, digits = 4)) %>%
  {
    cat(paste(colnames(.), collapse = "\t"), "\n")
    apply(., 1, function(x) cat(paste(x, collapse = "\t"), "\n"))
  }


#----EXTRACTING EMMEANS AND CLD, BACK-TRANSFORMING----

# ==============================================================================
# 1. Back-Transform using regrid() for Symmetric Standard Errors (response ± SE)
# ==============================================================================
# First, estimate marginal means on the log scale
S26_CC_CNrat_emm <- emmeans(
  S26_CC_CNrat_logMOD, 
  ~ Treatment_Full| Site)

S26_CC_CNrat_emm %>% 
  summary(infer = TRUE)

S26_CC_CNrat_contrasts <- pairs(S26_CC_CNrat_emm, adjust = "tukey")
print(S25_CC_CNrat_contrasts)

# Perform Tukey comparisons and get CLD letters on the log scale (where assumptions hold)
S26_CC_CNrat_cld <- cld(
  S26_CC_CNrat_emm, 
  alpha = 0.05, 
  Letters = letters, 
  adjust = "tukey", 
  decreasing = TRUE)
S26_CC_CNrat_cld 
# Convert the grid to the response scale using regrid() for symmetric SEs
# Then merge the letters back based on all indexing factors
S26_CC_CNrat_plot_data <- regrid(S26_CC_CNrat_emm, transform = "response") %>%
  as.data.frame() %>%
  left_join(
    as.data.frame(S26_CC_CNrat_cld) %>% 
      select(Treatment_Full, Site, .group), 
    by = c("Treatment_Full", "Site")
  ) %>%
  mutate(Letter = trimws(.group))


S26_CC_CNrat_plot_data

S26_CC_CNrat_plot_data %>%
  select(Site, Treatment_Full, response, SE, Letter) %>%
  {
    cat(paste(colnames(.), collapse = "\t"), "\n")
    apply(., 1, function(x) cat(paste(x, collapse = "\t"), "\n"))
  }



#----F25 and S26 N uptake----


F25_Nuptake_mod <- lmer(log(N_kgha) ~ Treatment_Full * Site + (1|Site:Rep), data = F25_AGBM_CN)

# ANOVA summary
anova(F25_Nuptake_mod, type = 3, ddf = "Satterthwaite")

# Extract Estimated Marginal Means partitioned by Site
F25_Nuptake_emm <- emmeans(F25_Nuptake_mod , ~ Treatment_Full | Site)

# Generate Compact Letter Display
F25_Nuptake_cld <- cld(
  F25_Nuptake_emm, 
  alpha = 0.05, 
  Letters = letters, 
  adjust = "tukey", 
  decreasing = TRUE
)

F25_Nuptake_cld

F25_Nuptake_plot_data <- regrid(F25_Nuptake_emm, transform = "response") %>%
  as.data.frame() %>%
  left_join(
    as.data.frame(F25_Nuptake_cld) %>% 
      select(Treatment_Full, Site, .group), 
    by = c("Treatment_Full", "Site")
  ) %>%
  mutate(Letter = trimws(.group))


F25_Nuptake_plot_data








S26_Nuptake_mod <- lmer(log(N_kgha) ~ Treatment_Full * Site + (1|Site:Rep), data = S26_AGBM_CN)



# ANOVA summary
anova(S26_Nuptake_mod, type = 3, ddf = "Satterthwaite")

# Extract Estimated Marginal Means partitioned by Site
S26_Nuptake_emm <- emmeans(S26_Nuptake_mod , ~ Treatment_Full | Site)

# Generate Compact Letter Display
S26_Nuptake_cld <- cld(
  S26_Nuptake_emm, 
  alpha = 0.05, 
  Letters = letters, 
  adjust = "tukey", 
  decreasing = TRUE
)

S26_Nuptake_cld

S26_Nuptake_plot_data <- regrid(S26_Nuptake_emm, transform = "response") %>%
  as.data.frame() %>%
  left_join(
    as.data.frame(S26_Nuptake_cld) %>% 
      select(Treatment_Full, Site, .group), 
    by = c("Treatment_Full", "Site")
  ) %>%
  mutate(Letter = trimws(.group))


S26_Nuptake_plot_data




