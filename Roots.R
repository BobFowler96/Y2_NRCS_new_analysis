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





#----Loading F25 Root data----

F25_Root = read.csv("F25_Root.csv")
view(F25_Root)
F25_Root  = F25_Root  %>% 
  mutate(
    Date = as.Date(Date, format = "%Y-%m-%d"),
    Depth_ft = factor(Depth_ft, levels = c("0-1", "1-2", "2-3")),
    Depth_cm = case_when(
      Depth_ft == "0-1" ~ "0-30",
      Depth_ft == "1-2" ~ "30-60",
      Depth_ft == "2-3" ~ "60-90",
      TRUE        ~ NA_character_ 
    ),
    Site = factor(Site),
    Rep = factor(Rep),
    Trt = factor(Trt, levels = c("R1", "R4", "O1", "O4")),
    Trt_type = factor(Trt_type),
    Treatment_Full = case_when(
      Trt == "R1" ~ "Rye 10",
      Trt == "R4" ~ "Rye 80",
      Trt == "O1" ~ "Oat/Radish 10/1",
      Trt == "O4" ~ "Oat/Radish 60/6",
      TRUE        ~ NA_character_ # This catches any codes that don't match
    ),
    Treatment_Full = factor(Treatment_Full, levels = c(
      "Rye 10", "Rye 80", 
      "Oat/Radish 10/1", "Oat/Radish 60/6"
    )),
    Trt_type = factor(Trt_type, levels = c("Cereal Rye", "Oat + Radish")))


F25_Root <- F25_Root %>%
  mutate(N_kgm3 = Root_kgm3 * Nmult,
         C_kgm3 = Root_kgm3 * Cmult)

view(F25_Root)

#----CC ROOT BIOMASS EXPLORATORY BOXPLOT----

ggplot(F25_Root, aes(x = Trt, y = Root_kgm3, fill = Treatment_Full)) +
  geom_boxplot() +
  facet_wrap(~ Site) +
  scale_fill_manual(values = color_map) + 
  theme_bw(base_family = "serif") +
  labs(
    x = "Cover Crop Treatment",
    y = expression(CC~Root~Biomass~(kg~m^-3)),
    fill = "CC Type"
  ) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    strip.text = element_text(face = "bold"))



#----F25 CC ROOT N EXPLORATORY BOXPLOT----
ggplot(F25_Root, aes(x = Trt, y = N_kgha, fill = Treatment_Full)) +
  geom_boxplot() +
  facet_wrap(~ Site) +
  scale_fill_manual(values = color_map) + 
  theme_bw(base_family = "serif") +
  labs(
    x = "Cover Crop Treatment",
    y = expression(CC~Root~Nitrogen~(kg~ha^-1)),
    fill = "CC Type"
  ) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    strip.text = element_text(face = "bold"))



#----F25 CC ROOT C:N  RATIO EXPLORATORY BOXPLOT----
ggplot(F25_Root, aes(x = Trt, y = CN_rat, fill = Treatment_Full)) +
  geom_boxplot() +
  facet_wrap(~ Site) +
  scale_fill_manual(values = color_map) + 
  theme_bw(base_family = "serif") +
  labs(
    x = "Cover Crop Treatment",
    y = expression(CC~Root~C:N~Ratio),
    fill = "CC Type"
  ) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    strip.text = element_text(face = "bold"))



view(F25_Root)


#----Root biomass by treatment and depth----

p_RBM_Trt_mod = lm(Root_kgm3 ~ Treatment_Full * Depth_cm ,data = F25_Root)

anova(p_RBM_Trt_mod)


p_RBM_Trt_emm <- emmeans(
  p_RBM_Trt_mod, 
  ~ Depth_cm|Treatment_Full)


# Perform Tukey comparisons and get CLD letters on the log scale (where assumptions hold)
p_RBM_Trt_cld <- cld(
  p_RBM_Trt_emm, 
  alpha = 0.05, 
  Letters = letters, 
  adjust = "tukey", 
  decreasing = TRUE)
p_RBM_Trt_cld 


#----Root biomass N by treatment and depth----

p_RBMN_Trt_mod = lmer(N_kgm3 ~ Treatment_Full * Depth_cm + (1|Rep) ,data = F25_Root)

anova(p_RBMN_Trt_mod)


p_RBMN_Trt_emm <- emmeans(
  p_RBMN_Trt_mod, 
  ~ Depth_cm|Treatment_Full)


# Perform Tukey comparisons and get CLD letters on the log scale (where assumptions hold)
p_RBMN_Trt_cld <- cld(
  p_RBMN_Trt_emm, 
  alpha = 0.05, 
  Letters = letters, 
  adjust = "tukey", 
  decreasing = TRUE)
p_RBMN_Trt_cld 





#----Root biomass C by treatment and depth----

p_RBMC_Trt_mod = lmer(C_kgm3 ~ Treatment_Full * Depth_cm + (1|Rep) ,data = F25_Root)

anova(p_RBMC_Trt_mod)


p_RBMC_Trt_emm <- emmeans(
  p_RBMC_Trt_mod, 
  ~ Depth_cm|Treatment_Full)


# Perform Tukey comparisons and get CLD letters on the log scale (where assumptions hold)
p_RBMC_Trt_cld <- cld(
  p_RBMC_Trt_emm, 
  alpha = 0.05, 
  Letters = letters, 
  adjust = "tukey", 
  decreasing = TRUE)
p_RBMC_Trt_cld 


#----Root biomass C:N by treatment and depth----

p_RCN_Trt_mod = lm(CN_rat ~ Treatment_Full,data = F25_Root)

anova(p_RCN_Trt_mod)


p_RCN_Trt_emm <- emmeans(
  p_RCN_Trt_mod, 
  ~ Treatment_Full)


# Perform Tukey comparisons and get CLD letters on the log scale (where assumptions hold)
p_RCN_Trt_cld <- cld(
  p_RCN_Trt_emm, 
  alpha = 0.05, 
  Letters = letters, 
  adjust = "tukey", 
  decreasing = TRUE)
p_RCN_Trt_cld 
