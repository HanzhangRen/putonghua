# Set up ----------------------------------------------------------------------
# This is the analysis code for the presentation "Putonghua Usage with Parents 
# and Academic Performance"
# Title: Putonghua Usage with Parents and Academic Performance
# Author: Hanzhang Ren
# Date: March 23, 2023
# Task: Use CEPS baseline data to study the relationship between Putonghua
# usage at home and academic performance. Does this relationship differ between
# domestic migrants and non-migrants?
# Input: data/cepsw1studentEN.dta, cepsw1parentEN.dta
# Output: table/table1.tex; fig/figure1-4.png

# Load packages
# install.packages(
#   c("rstudioapi", "tidyverse", "foreign",
#     "arsenal", "Amelia", "survey",
#     "ggplot2", "performance", "dotwhisker", "effects", "sjPlot", "ggpubr"
#   )
# )
library(rstudioapi)
library(sjPlot)
library(tidyverse)
library(foreign)
library(arsenal)
library(Amelia)
library(survey)
library(ggplot2)
library(performance)
library(dotwhisker)
library(effects)
library(ggpubr)

# Set working directory
getSourceEditorContext()$path %>%
  dirname() %>%
  dirname() %>%
  setwd()

# Random seed
set.seed(0)

# Data cleaning ---------------------------------------------------------------
# Get the variables we need
student <- read.dta("data/cepsw1studentEN.dta") %>%
  select(ids, clsids,
    stdchn, stdmat, stdeng, b27, a04,
    a01, a02a, c01,
    a03, a06, b09, b06, b07,
    b01, b0201, b0202, b0203, b0204, b03, b0403, b0401, b0402,
    sweight
  ) %>%
  rename(
    vernacular = b27, migrant = a04,
    female = a01, birthyr = a02a, kindergarten = c01,
    han = a03, urban_hukou = a06, wealth = b09, m_edu = b06, f_edu = b07,
    only_child = b01,
    older_brothers = b0201, younger_brothers = b0202,
    older_sisters = b0203, younger_sisters = b0204,
    sibling_old_home_school = b03,
    sibling_home = b0403, m_home = b0401, f_home = b0402
  )
parent <- read.dta("data/cepsw1parentEN.dta") %>%
  select(ids,
    bd01,
    bd05, be23, be29, be30, be19, be15, be15a, be16, be17, be18, be1201, be1202
  ) %>%
  rename(
    migrant = bd01,
    urban_hukou = bd05, home = be23, area = be29, community = be30, 
    wealth = be19,
    sibling_old_home = be15, siblings_old_home = be15a,
    sibling_old_home_custody = be16,
    sibling_old_home_max_age = be17, sibling_old_home_school = be18,
    f_home = be1201, m_home = be1202
  )
data <- merge(student, parent, by = "ids", suffixes = c("_student", "_parent"))
summary(data)

# Recoding
levels(data$vernacular) <- c("P", "P + H", "H", "O")
data <- mutate(data,
  migrant = case_when(migrant_parent == "In the local county/district" ~ 0,
    !is.na(migrant_parent) ~ 1,
    migrant_student == "In the local county/district" ~ 0,
    !is.na(migrant_student) ~ 1
  ),
  female = ifelse(female == "Female", 1, 0),
  age = 2013 - birthyr,
  kindergarten = ifelse(kindergarten == "Yes, since [__] years old", 1, 0),
  han = ifelse(han == "The Han nationality", 1, 0),
  urban_hukou = case_when(
    urban_hukou_parent %in% c("Non-agricultural Hukou", "Residential Hukou") ~
      1,
    !is.na(urban_hukou_parent) ~ 0,
    urban_hukou_student %in% c("Non-agricultural Hukou", "Residential Hukou") ~
      1,
    !is.na(urban_hukou_student) ~ 0
  ),
  urban_residence = case_when(area == "Rural area" ~ 0,
    !is.na(area) ~ 1,
    home %in% c("Rural bungalow", "Rural building with more than one story") ~
      0,
    home %in% c(
      "Urban bungalow", "Ordinary urban apartment", "Urban housing estate"
    )
    ~ 1,
    community %in% c("Rural Area") ~ 0,
    community %in% c("Villages inside the city") ~ 1
  ),
  wealth = factor(ifelse(is.na(wealth_parent), wealth_student, wealth_parent)),
  parent_edu = case_when(
    m_edu == "Master degree or higher" ~ "Master degree or higher",
    f_edu == "Master degree or higher" ~ "Master degree or higher",
    as.numeric(m_edu) > as.numeric(f_edu) ~ m_edu,
    as.numeric(m_edu) <= as.numeric(f_edu) ~ f_edu
  ),
  parent_edu = factor(ifelse(parent_edu %in% c(
    "Technical secondary school or technical school degree",
    "Vocational high school degree"
  ),
  "Vocational/Technical High School", parent_edu
  ),
  levels = c("None",
    "Finished elementary school",
    "Junior high school degree",
    "Vocational/Technical High School",
    "Senior high school degree",
    "Junior college degree",
    "Bachelor degree",
    "Master degree or higher"
  )
  ),
  only_child = case_when(only_child == "Yes, I am" ~ 1,
    !is.na(only_child) ~ 0,
    older_brothers > 0 ~ 0,
    younger_brothers > 0 ~ 0,
    older_sisters > 0 ~ 0,
    younger_sisters > 0 ~ 0,
    sibling_old_home_school_student == "Yes, I do" ~ 0,
    sibling_home == "Yes" ~ 0,
    sibling_old_home == "Yes" ~ 0,
    siblings_old_home > 0 ~ 0,
    !is.na(sibling_old_home_custody) ~ 0,
    !is.na(sibling_old_home_max_age) ~ 0,
    !is.na(sibling_old_home_school_parent) ~ 0
  ),
  m_home = case_when(m_home_student == "Yes" ~ 1,
    m_home_student == "No" ~ 0,
    m_home_parent == "No" ~ 1, # The dataset clearly coded this backwards
    m_home_parent == "Yes" ~ 0
  ),
  f_home = case_when(f_home_student == "Yes" ~ 1,
    f_home_student == "No" ~ 0,
    f_home_parent == "No" ~ 1, # The dataset clearly coded this backwards
    f_home_parent == "Yes" ~ 0
  ),
  parents_home = factor(case_when(m_home & f_home ~ "Both",
    m_home + f_home == 1 ~ "OneParentHome",
    !m_home & !f_home ~ "NoParentHome"
  ),
  levels = c("Both", "OneParentHome", "NoParentHome")
  )
) %>%
  select(ids, clsids,
    stdchn, stdmat, stdeng, vernacular, migrant,
    female, age, kindergarten,
    han, urban_hukou, urban_residence, wealth, parent_edu,
    only_child, parents_home,
    sweight
  )
levels(data$wealth) <- c("VeryPoor",
  "SomewhatPoor",
  "Moderate",
  "SomewhatRich",
  "VeryRich"
)
levels(data$parent_edu) <- c("None",
  "Elementary School",
  "Middle School",
  "Vocational/Technical High School",
  "High School",
  "Junior College",
  "Bachelor's",
  "Graduate"
)
summary(data)

# Make a descriptive statistics table -----------------------------------------
data_table1 <- filter(data, vernacular != "O") %>%
  mutate(
    rich = ifelse(wealth %in% c("Somewhat Rich", "Very Rich"), 1, 0),
    parent_hi_edu = ifelse(parent_edu %in% c(
      "Vocational/Technical High School",
      "High School",
      "Junior College",
      "Bachelor's",
      "Graduate"
    ), 1, 0),
    both_parents_home = ifelse(parents_home == "Both", 1, 0),
    across(
      c("migrant",
        "female", "kindergarten",
        "han", "urban_hukou", "urban_residence", "rich", "parent_hi_edu",
        "only_child", "both_parents_home"
      ),
      ~ factor(.x)
    )
  ) %>%
  select(
    vernacular, migrant,
    kindergarten,
    han, urban_hukou, urban_residence, rich, parent_hi_edu,
    only_child, both_parents_home
  )
levels(data_table1$vernacular) <- c("P", "P + H", "H", "O")
labels(data_table1) <- c(
  "Vernacular", "Migrant",
  "Had Kindergarten",
  "Han Ethnicity",
  "Urban Hukou", "Urban Residence",
  "Rich", "Max Parent Edu $>$ MS",
  "Only Child", "Both Parents Home"
)
table1 <- file("table/table1.tex")
tableby(vernacular ~
  migrant +
  kindergarten +
  han + urban_hukou + urban_residence + rich + parent_hi_edu +
  only_child + both_parents_home,
data = data_table1,
total = FALSE,
test = FALSE,
numeric.simplify = TRUE,
cat.simplify = TRUE,
numeric.stats = "meansd",
cat.stats = "countpct"
) %>%
  summary(text = "latex") %>%
  print() %>%
  capture.output(file = table1)
# Who's richer? Migrants or non-migrants?
group_by(data_table1, migrant) %>%
  summarize(rich = mean(rich == "1", na.rm = TRUE))

# Impute the data -------------------------------------------------------------
sum(is.na(data)) / (nrow(data) * (ncol(data) - 3))
data_imputed <- mutate(data,
  ph_migrant = ifelse(vernacular == "P + H" & migrant == 1, 1, 0),
  h_migrant = ifelse(vernacular == "H" & migrant == 1, 1, 0),
  o_migrant = ifelse(vernacular == "O" & migrant == 1, 1, 0)
) %>%
  amelia(
    m = 1,
    idvars = c("ids", "sweight"),
    noms = c("clsids", "vernacular", "wealth", "parent_edu", "parents_home")
  )
data_imputed <- mutate(data_imputed$imputations$imp1, clsids = factor(clsids))

# Plot weighted sample averages by vernacular use -----------------------------
# Calculate the averages
design <- svydesign(~ clsids, weights = ~ sweight, data = data_imputed)
df <- degf(design)
# For each of Chinese, Math, and English, this function produces a dataframe
# indicating the average, standard error, and confidence intervals for exam
# scores, faceted by the type of vernacular usage at home
mean_score <- function(subject) {
  stat <- svyby(formula(paste("~", subject)), ~ vernacular, design, svymean)
  confint <- confint(stat, df = df)
  cbind(stat, confint) %>%
    mutate(subject = subject) %>%
    rename(score = all_of(subject))
}
data_figure1 <- map_dfr(c("stdchn", "stdmat", "stdeng"), mean_score) %>%
  rename(lower = "2.5 %", upper = "97.5 %") %>%
  filter(vernacular != "O") %>%
  mutate(
    subject = factor(subject, levels = c("stdchn", "stdmat", "stdeng")),
    vernacular = factor(vernacular)
  )
levels(data_figure1$subject) <- c("Chinese", "Math", "English")
levels(data_figure1$vernacular) <- c(
  "Putonghua", "Putonghua +\nHometown Vernacular", "Hometown Vernacular"
)

# Plot the weighted average with confidence intervals
ggplot(data_figure1,
  aes(subject, score, color = vernacular, shape = vernacular)
) +
  geom_point(position = position_dodge(.75)) +
  geom_errorbar(aes(ymin = lower, ymax = upper),
    position = position_dodge(.75)
  ) +
  labs(x = "Subject", y = "Score", color = "", shape = "") +
  theme_bw()
ggsave("fig/figure1.png", width = 8, height = 2.25, units = "in", dpi = 300)
  
# Plot linear models without interactions -------------------------------------
# Run the models
levels(data_imputed$vernacular) <- c("P", "PH", "H", "O")
# This function generates a desired linear model for each academic subject.
# This model does not include interactions.
lm_score <- function(subject) {
  lm(formula(paste(subject,
    "~ vernacular + migrant + female + age + kindergarten + 
    han + urban_hukou + urban_residence + wealth + parent_edu + 
    only_child + parents_home + 
    clsids"
  )), data = data_imputed)
}
lm_chn <- lm_score("stdchn")
summary(lm_chn)
lm_mat <- lm_score("stdmat")
summary(lm_mat)
lm_eng <- lm_score("stdeng")
summary(lm_eng)
check_model(lm_chn)
check_model(lm_mat)
check_model(lm_eng)

# Make a coefficient plot of the model coefficients
dwplot(list(lm_chn, lm_mat, lm_eng),
  vars_order = c(
    "vernacularPH", "vernacularH", "migrant",
    "female", "kindergarten",
    "urban_hukou",
    "wealthModerate", "wealthVeryRich",
    "parent_eduGraduate",
    "parents_homeNoParentHome"
  ),
  model_order = c("Model 1", "Model 2", "Model 3")
) %>% 
  relabel_predictors(
    c(
      vernacularPH = "Putonghua + Hometown Vernacular (vs. Putonghua)",
      vernacularH = "Hometown Vernacular",
      migrant = "Migrant",
      female = "Female",
      kindergarten = "Had Kindergarten",
      urban_hukou = "Urban Household Registration",
      wealthModerate = "Moderate (vs. Very Poor)",
      wealthVeryRich = "Very Rich",
      parent_eduGraduate = "Parent Graduate Degree (vs. No Education)",
      parents_homeNoParentHome = "No Parent Home (vs. Both Home)"
    )
  ) +
  theme_bw() +
  labs(x = "Coefficient (SD of Scores = 10)", y = "", color = "") +
  scale_color_discrete(labels = c("English", "Math", "Chinese")) +
  geom_vline(xintercept = 0)
ggsave("fig/figure2.png", width = 8, height = 2.25, units = "in", dpi = 300)

# Plot linear models with interactions-----------------------------------------
# This function generates a desired linear model for each academic subject.
# This model includes interactions.
lm_score_interact <- function(subject) {
  lm(formula(paste(subject,
    "~ vernacular + migrant + ph_migrant + h_migrant + o_migrant +
    female + age + kindergarten + 
    han + urban_hukou + urban_residence + wealth + parent_edu + 
    only_child + parents_home + 
    clsids"
  )), data = data_imputed)
}
options(max.print = 10000)
lm_chn_interact <- lm_score_interact("stdchn")
summary(lm_chn_interact)
lm_mat_interact <- lm_score_interact("stdmat")
summary(lm_mat_interact)
lm_eng_interact <- lm_score_interact("stdeng")
summary(lm_eng_interact)
options(max.print = 1000)
check_model(lm_chn_interact)
check_model(lm_mat_interact)
check_model(lm_eng_interact)

# Make a coefficient plot of the model coefficients
dwplot(list(lm_chn_interact, lm_mat_interact, lm_eng_interact),
  vars_order = c( "vernacularPH", "vernacularH", "migrant",
    "ph_migrant", "h_migrant"
  ),
  model_order = c("Model 1", "Model 2", "Model 3")
) %>% # plot line at zero _behind_coefs
  relabel_predictors(
    c(
      vernacularPH = "Putonghua + Hometown Vernacular (vs. Putonghua)",
      vernacularH = "Hometown Vernacular",
      migrant = "Migrant",
      ph_migrant = "(Putonghua + Hometown Vernacular) * Migrant",
      h_migrant = "Hometown Vernacular * Migrant"
    )
  ) +
  theme_bw() +
  labs(x = "Coefficient (SD of Scores = 10)", y = "", color = "") +
  scale_color_discrete(labels = c("English", "Math", "Chinese")) +
  geom_vline(xintercept = 0)
ggsave("fig/figure3.png", width = 8, height = 2.25, units = "in", dpi = 300)

# This function plots a margins plot for each subject
lm_score_interact_plot <- function(subject) {
  lm(formula(paste(
    subject,
    "~ vernacular * migrant +
    female + age + kindergarten + 
    han + urban_hukou + urban_residence + wealth + parent_edu + 
    only_child + parents_home + 
    clsids"
  )),
  data = data_imputed
  ) %>%
    plot_model(
      type = "eff", terms = c("vernacular [P,PH,H]", "migrant [0, 1]")
    ) +
    ylim(69.2, 71.5) +
    labs(title = "", x = "", y = "", color = "") +
    scale_color_discrete(labels = c("Non-migrant", "Migrant")) +
    scale_x_continuous(breaks = c(1, 2, 3), labels = c("P", "P + H", "H")) +
    theme_bw()
}
chn <- lm_score_interact_plot("stdchn") + theme(legend.position = "none")
mat <- lm_score_interact_plot("stdmat") +
  theme(
    legend.position = "none",
    axis.text.y = element_blank(), axis.ticks.y = element_blank()
  )
eng <- lm_score_interact_plot("stdeng") +
  theme(axis.text.y = element_blank(), axis.ticks.y = element_blank())
ggarrange(chn, mat, eng,
  labels = c("Chinese", "Math", "English"),
  ncol = 3, nrow = 1,
  widths = c(1, .9, 1.45)
)
ggsave("fig/figure4.png", width = 8, height = 4.5, units = "in", dpi = 300)
