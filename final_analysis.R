# ==============================================================================
# Scatterbrainedness & Interpersonal Attraction
# Bachelor thesis analysis script
#
# R version 4.5.2 (2025-10-31 ucrt)
# Platform: x86_64-w64-mingw32/x64
# Running under: Windows 11 x64 (build 26200)
#
# Requirements:
# final_survey_results_1.csv and final_survey_results_2.csv are in
# the working directory.
#
# Note: Used UTF-8 codes for various symbols and "Umlaute" to avoid
# compatibility issues during reproduction of this script.
# ==============================================================================

# install.packages(c("tidyverse", "lme4", "lmerTest", "psych"))
library(tidyverse)
library(lme4)
library(lmerTest)
library(psych)

# ==== 1. Load & merge =========================================================
batch1 <- read_csv("final_survey_results_1.csv", locale = locale(encoding = "UTF-8"))
batch2 <- read_csv("final_survey_results_2.csv", locale = locale(encoding = "UTF-8"))

data <- bind_cols(batch1, batch2)

item_cols <- names(data)[str_detect(names(data), "^G\\d{2}V\\d{2}\\[SQ\\d{3}\\]$")]

# ==== 2. Basic cleaning =======================================================
data <- data |>
  select(-any_of(c("submitdate", "lastpage", "startlanguage", "seed",
                   "Consent01", "Consent02[SQ001]", "Info01", "G45Q129"))) |>
  rename(age = Demo01, participant_gender = Demo02, education = Demo03)

# I deliberately do not drop all NA columns here. With 42 groups and
# n = 151 randomly assigned participants, it's entirely possible that zero
# participants landed in a given group, which would mean that entire group
# is dropped. That would interfere with calculations further along.

# ==== 3. Reshape vignette-rating items to long format =========================
df_long <- data |>
  pivot_longer(
    cols = all_of(item_cols),
    names_to = "item",
    values_to = "rating_raw"
  ) |>
  mutate(
    group   = str_extract(item, "G\\d{2}"),
    slot    = str_extract(item, "V\\d{2}"),
    subq    = as.integer(str_extract(item, "(?<=SQ)\\d{3}")),
    context = if_else(subq <= 5, "social", "work")
  ) |>
  drop_na(rating_raw)

# Recode the 5-point agreement scale to 1-5 (higher = more agreement)
df_long <- df_long |>
  mutate(rating = as.integer(factor(rating_raw,
                                    levels = c("Starke Ablehnung", "Ablehnung", "Neutral",
                                               "Zustimmung", "Starke Zustimmung"))))

# Reverse-code the negatively worded attraction items so that, after
# reversal, higher rating = more positive/likeable for all 10 items.
reverse_subq <- c(2, 3, 4, 6, 9, 10)
df_long <- df_long |>
  mutate(rating = if_else(subq %in% reverse_subq, 6 - rating, rating))

# ==== 4. Add frequency (scatterbrainedness level) and name/gender info ========
frequency_patterns <- list(
  c("low", "med", "high"),
  c("low", "high", "med"),
  c("med", "low", "high"),
  c("med", "high", "low"),
  c("high", "low", "med"),
  c("high", "med", "low")
)
group_nums <- 1:42

frequency_lookup <- map_dfr(group_nums, function(g) {
  pattern <- frequency_patterns[[(g - 1) %% 6 + 1]]
  tibble(group = sprintf("G%02d", g), slot = sprintf("V%02d", 1:3),
         frequency = pattern)
})

name_lookup <- tibble(name_num = 1:14,
                      vignette_gender = rep(c("female", "male"), 7))

group_name_lookup <- map_dfr(group_nums, function(g) {
  start_val <- (g - 1) %% 14
  tibble(group = sprintf("G%02d", g), slot = sprintf("V%02d", 1:3),
         name_num = ((start_val + 0:2) %% 14) + 1)
}) |>
  left_join(name_lookup, by = "name_num")

df_long <- df_long |>
  left_join(frequency_lookup, by = c("group", "slot")) |>
  left_join(group_name_lookup, by = c("group", "slot")) |>
  mutate(
    frequency = factor(frequency, levels = c("low", "med", "high")),
    context   = factor(context, levels = c("social", "work")),
    slot_num  = as.integer(str_extract(slot, "\\d{2}"))  # presentation order 1-3
  )

# ==== 5. Own scatterbrainedness (CFQ-12) ======================================
cfq_cols   <- paste0("CFQ[SQ", sprintf("%03d", 1:12), "]")
cfq_levels <- c("Nie", "Selten", "Manchmal", "Oft", "Sehr oft")

cfq_wide <- data |>
  select(id, all_of(cfq_cols)) |>
  mutate(across(all_of(cfq_cols), ~ as.integer(factor(., levels = cfq_levels))))

# Reliability of CFQ-12 (higher = more self-reported everyday cognitive
# failures / scatterbrainedness)
cat("\n--- CFQ-12 reliability ---\n")
cfq_alpha <- psych::alpha(cfq_wide |> select(-id))
print(cfq_alpha)

cfq_scores <- cfq_wide |>
  rowwise() |>
  mutate(CFQ_mean = mean(c_across(all_of(cfq_cols)), na.rm = TRUE)) |>
  ungroup() |>
  select(id, CFQ_mean)

# ==== 6. Attitude towards scatterbrainedness (ATS-7) ==========================
# Used UTF-8 encoding variants of ü and ä to avoid compatibility issues
# during reproduction.
ats_cols   <- paste0("ATS[SQ", sprintf("%03d", 1:7), "]")
ats_levels <- c("trifft \u00fcberhaupt nicht zu", "trifft nicht zu",
                "trifft eher nicht zu", "teils/teils",
                "trifft eher zu", "trifft zu", "trifft vollst\u00e4ndig zu")

ats_wide <- data |>
  select(id, all_of(ats_cols)) |>
  mutate(across(all_of(ats_cols), ~ as.integer(factor(., levels = ats_levels))))

# ATS[SQ001] is a self-identification item ("I am scatterbrained"),conceptually
# distinct and therefore kept separately.
ats_wide <- ats_wide |>
  mutate(
    `ATS[SQ003]_r` = 8 - `ATS[SQ003]`,
    `ATS[SQ005]_r` = 8 - `ATS[SQ005]`,
    `ATS[SQ006]_r` = 8 - `ATS[SQ006]`
  )

ats_attitude_cols <- c("ATS[SQ002]", "ATS[SQ004]", "ATS[SQ007]",
                       "ATS[SQ003]_r", "ATS[SQ005]_r", "ATS[SQ006]_r")

cat("\n--- ATS attitude subscale (6 items) reliability ---\n")
ats_alpha <- psych::alpha(ats_wide |> select(all_of(ats_attitude_cols)))
print(ats_alpha)

ats_scores <- ats_wide |>
  rowwise() |>
  mutate(
    ATS_attitude   = mean(c_across(all_of(ats_attitude_cols)), na.rm = TRUE),
    ATS_self_ident = `ATS[SQ001]`
  ) |>
  ungroup() |>
  select(id, ATS_attitude, ATS_self_ident)

# ==== 7. Merge person-level scores + demographics into df_long ================
df_long <- df_long |>
  left_join(cfq_scores, by = "id") |>
  left_join(ats_scores, by = "id") |>
  mutate(
    age      = as.numeric(age),
    id       = factor(id),
    name_num = factor(name_num)
  )

# Convergent/discriminant check: does own scatterbrainedness relate to
# attitude towards scatterbrainedness, or to self-identification as
# scatterbrained.
person_scores <- df_long |> distinct(id, CFQ_mean, ATS_attitude, ATS_self_ident)
cat("\n--- CFQ_mean vs ATS_attitude ---\n")
print(cor.test(person_scores$CFQ_mean, person_scores$ATS_attitude))
cat("\n--- CFQ_mean vs ATS_self_ident ---\n")
print(cor.test(person_scores$CFQ_mean, person_scores$ATS_self_ident))

# --------------------
# Demographics table
# --------------------
library(gt)

demo_participants <- data |>
  distinct(id, age, participant_gender, education)

# Age: continuous summary
age_summary <- demo_participants |>
  summarise(
    Variable = "Age",
    Category = "M (SD) [range]",
    Value = sprintf("%.1f (%.1f) [%d\u2013%d]",
                    mean(age, na.rm = TRUE), sd(age, na.rm = TRUE),
                    min(age, na.rm = TRUE), max(age, na.rm = TRUE)),
    n = sum(!is.na(age))
  )

# Gender and education: categorical counts + percentages
categorical_summary <- function(df, var, var_label) {
  df |>
    filter(!is.na(.data[[var]]), .data[[var]] != "") |>
    count(Category = .data[[var]], name = "n") |>
    mutate(Variable = var_label,
           Value = sprintf("%d (%.1f%%)", n, 100 * n / sum(n))) |>
    select(Variable, Category, Value, n)
}

gender_summary    <- categorical_summary(demo_participants, "participant_gender", "Gender")
education_summary <- categorical_summary(demo_participants, "education", "Education")

demographics_table <- bind_rows(age_summary, gender_summary, education_summary) |>
  select(Variable, Category, Value)

cat("\n--- Sample demographics (N = ", nrow(demo_participants), ") ---\n", sep = "")
print(demographics_table)

# gt table
 demographics_gt <- demographics_table |>
   gt(groupname_col = "Variable") |>
   cols_label(Category = "", Value = "") |>
   tab_header(title = "Table 1", subtitle = paste0("Sample Characteristics (N = ", nrow(demo_participants), ")")) |>
   tab_options(table.width = pct(70))

 demographics_gt

# ==== 8. Reliability of the social / work attraction subscales ================
attraction_wide <- df_long |>
  select(id, group, slot, context, subq, rating) |>
  pivot_wider(names_from = subq, values_from = rating, names_prefix = "item")

social_items <- paste0("item", 1:5)
work_items   <- paste0("item", 6:10)

social_wide <- attraction_wide |> filter(context == "social") |> select(all_of(social_items))
work_wide   <- attraction_wide |> filter(context == "work")   |> select(all_of(work_items))

cat("\n--- Social attraction subscale (5 items) reliability ---\n")
print(psych::alpha(social_wide))

cat("\n--- Work attraction subscale (5 items) reliability ---\n")
print(psych::alpha(work_wide))

# ==== 9. Descriptives & manipulation-check plot ===============================
interaction_means <- df_long |>
  group_by(context, frequency) |>
  summarise(mean = mean(rating), sd = sd(rating), n = n(), .groups = "drop")
print(interaction_means)

ggplot(interaction_means, aes(x = frequency, y = mean, color = context, group = context)) +
  geom_line() +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = mean - sd / sqrt(n), ymax = mean + sd / sqrt(n)), width = .1) +
  scale_x_discrete(limits = c("low", "med", "high")) +
  labs(title = "Likeability by scatterbrainedness level and context",
       x = "Scatterbrainedness level (vignette manipulation)",
       y = "Mean rating (1-5, reverse-coded, higher = more positive)",
       color = "Context") +
  theme_minimal()

# ==============================================================================
# PRIMARY ANALYSIS: Does likeability differ between social and work context?
# ==============================================================================

# Simple comparison
person_context_means <- df_long |>
  group_by(id, context) |>
  summarise(mean_rating = mean(rating), .groups = "drop") |>
  pivot_wider(names_from = context, values_from = mean_rating)

cat("\n--- Paired t-test: social vs. work (person-level means) ---\n")
t_context <- t.test(person_context_means$social, person_context_means$work, paired = TRUE)
print(t_context)

cohens_d_context <- with(person_context_means,
                         (mean(social) - mean(work)) / sd(social - work))
cat("Paired Cohen's d (social - work):", round(cohens_d_context, 3), "\n")

# Full mixed model
m_context <- lmer(
  rating ~ context * frequency + (1 + frequency | id) + (1 | subq) + (1 | name_num),
  data = df_long, REML = TRUE,
  control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5))
)
cat("\n--- Primary LMM: context * frequency (a priori random structure) ---\n")
summary(m_context)
anova(m_context)

cat("isSingular(m_context):", isSingular(m_context), "\n")

# Alternative 1: drop the by-participant slope/intercept
m_context_nocorr <- lmer(
  rating ~ context * frequency + (1 + frequency || id) + (1 | subq) + (1 | name_num),
  data = df_long, REML = TRUE,
  control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5))
)
cat("isSingular(m_context_nocorr):", isSingular(m_context_nocorr), "\n")

# Alternative 2: without the additional name_num term
m_context_minimal <- lmer(
  rating ~ context * frequency + (1 + frequency | id) + (1 | subq),
  data = df_long, REML = TRUE,
  control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5))
)
cat("isSingular(m_context_minimal):", isSingular(m_context_minimal), "\n")

# Compare fit of the three versions
anova(m_context_minimal, m_context_nocorr, m_context, refit = FALSE)

# Simple-effects tests: is there a significant context (social vs.
# work) difference at each frequency level?
# install.packages("emmeans")
library(emmeans)

emm_context_by_freq <- emmeans(m_context, ~ context | frequency)
cat("\n--- Simple effects: context contrast within each frequency level ---\n")
print(pairs(emm_context_by_freq, adjust = "none"))
print(confint(pairs(emm_context_by_freq, adjust = "none")))

# ==============================================================================
# MODEL QUALITY: Residuals + effect size for the primary model
# ==============================================================================
# install.packages("performance")
# install.packages("see")
library(see)
library(performance)

# Residuals
check_model(m_context)

# Effect size: variance explained. Nakagawa & Schielzeth's (2013) R^2
r2_context <- r2_nakagawa(m_context)
cat("\n--- Effect size: R^2 (Nakagawa & Schielzeth, 2013) ---\n")
print(r2_context)

# Effect size for context:frequency interaction
m_context_no_interaction <- lmer(
  rating ~ context + frequency + (1 + frequency | id) + (1 | subq) + (1 | name_num),
  data = df_long, REML = TRUE,
  control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5))
)
r2_full    <- r2_nakagawa(m_context)$R2_marginal
r2_reduced <- r2_nakagawa(m_context_no_interaction)$R2_marginal
f2_interaction <- (r2_full - r2_reduced) / (1 - r2_full)
cat("\nLocal effect size (Cohen's f^2) for context:frequency interaction:",
    round(f2_interaction, 3), "\n")

# ==============================================================================
# SECONDARY ANALYSIS: Own scatterbrainedness & attitude towards it
# ==============================================================================

# Center continuous predictors for interpretability of main effects
df_long <- df_long |>
  mutate(
    CFQ_c = as.numeric(scale(CFQ_mean, scale = FALSE)),
    ATS_c = as.numeric(scale(ATS_attitude, scale = FALSE))
  )

cat("\n--- Secondary LMM: own scatterbrainedness (CFQ) as moderator ---\n")
m_cfq <- lmer(
  rating ~ context * frequency * CFQ_c + (1 + frequency | id) + (1 | subq) + (1 | name_num),
  data = df_long, control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5))
)
summary(m_cfq)
cat("isSingular(m_cfq):", isSingular(m_cfq), "\n")

cat("\n--- Secondary LMM: attitude towards scatterbrainedness (ATS) as moderator ---\n")
m_ats <- lmer(
  rating ~ context * frequency * ATS_c + (1 + frequency | id) + (1 | subq) + (1 | name_num),
  data = df_long, control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5))
)
summary(m_ats)
cat("isSingular(m_ats):", isSingular(m_ats), "\n")

# Simpler alternative to 3-way interaction
cat("\n--- Secondary LMM (simplified): CFQ + ATS, two-way interactions only ---\n")
m_secondary_simple <- lmer(
  rating ~ context * frequency + context * CFQ_c + frequency * CFQ_c +
    context * ATS_c + frequency * ATS_c +
    (1 + frequency | id) + (1 | subq) + (1 | name_num),
  data = df_long, control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5))
)
summary(m_secondary_simple)

# ==============================================================================
# Randomization checks (order / name / gender effects)
# ==============================================================================

cat("\n--- Randomization check: presentation order ---\n")
m_order <- lmer(rating ~ context * frequency + slot_num +
                  (1 + frequency | id) + (1 | subq) + (1 | name_num),
                data = df_long, control = lmerControl(optimizer = "bobyqa"))
summary(m_order)  # slot_num should be non-significant

cat("\n--- Randomization check: gender of fictional vignette person ---\n")
m_gender <- lmer(rating ~ context * frequency + vignette_gender +
                   (1 + frequency | id) + (1 | subq) + (1 | name_num),
                 data = df_long, control = lmerControl(optimizer = "bobyqa"))
summary(m_gender)  # vignette_gender should be non-significant

cat("\n--- Randomization check: name-identity random effect ---\n")
m_no_name   <- lmer(rating ~ context * frequency + (1 + frequency | id) + (1 | subq),
                    data = df_long, REML = FALSE,
                    control = lmerControl(optimizer = "bobyqa"))
m_with_name <- lmer(rating ~ context * frequency + (1 + frequency | id) + (1 | subq) + (1 | name_num),
                    data = df_long, REML = FALSE,
                    control = lmerControl(optimizer = "bobyqa"))
print(anova(m_no_name, m_with_name))

# ==============================================================================
# FIGURES
# ==============================================================================
# install.packages("patchwork")
library(patchwork)
library(emmeans)
library(broom.mixed)
library(ggplot2)

# Figure 1: Primary result: context x frequency interaction ====================
emm1 <- emmeans(m_context, ~ context * frequency) |> as.data.frame()

fig1 <- ggplot(emm1, aes(x = frequency, y = emmean, color = context, group = context)) +
  geom_line(position = position_dodge(width = 0.15), linewidth = 0.8) +
  geom_point(position = position_dodge(width = 0.15), size = 3) +
  geom_errorbar(aes(ymin = asymp.LCL, ymax = asymp.UCL),
                width = 0.1, position = position_dodge(width = 0.15)) +
  scale_x_discrete(limits = c("low", "med", "high"),
                   labels = c("Low", "Medium", "High")) +
  scale_color_manual(values = c(social = "#1b9e77", work = "#d95f02"),
                     labels = c("Social", "Work")) +
  labs(x = "Scatterbrainedness level", y = "Predicted likeability rating",
       color = "Context",
       title = "Likeability by scatterbrainedness level and context") +
  theme_minimal(base_size = 13) +
  theme(legend.position = "bottom")

fig1

# ==== Figure 2: Secondary result: moderation by CFQ and ATS ===================
cfq_sd <- sd(person_scores$CFQ_mean, na.rm = TRUE)
ats_sd <- sd(person_scores$ATS_attitude, na.rm = TRUE)

emm_cfq <- emmeans(m_cfq, ~ context * frequency * CFQ_c,
                   at = list(CFQ_c = c(-cfq_sd, cfq_sd))) |>
  as.data.frame() |>
  mutate(moderator = "Own scatterbrainedness (CFQ)",
         level = factor(CFQ_c, labels = c("-1 SD", "+1 SD")))

emm_ats <- emmeans(m_ats, ~ context * frequency * ATS_c,
                   at = list(ATS_c = c(-ats_sd, ats_sd))) |>
  as.data.frame() |>
  mutate(moderator = "Attitude (ATS)",
         level = factor(ATS_c, labels = c("-1 SD", "+1 SD")))

emm_combined <- bind_rows(
  emm_cfq |> select(context, frequency, level, moderator, emmean, asymp.LCL, asymp.UCL),
  emm_ats |> select(context, frequency, level, moderator, emmean, asymp.LCL, asymp.UCL)
)

fig2 <- ggplot(emm_combined, aes(x = frequency, y = emmean, color = level, group = level)) +
  geom_line(position = position_dodge(width = 0.15), linewidth = 0.8) +
  geom_point(position = position_dodge(width = 0.15), size = 2.5) +
  geom_errorbar(aes(ymin = asymp.LCL, ymax = asymp.UCL),
                width = 0.1, position = position_dodge(width = 0.15)) +
  facet_grid(moderator ~ context,
             labeller = labeller(context = c(social = "Social", work = "Work"))) +
  scale_x_discrete(limits = c("low", "med", "high"), labels = c("Low", "Medium", "High")) +
  scale_color_manual(values = c("-1 SD" = "#7570b3", "+1 SD" = "#e7298a")) +
  labs(x = "Scatterbrainedness level", y = "Predicted likeability rating",
       color = "Moderator level",
       title = "Moderation of the frequency effect by own scatterbrainedness and attitude") +
  theme_minimal(base_size = 12) +
  theme(legend.position = "bottom")

fig2

# ==============================================================================
# THESIS TABLES AND FIGURES (APA)
# ==============================================================================
# install.packages("gt")
library(gt)

THESIS_TABLE_WIDTH <- "16cm"

apa_table <- function(gt_tbl, title = NULL, width = THESIS_TABLE_WIDTH) {
  has_spanners <- tryCatch(nrow(gt_tbl[["_spanners"]]) > 0, error = function(e) FALSE)
  
  if (isTRUE(has_spanners)) {
    bh   <- gt_tbl[["_boxhead"]]
    vars <- bh$var[bh$type %in% c("default", "stub")]
    
    if (length(vars) > 0) {
      w <- 100 / length(vars)
      widths <- lapply(vars, function(v) {
        rlang::new_formula(rlang::sym(v), rlang::expr(gt::pct(!!w)))
      })
      gt_tbl <- do.call(gt::cols_width, c(list(gt_tbl), widths))
    }
  }
  
  out <- gt_tbl |>
    tab_options(
      table.width = width,
      table.layout = "fixed",
      table.border.top.style = "solid", table.border.top.width = px(1.5),
      table.border.top.color = "black",
      table.border.bottom.style = "solid", table.border.bottom.width = px(1.5),
      table.border.bottom.color = "black",
      column_labels.border.bottom.style = "solid",
      column_labels.border.bottom.width = px(1),
      column_labels.border.bottom.color = "black",
      column_labels.border.top.style = "none",
      table_body.hlines.color = "white",
      table_body.border.bottom.style = "none",
      table.font.size = px(12),
      heading.align = "left",
      column_labels.font.weight = "bold",
      data_row.padding.horizontal = px(18),
      column_labels.padding.horizontal = px(18)
    )
  
  if (!is.null(title)) out <- out |> tab_header(title = title)
  out
}

# ==== Table 1: Sample demographics ============================================
gender_wide <- gender_summary |> transmute(`Gender` = Category, `n (%)` = Value)
education_wide <- education_summary |> transmute(`Education` = Category, `n (%) ` = Value)

max_rows <- max(nrow(gender_wide), nrow(education_wide))
pad_rows <- function(df, n) {
  if (nrow(df) < n) df <- bind_rows(df, tibble(!!!setNames(as.list(rep(NA_character_, ncol(df))), names(df)))[rep(1, n - nrow(df)), ])
  df
}
demographics_wide <- bind_cols(pad_rows(gender_wide, max_rows), pad_rows(education_wide, max_rows))

table1 <- demographics_wide |>
  gt() |>
  sub_missing(missing_text = "") |>
  apa_table() |>
  tab_header(
    title = "Table 1",
    subtitle = md("*Observed Gender and Education Distribution in the Primary Sample*")
  )
table1

# ==== Figure 1 (thesis version) ===============================================
fig1_thesis <- ggplot(emm1, aes(x = frequency, y = emmean, color = context, group = context)) +
  geom_line(position = position_dodge(width = 0.15), linewidth = 0.8) +
  geom_point(position = position_dodge(width = 0.15), size = 3) +
  geom_errorbar(aes(ymin = asymp.LCL, ymax = asymp.UCL),
                width = 0.1, position = position_dodge(width = 0.15)) +
  scale_x_discrete(limits = c("low", "med", "high"), labels = c("Low", "Medium", "High")) +
  scale_color_manual(values = c(social = "#1b9e77", work = "#d95f02"),
                     labels = c("Social", "Work")) +
  labs(title = "Figure 1", x = "Scatterbrainedness level", y = "Predicted likeability rating", color = "Context") +
  theme_minimal(base_size = 13) +
  theme(legend.position = "bottom", panel.grid.minor = element_blank())
fig1_thesis

# ==== Table 3: Primary model fixed effects AND random effects =================
primary_fixed <- broom.mixed::tidy(m_context, effects = "fixed") |>
  mutate(term = c("Intercept", "Context (Work)", "Frequency (Medium)",
                  "Frequency (High)", "Context \u00d7 Freq (Medium)",
                  "Context \u00d7 Freq (High)"),
         b  = sprintf("%.3f", estimate),
         SE = sprintf("%.3f", std.error),
         t  = sprintf("%.2f", statistic),
         p  = fmt_p(p.value)) |>
  select(Term = term, b, SE, t, p)

varcorr_df <- as.data.frame(VarCorr(m_context)) |>
  filter(is.na(var2)) |>
  transmute(Group = grp, Term = ifelse(is.na(var1), "Intercept", var1),
            Variance = sprintf("%.3f", vcov), SD = sprintf("%.3f", sdcor)) |>
  transmute(Term2 = paste0(Group, ": ", Term), Variance, SD)

n_rows <- max(nrow(primary_fixed), nrow(varcorr_df))
pad_panel <- function(df, n) {
  if (nrow(df) < n) {
    filler <- as_tibble(setNames(as.list(rep(NA_character_, ncol(df))), names(df)))[rep(1, n - nrow(df)), ]
    df <- bind_rows(df, filler)
  }
  df
}
table3 <- bind_cols(pad_panel(primary_fixed, n_rows), pad_panel(varcorr_df, n_rows)) |>
  gt() |>
  sub_missing(missing_text = "") |>
  tab_spanner(label = "Fixed effects", columns = c(Term, b, SE, t, p)) |>
  tab_spanner(label = "Random effects", columns = c(Term2, Variance, SD)) |>
  cols_label(Term2 = "Term") |>
  apa_table() |>
  tab_header(
    title = "Table 3",
    subtitle = md("*LMM estimates for fixed and random effects - 2-way interaction*")) |>
  tab_style(style = cell_text(weight = "bold"),
            locations = cells_body(columns = c(Term, b, SE, t, p),
                                   rows = Term %in% c("Context \u00d7 Freq (Medium)",
                                                      "Context \u00d7 Freq (High)")))
table3

# ==== Table 4: Simple effects (context within frequency) ======================
simple_effects_df <- as.data.frame(pairs(emm_context_by_freq, adjust = "none")) |>
  left_join(as.data.frame(confint(pairs(emm_context_by_freq, adjust = "none"))) |>
              select(frequency, asymp.LCL, asymp.UCL), by = "frequency") |>
  mutate(Frequency = str_to_title(as.character(frequency)),
         Estimate = sprintf("%.3f", estimate),
         SE = sprintf("%.3f", SE),
         z = sprintf("%.2f", z.ratio),
         p = fmt_p(p.value),
         CI = paste0("[", sprintf("%.3f", asymp.LCL), ", ", sprintf("%.3f", asymp.UCL), "]")) |>
  select(Frequency, Estimate, SE, z, p, CI)

table4 <- simple_effects_df |>
  pivot_longer(-Frequency, names_to = "Statistic", values_to = "value") |>
  pivot_wider(names_from = Frequency, values_from = value) |>
  gt() |>
  apa_table() |>
  tab_header(
    title = "Table 4",
    subtitle = md("*Decomposition of frequency and context interaction effect (using estimated marginal means)*"))
table4

# ==== Table 5: Secondary model coefficients (CFQ and ATS side by side) ========
cfq_terms <- broom.mixed::tidy(m_cfq, effects = "fixed") |>
  filter(str_detect(term, "CFQ_c")) |>
  transmute(term_clean = term, cfq_b = estimate, cfq_p = p.value)
ats_terms <- broom.mixed::tidy(m_ats, effects = "fixed") |>
  filter(str_detect(term, "ATS_c")) |>
  transmute(term_clean = sub("ATS_c", "CFQ_c", term), ats_b = estimate, ats_p = p.value)

term_labels <- tibble(
  term_clean = c("CFQ_c", "contextwork:CFQ_c", "frequencymed:CFQ_c",
                 "frequencyhigh:CFQ_c", "contextwork:frequencymed:CFQ_c",
                 "contextwork:frequencyhigh:CFQ_c"),
  term_label = c("Moderator (main effect)", "Context \u00d7 Moderator",
                 "Frequency (Medium) \u00d7 Moderator", "Frequency (High) \u00d7 Moderator",
                 "Context \u00d7 Freq (Medium) \u00d7 Moderator",
                 "Context \u00d7 Freq (High) \u00d7 Moderator")
)

table5_df <- term_labels |>
  left_join(cfq_terms, by = "term_clean") |>
  left_join(ats_terms, by = "term_clean") |>
  transmute(Term = term_label,
            `CFQ b` = sprintf("%.3f", cfq_b), `CFQ p` = fmt_p(cfq_p),
            `ATS b` = sprintf("%.3f", ats_b), `ATS p` = fmt_p(ats_p))

table5 <- table5_df |>
  gt() |>
  tab_spanner(label = "CFQ model", columns = c(`CFQ b`, `CFQ p`)) |>
  tab_spanner(label = "ATS model", columns = c(`ATS b`, `ATS p`)) |>
  apa_table() |>
  tab_header(
    title = "Table 5",
    subtitle = md("*Extensions of the primary model via CFQ and ATS. Either model was only extended by one factor at a time.*"))
table5

# ==== Table 6: Model quality metrics ==========================================
table6 <- tibble(
  r2_marginal    = sprintf("%.3f", r2_context$R2_marginal),
  r2_conditional = sprintf("%.3f", r2_context$R2_conditional),
  f2_interaction = sprintf("%.3f", f2_interaction)
) |>
  gt() |>
  cols_label(
    r2_marginal    = "Marginal R\u00b2",
    r2_conditional = "Conditional R\u00b2",
    f2_interaction = "Local f\u00b2 (interaction)"
  ) |>
  apa_table() |>
  tab_header(
    title = "Table 6",
    subtitle = md("*Variance explained by the primary model*"))
table6