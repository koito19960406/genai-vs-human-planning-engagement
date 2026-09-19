# =============================================================
# 6_change_in_interest.R
# Analysis of change in interest for GenAI experiment
# - Modularized, robust, and well-documented version
# - Uses variable names and mappings from main branch
# =============================================================

# ----------------------
# 0. Load Packages & Setup
# ----------------------
pacman::p_load(
  dplyr, tidyr, readr, readxl, ggplot2, broom, car, purrr, stargazer, boot, forcats, stringr, corrplot, viridis
)

source("genai_experiment/modeling/theme_beautiful.R")

# The inference standard adopted in issue #45, shared with 4_regression_analysis.R.
# Until now the four models below were plain lm with no bootstrap at all, so
# section 4.4 ran a different inference standard from section 4.3 without saying so.
source("genai_experiment/modeling/bootstrap_inference.R")

# Configurable paths
processed_data_path <- "data/processed/survey_processed.csv"
model_dir <- "reports/models/change_in_interest/"
fig_dir <- "reports/figures/change_in_interest/"

# Create directories if needed
for (dir in c(model_dir, fig_dir)) {
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)
}

# ----------------------
# 1. Data Loading & Validation
# ----------------------
# Helper: Check required columns
check_columns <- function(df, required_cols) {
  missing <- setdiff(required_cols, names(df))
  if (length(missing) > 0) {
    stop(paste("Missing columns:", paste(missing, collapse=", ")))
  }
}

# Load data
if (!file.exists(processed_data_path)) stop("Processed data not found!")
df <- read_csv(processed_data_path)

# List of all variables used in this script
required_vars <- c(
  "Civic_Engagement_Change_num", "Walking_Interest_Change_num", "Cycling_Interest_Change_num", "Place_Attachment_Change_num",
  "Gender", "Race_Ethnicity", "Walking_Frequency_num", "Cycling_Frequency_num", "Planning_Knowledge_num", "AI_Experience_num", "AI_Tool_Usage_num", "genai_first", "street_first", "Primary_Transportation", "Hometown",
  "Engagement_Public_Meetings", "Engagement_Online_Surveys", "Engagement_Traditional_Workshops", "Engagement_AI_Workshops", "Engagement_Voting", "Engagement_Social_Media"
)
check_columns(df, required_vars)

# Set Race_Ethnicity reference level to white
df <- df %>%
  mutate(Race_Ethnicity = fct_relevel(factor(Race_Ethnicity), "White"))

# ----------------------
# 2. Descriptive Statistics and Distributions
# ----------------------
interest_summary <- df %>%
  summarise(
    civic_mean = mean(Civic_Engagement_Change_num, na.rm = TRUE),
    civic_sd = sd(Civic_Engagement_Change_num, na.rm = TRUE),
    civic_median = median(Civic_Engagement_Change_num, na.rm = TRUE),
    walking_mean = mean(Walking_Interest_Change_num, na.rm = TRUE),
    walking_sd = sd(Walking_Interest_Change_num, na.rm = TRUE),
    walking_median = median(Walking_Interest_Change_num, na.rm = TRUE),
    cycling_mean = mean(Cycling_Interest_Change_num, na.rm = TRUE),
    cycling_sd = sd(Cycling_Interest_Change_num, na.rm = TRUE),
    cycling_median = median(Cycling_Interest_Change_num, na.rm = TRUE),
    attachment_mean = mean(Place_Attachment_Change_num, na.rm = TRUE),
    attachment_sd = sd(Place_Attachment_Change_num, na.rm = TRUE),
    attachment_median = median(Place_Attachment_Change_num, na.rm = TRUE)
  )
write_excel_csv(interest_summary, paste0(model_dir, "interest_summary_stats.csv"))

interest_long <- df %>%
  select(ResponseId, Civic_Engagement_Change_num, Walking_Interest_Change_num, 
         Cycling_Interest_Change_num, Place_Attachment_Change_num) %>%
  pivot_longer(
    cols = c(Civic_Engagement_Change_num, Walking_Interest_Change_num, 
             Cycling_Interest_Change_num, Place_Attachment_Change_num),
    names_to = "interest_type",
    values_to = "change"
  ) %>%
  mutate(
    interest_type = case_when(
      interest_type == "Civic_Engagement_Change_num" ~ "Civic Engagement",
      interest_type == "Walking_Interest_Change_num" ~ "Walking Interest",
      interest_type == "Cycling_Interest_Change_num" ~ "Cycling Interest",
      interest_type == "Place_Attachment_Change_num" ~ "Place Attachment",
      TRUE ~ interest_type
    ),
    interest_type = factor(interest_type, 
                          levels = c("Civic Engagement", "Walking Interest", 
                                    "Cycling Interest", "Place Attachment"))
  )

# Beautified bar plot for distribution (legend removed)
p1 <- ggplot(interest_long, aes(x = change, fill = interest_type)) +
  geom_bar(position = "dodge", color = "black", alpha = 0.85) +
  scale_x_continuous(breaks = c(-2, -1, 0, 1, 2),
                    labels = c("Very\nNegative", "Somewhat\nNegative", 
                              "No Change", "Somewhat\nPositive", "Very\nPositive")) +
  scale_fill_manual(values = c("Civic Engagement" = "#5B859EFF", "Walking Interest" = "#75884BFF", "Cycling Interest" = "#AB84A5FF", "Place Attachment" = "#D8B847FF")) +
  labs(
    title = "Distribution of Interest Changes After the Experiment",
    x = "Change in Interest/Connection",
    y = "Number of Participants"
  ) +
  theme_beautiful() +
  theme(
    axis.text.x = element_text(size = 14, face = "bold"),
    legend.position = "none"
  ) +  # Remove legend
  facet_wrap(~ interest_type, ncol = 2)
ggsave(paste0(fig_dir, "interest_change_distribution.png"), p1, width = 12, height = 10, dpi = 300)

# save interest_long
write_excel_csv(interest_long, paste0(model_dir, "interest_change_long_format.csv"))
# ----------------------
# 3. T-tests: Testing if changes are significantly different from 0
# ----------------------
civic_ttest <- t.test(df$Civic_Engagement_Change_num)
walking_ttest <- t.test(df$Walking_Interest_Change_num)
cycling_ttest <- t.test(df$Cycling_Interest_Change_num)
attachment_ttest <- t.test(df$Place_Attachment_Change_num)

ttest_results <- bind_rows(
  broom::tidy(civic_ttest) %>% mutate(variable = "Civic Engagement"),
  broom::tidy(walking_ttest) %>% mutate(variable = "Walking Interest"),
  broom::tidy(cycling_ttest) %>% mutate(variable = "Cycling Interest"),
  broom::tidy(attachment_ttest) %>% mutate(variable = "Place Attachment")
) %>%
  mutate(
    significant = p.value < 0.05,
    direction = case_when(
      estimate > 0 & significant ~ "Significant Positive Change",
      estimate < 0 & significant ~ "Significant Negative Change",
      TRUE ~ "No Significant Change"
    )
  )
write_excel_csv(ttest_results, paste0(model_dir, "interest_ttests.csv"))

p2 <- ggplot(ttest_results, aes(x = variable, y = estimate, fill = direction)) +
  geom_col(color = "black", alpha = 0.85) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.2) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  scale_fill_manual(values = c("Significant Positive Change" = viridis(5)[4], 
                               "No Significant Change" = "gray",
                               "Significant Negative Change" = viridis(5)[2])) +
  labs(
    title = "T-Test Results for Changes in Interest and Place Attachment",
    subtitle = "Testing if the mean change is significantly different from 0",
    x = "",
    y = "Mean Change (95% CI)",
    fill = "Result"
  ) +
  theme_beautiful()
ggsave(paste0(fig_dir, "interest_ttest_results.png"), p2, width = 12, height = 7)

# ----------------------
# 4. Regression Analysis: Factors affecting changes in interest
# ----------------------
civic_model <- lm(Civic_Engagement_Change_num ~ Gender + Race_Ethnicity + Planning_Knowledge_num + AI_Experience_num + AI_Tool_Usage_num + genai_first + street_first + Primary_Transportation + Hometown, data = df)
walking_model <- lm(Walking_Interest_Change_num ~ Gender + Race_Ethnicity + Planning_Knowledge_num + AI_Experience_num + AI_Tool_Usage_num + genai_first + street_first + Primary_Transportation + Hometown, data = df)
cycling_model <- lm(Cycling_Interest_Change_num ~ Gender + Race_Ethnicity + Planning_Knowledge_num + AI_Experience_num + AI_Tool_Usage_num + genai_first + street_first + Primary_Transportation + Hometown, data = df)
attachment_model <- lm(Place_Attachment_Change_num ~ Gender + Race_Ethnicity + Planning_Knowledge_num + AI_Experience_num + AI_Tool_Usage_num + genai_first + street_first + Primary_Transportation + Hometown, data = df)

stargazer(
  civic_model, walking_model, cycling_model, attachment_model,
  title = "Regression Models of Change in Interest and Place Attachment",
  align = TRUE,
  type = "latex",
  dep.var.labels = c("Civic Engagement", "Walking Interest", "Cycling Interest", "Place Attachment"),
  model.numbers = FALSE,
  font.size = "footnotesize",
  table.placement = "H",
  header = FALSE,
  no.space = TRUE,
  column.sep.width = "3pt",
  star.cutoffs = c(0.1, 0.05, 0.01, 0.001),
  star.char = c("†", "*", "**", "***"),
  notes = "$^{†}$p$<$0.1; $^{*}$p$<$0.05; $^{**}$p$<$0.01; $^{***}$p$<$0.001",
  out = paste0(model_dir, "interest_regression_models.tex")
)

# ----------------------
# 4.1. Bootstrap inference and the two tables the manuscript reports
# ----------------------
# main.tex inputs models/change_in_interest/interest_regression_models_updated.tex,
# which no script produced: it was a hand-built restyling of the stargazer output
# above, so its stars were parametric OLS p-values. Issue #45 made the bootstrap
# the standard in BOTH regression tables; this block writes the main-text table
# and its supplementary companion, mirroring 4_regression_analysis.R exactly.
interest_model_specs <- list(
  "Civic Engagement" = formula(civic_model),
  "Walking Interest" = formula(walking_model),
  "Cycling Interest" = formula(cycling_model),
  "Place Attachment" = formula(attachment_model)
)

set.seed(BOOT_SEED)
interest_boot <- purrr::imap_dfr(interest_model_specs, function(f, nm) {
  boot_lm(f, df) %>% mutate(outcome = nm, .before = 1)
})
write_csv(interest_boot, paste0(model_dir, "interest_bootstrap_results.csv"))

# Row order preserved from the submitted table so the latexdiff stays readable.
INTEREST_TERM_ORDER <- c(
  "GenderWoman",
  "Race_EthnicityAnother race or ethnicity not listed",
  "Race_EthnicityAsian",
  "Race_EthnicityBlack or African American",
  "Race_EthnicityHispanic or Latino/a/x",
  "Planning_Knowledge_num",
  "AI_Experience_num",
  "AI_Tool_Usage_num",
  "genai_firstTRUE",
  "street_firstTRUE",
  "Primary_TransportationPersonal car",
  "Primary_TransportationRideshare (Uber/Lyft)",
  "Primary_TransportationWalking",
  "HometownSuburban",
  "HometownUrban",
  "(Intercept)"
)
# The submitted table calls the intercept "Constant" and puts it last; kept.
interest_label <- function(x) ifelse(x == "(Intercept)", "Constant", label_term(x))

interest_outcomes <- names(interest_model_specs)
interest_notes <- paste(note_inference(), note_hometown(df), note_cell_sizes(df))

# --- Main text: tab:interest_regression ---
wide <- interest_boot %>%
  mutate(
    cell = fmt_cell(estimate, significant, stable),
    row_label = interest_label(term),
    outcome = factor(outcome, levels = interest_outcomes)
  ) %>%
  select(term, row_label, outcome, cell) %>%
  arrange(match(term, INTEREST_TERM_ORDER)) %>%
  select(-term) %>%
  pivot_wider(names_from = outcome, values_from = cell, values_fill = "")

fit_footer <- interest_boot %>%
  group_by(outcome) %>%
  slice(1) %>%
  ungroup() %>%
  mutate(outcome = factor(outcome, levels = interest_outcomes)) %>%
  arrange(outcome)

writeLines(c(
  "\\begin{sidewaystable}[htbp]",
  "\\centering",
  "\\caption{Regression Models of Change in Interest and Place Attachment}",
  "\\label{tab:interest_regression}",
  "\\footnotesize",
  paste0("\\begin{tabularx}{\\textwidth}{l*{", length(interest_outcomes), "}{X}}"),
  "\\toprule",
  paste0(paste(c(" ", interest_outcomes), collapse = " & "), " \\\\"),
  "\\midrule",
  apply(as.matrix(wide), 1, function(r) paste0(paste(r, collapse = " & "), " \\\\")),
  "\\midrule",
  paste0("Observations & ", paste(fit_footer$n_obs, collapse = " & "), " \\\\"),
  paste0("R$^{2}$ & ", paste(formatC(fit_footer$r_squared, format = "f", digits = 3), collapse = " & "), " \\\\"),
  paste0("Adjusted R$^{2}$ & ", paste(formatC(fit_footer$adj_r_squared, format = "f", digits = 3), collapse = " & "), " \\\\"),
  "\\bottomrule",
  "\\end{tabularx}",
  "\\begin{minipage}{\\textwidth}",
  "\\footnotesize",
  paste0("\\textit{Notes:} ", interest_notes),
  "\\end{minipage}",
  "\\end{sidewaystable}"
), paste0(model_dir, "interest_regression_models_updated.tex"))
cat("Saved main-text interest table: interest_regression_models_updated.tex\n")

# --- Supplementary: full intervals and per-term dropout ---
supp <- interest_boot %>%
  mutate(
    outcome = factor(outcome, levels = interest_outcomes),
    row_label = interest_label(term),
    est = paste0(formatC(estimate, format = "f", digits = 3), ifelse(significant, "*", "")),
    ci = ifelse(stable,
                sprintf("[%s, %s]",
                        formatC(boot_ci_lower, format = "f", digits = 3),
                        formatC(boot_ci_upper, format = "f", digits = 3)),
                "refused"),
    drop = sprintf("%.1f\\%%", 100 * dropout)
  ) %>%
  arrange(outcome, match(term, INTEREST_TERM_ORDER))

supp_body <- unlist(lapply(interest_outcomes, function(o) {
  rows <- supp %>% filter(outcome == o)
  c(paste0("\\multicolumn{4}{l}{\\textit{", o, "}} \\\\"),
    apply(as.matrix(rows %>% select(row_label, est, ci, drop)), 1,
          function(r) paste0("\\quad ", paste(r, collapse = " & "), " \\\\")),
    "\\addlinespace")
}))

writeLines(c(
  "\\footnotesize",
  "\\begin{longtable}{lrrr}",
  "\\caption{Bootstrap regression coefficients with full 95\\% confidence intervals and per-term dropout: change in interest and place attachment}",
  "\\label{tab:interest_ci_supp} \\\\",
  "\\toprule",
  "Variable & Estimate & 95\\% CI & Dropout \\\\",
  "\\midrule",
  "\\endfirsthead",
  "\\toprule",
  "Variable & Estimate & 95\\% CI & Dropout \\\\",
  "\\midrule",
  "\\endhead",
  supp_body,
  "\\bottomrule",
  "\\end{longtable}",
  "\\begin{minipage}{\\textwidth}",
  "\\footnotesize",
  paste0("\\textit{Notes:} ", interest_notes,
         " An interval is shown as \\emph{refused} where dropout reaches 1\\%."),
  "\\end{minipage}",
  "\\normalsize"
), paste0(model_dir, "interest_regression_ci_supp.tex"))
cat("Saved supplementary interest CI table: interest_regression_ci_supp.tex\n")

extract_model_results <- function(model, outcome_name) {
  broom::tidy(model) %>%
    mutate(
      outcome = outcome_name,
      significant = p.value < 0.05,
      stars = case_when(
        p.value < 0.001 ~ "***",
        p.value < 0.01 ~ "**",
        p.value < 0.05 ~ "*",
        TRUE ~ ""
      )
    )
}

all_models <- bind_rows(
  extract_model_results(civic_model, "Civic Engagement"),
  extract_model_results(walking_model, "Walking Interest"),
  extract_model_results(cycling_model, "Cycling Interest"),
  extract_model_results(attachment_model, "Place Attachment")
)
write_excel_csv(all_models, paste0(model_dir, "interest_regression_results.csv"))

key_predictors <- c("genai_firstTRUE", "street_firstTRUE", "AI_Experience_num", "Planning_Knowledge_num", "AI_Tool_Usage_num", "GenderWoman", "Primary_TransportationWalk", "Primary_TransportationBike", "HometownRural", "HometownSuburban")
key_results <- all_models %>%
  filter(term %in% key_predictors) %>%
  mutate(
    term = case_when(
      term == "genai_firstTRUE" ~ "GenAI First",
      term == "street_firstTRUE" ~ "Street First",
      term == "AI_Experience_num" ~ "AI Familiarity",
      term == "Planning_Knowledge_num" ~ "Planning Knowledge",
      term == "AI_Tool_Usage_num" ~ "AI Image-Tool Use",
      term == "GenderWoman" ~ "Gender: Woman",
      term == "Primary_TransportationWalk" ~ "Transport: Walk",
      term == "Primary_TransportationBike" ~ "Transport: Bike",
      term == "HometownRural" ~ "Hometown: Rural",
      term == "HometownSuburban" ~ "Hometown: Suburban",
      TRUE ~ term
    )
  )
p3 <- ggplot(key_results, aes(x = term, y = estimate, fill = significant)) +
  geom_col(color = "black", alpha = 0.85) +
  geom_errorbar(aes(ymin = estimate - std.error, ymax = estimate + std.error), width = 0.2) +
  facet_wrap(~ outcome, ncol = 2) +
  scale_fill_manual(values = c("TRUE" = viridis(5)[4], "FALSE" = "gray")) +
  labs(
    title = "Key Predictors of Changes in Interest and Place Attachment",
    x = "Predictor",
    y = "Coefficient Estimate (with SE)",
    fill = "Significant (p<0.05)"
  ) +
  theme_beautiful()
ggsave(paste0(fig_dir, "interest_key_predictors.png"), p3, width = 12, height = 8)

# ----------------------
# 5. Correlations Between Interest Changes
# ----------------------
interest_cors <- df %>%
  select(Civic_Engagement_Change_num, Walking_Interest_Change_num, Cycling_Interest_Change_num, Place_Attachment_Change_num) %>%
  cor(use = "pairwise.complete.obs")
write.csv(interest_cors, paste0(model_dir, "interest_correlations.csv"))
png(paste0(fig_dir, "interest_correlation_plot.png"), width = 900, height = 700)
corrplot(interest_cors, method = "color", type = "upper", tl.col = "black", tl.srt = 45, addCoef.col = "black", diag = FALSE, mar = c(0,0,2,0), title = "Correlations Between Interest Changes", col = colorRampPalette(c("#6D9EC1", "white", "#E46726"))(200))
dev.off()

# ----------------------
# 6. Analysis of Engagement Interest Types (QID146)
# Only use binary indicator columns for engagement types
engagement_cols <- c(
  "Engagement_Public_Meetings", "Engagement_Online_Surveys", "Engagement_Traditional_Workshops",
  "Engagement_AI_Workshops", "Engagement_Voting", "Engagement_Social_Media"
)
engagement_data <- df %>%
  select(all_of(engagement_cols)) %>%
  summarise(across(everything(), ~sum(.x, na.rm = TRUE))) %>%
  pivot_longer(cols = everything(), names_to = "engagement_type", values_to = "count") %>%
  mutate(
    engagement_type = case_when(
      engagement_type == "Engagement_Public_Meetings" ~ "Public meetings/hearings",
      engagement_type == "Engagement_Online_Surveys" ~ "Online surveys/feedback",
      engagement_type == "Engagement_Traditional_Workshops" ~ "Design workshops (traditional)",
      engagement_type == "Engagement_AI_Workshops" ~ "Design workshops (AI tools)",
      engagement_type == "Engagement_Voting" ~ "Voting on proposed designs",
      engagement_type == "Engagement_Social_Media" ~ "Social media discussions",
      TRUE ~ engagement_type
    ),
    percentage = count / nrow(df) * 100
  ) %>%
  arrange(desc(count)) %>%
  mutate(engagement_type = factor(engagement_type, levels = engagement_type))
p4 <- ggplot(engagement_data, aes(x = engagement_type, y = percentage)) +
  geom_col(color = "black", alpha = 0.85, fill = "#75884BFF") +
  coord_flip() +
  labs(
    title = "Preferred Types of Urban Planning Engagement",
    x = "",
    y = "Percentage of Participants (%)",
    caption = "Participants could select multiple activities"
  ) +
  theme_beautiful() +
  theme(legend.position = "none")
ggsave(paste0(fig_dir, "preferred_engagement_types.png"), p4, width = 12, height = 8, dpi = 300)

# ----------------------
# 7. Comparison by Experimental Conditions
# ----------------------
condition_comparison <- df %>%
  group_by(genai_first) %>%
  summarise(
    civic_mean = mean(Civic_Engagement_Change_num, na.rm = TRUE),
    walking_mean = mean(Walking_Interest_Change_num, na.rm = TRUE),
    cycling_mean = mean(Cycling_Interest_Change_num, na.rm = TRUE),
    attachment_mean = mean(Place_Attachment_Change_num, na.rm = TRUE)
  ) %>%
  mutate(condition = if_else(genai_first, "GenAI First", "Conventional First")) %>%
  select(-genai_first) %>%
  pivot_longer(
    cols = ends_with("_mean"),
    names_to = "interest_type",
    values_to = "mean_change"
  ) %>%
  mutate(
    interest_type = case_when(
      interest_type == "civic_mean" ~ "Civic Engagement",
      interest_type == "walking_mean" ~ "Walking Interest",
      interest_type == "cycling_mean" ~ "Cycling Interest",
      interest_type == "attachment_mean" ~ "Place Attachment",
      TRUE ~ interest_type
    )
  )
p5 <- ggplot(condition_comparison, aes(x = interest_type, y = mean_change, fill = condition)) +
  geom_col(position = "dodge", color = "black", alpha = 0.85) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  scale_fill_viridis_d(option = "A", end = 0.8, direction = -1) +
  labs(
    title = "Change in Interest by Experimental Condition",
    x = "",
    y = "Mean Change in Interest/Connection",
    fill = "Experimental Condition"
  ) +
  theme_beautiful() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "bottom")
ggsave(paste0(fig_dir, "interest_change_by_condition.png"), p5, width = 12, height = 7)

condition_ttests <- df %>%
  select(genai_first, Civic_Engagement_Change_num, Walking_Interest_Change_num, Cycling_Interest_Change_num, Place_Attachment_Change_num) %>%
  pivot_longer(
    cols = ends_with("_num"),
    names_to = "interest_type",
    values_to = "change"
  ) %>%
  group_by(interest_type) %>%
  summarise(
    t_test = list(t.test(change ~ genai_first, var.equal = TRUE))
  ) %>%
  mutate(
    t_test_result = map(t_test, broom::tidy)
  ) %>%
  unnest(t_test_result) %>%
  select(-t_test) %>%
  mutate(
    interest_type = case_when(
      interest_type == "Civic_Engagement_Change_num" ~ "Civic Engagement",
      interest_type == "Walking_Interest_Change_num" ~ "Walking Interest",
      interest_type == "Cycling_Interest_Change_num" ~ "Cycling Interest",
      interest_type == "Place_Attachment_Change_num" ~ "Place Attachment",
      TRUE ~ interest_type
    ),
    significant = p.value < 0.05
  )
write_excel_csv(condition_ttests, paste0(model_dir, "condition_comparison_ttests.csv"))

cat("Analysis of changes in interest and engagement preferences completed.\n")
cat("Generated plots and data files have been saved to reports/figures/change_in_interest/ and reports/models/change_in_interest/\n")

# Save results
interest_change_results <- list(
  ttest_results = ttest_results,
  regression_results = all_models
)
write_excel_csv(ttest_results, "reports/models/change_in_interest/interest_ttest_results.xlsx")
write_excel_csv(all_models, "reports/models/change_in_interest/interest_regression_results.xlsx")
