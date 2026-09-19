# =============================================================
# 4_regression_analysis.R
# Main regression analysis for GenAI experiment
# - Modularized, robust, and well-documented version
# - Uses variable names and mappings from main branch
# =============================================================

# ----------------------
# 0. Load Packages & Setup
# ----------------------
pacman::p_load(
  dplyr,
  tidyr,
  readr,
  readxl,
  ggplot2,
  broom,
  car,
  purrr,
  stargazer,
  boot,
  forcats,
  stringr,
  corrplot,
  viridis
)

# The inference standard adopted in issue #45, shared with 6_change_in_interest.R
source("genai_experiment/modeling/bootstrap_inference.R")

# Configurable paths
processed_data_path <- "data/processed/survey_processed.csv"
summary_stats_path <- "reports/models/main_test/summary_statistics.csv"
regression_results_path <- "reports/models/main_test/regression_results.csv"
latex_dir <- "reports/models/main_test/individual/"
boot_results_path <- "reports/models/main_test/bootstrapped_regression_results.csv"
main_table_path <- "bootstrap_ai_conventional_updated.tex"   # the file main.tex inputs
supp_table_path <- "bootstrap_ai_conventional.tex"           # full intervals, supplementary
vif_results_path <- "reports/models/main_test/vif_results.csv"
coef_plot_path <- "reports/figures/main_test/bootstrapped_coefficient_plots.png"
coef_plot_pdf_path <- "reports/figures/main_test/bootstrapped_coefficient_plots.pdf"
exp_plot_path <- "reports/figures/main_test/bootstrapped_experimental_effects.png"
exp_plot_pdf_path <- "reports/figures/main_test/bootstrapped_experimental_effects.pdf"

# Create directories if needed
for (dir in c(dirname(summary_stats_path), latex_dir, dirname(coef_plot_path))) {
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

# List of all variables used in models
required_vars <- c(
  # Outcomes
  "walk_ai", "walk_conv", "cycle_ai", "cycle_conv", "plan_ai", "plan_conv", "attach_ai", "attach_conv",
  "walk_street", "walk_park", "cycle_street", "cycle_park", "plan_street", "plan_park", "attach_street", "attach_park",
  "walk_total_genai_first", "walk_total_conv_first", "cycle_total_genai_first", "cycle_total_conv_first",
  "plan_total_genai_first", "plan_total_conv_first", "attach_total_genai_first", "attach_total_conv_first",
  "walk_total_street_first", "walk_total_park_first", "cycle_total_street_first", "cycle_total_park_first",
  "plan_total_street_first", "plan_total_park_first", "attach_total_street_first", "attach_total_park_first",
  "walk_diff_total", "cycle_diff_total", "plan_diff_total", "attach_diff_total", "AI_or_Conventional_num",
  # Covariates
  "Gender", "Race_Ethnicity", "Walking_Frequency_num", "Cycling_Frequency_num", "Planning_Knowledge_num", "AI_Experience_num", "AI_Tool_Usage_num", "genai_first", "street_first", "Primary_Transportation", "Hometown"
)
check_columns(df, required_vars)

# Set Race_Ethnicity reference level to white
df <- df %>%
  mutate(Race_Ethnicity = fct_relevel(factor(Race_Ethnicity), "White"))

# ----------------------
# 2. Descriptive Statistics
# ----------------------
summary_df <- df %>%
  select(all_of(required_vars[1:32])) %>%
  summarise(across(everything(), list(
    mean = ~mean(., na.rm = TRUE),
    sd = ~sd(., na.rm = TRUE),
    n = ~sum(!is.na(.))
  )))
summary_long <- summary_df %>%
  pivot_longer(cols = everything(), 
               names_to = c("variable", ".value"), 
               names_pattern = "(.+)_(mean|sd|n)$")
write_csv(summary_long, summary_stats_path)

# ----------------------
# 3. Model Fitting (Modularized)
# ----------------------
# Helper: Safe model fitting
safe_lm <- function(formula, data) {
  tryCatch(lm(formula, data = data), error = function(e) NA)
}

# Model formulas
model_formulas <- list(
  walk_ai_model = walk_ai ~ Gender + Race_Ethnicity + Planning_Knowledge_num + AI_Experience_num + AI_Tool_Usage_num + genai_first + street_first + Primary_Transportation + Hometown,
  walk_conv_model = walk_conv ~ Gender + Race_Ethnicity + Planning_Knowledge_num + AI_Experience_num + AI_Tool_Usage_num + genai_first + street_first + Primary_Transportation + Hometown,
  cycle_ai_model = cycle_ai ~ Gender + Race_Ethnicity + Planning_Knowledge_num + AI_Experience_num + AI_Tool_Usage_num + genai_first + street_first + Primary_Transportation + Hometown,
  cycle_conv_model = cycle_conv ~ Gender + Race_Ethnicity + Planning_Knowledge_num + AI_Experience_num + AI_Tool_Usage_num + genai_first + street_first + Primary_Transportation + Hometown,
  plan_ai_model = plan_ai ~ Gender + Race_Ethnicity + Planning_Knowledge_num + AI_Experience_num + AI_Tool_Usage_num + genai_first + street_first + Primary_Transportation + Hometown,
  plan_conv_model = plan_conv ~ Gender + Race_Ethnicity + Planning_Knowledge_num + AI_Experience_num + AI_Tool_Usage_num + genai_first + street_first + Primary_Transportation + Hometown,
  attach_ai_model = attach_ai ~ Gender + Race_Ethnicity + Planning_Knowledge_num + AI_Experience_num + AI_Tool_Usage_num + genai_first + street_first + Primary_Transportation + Hometown,
  attach_conv_model = attach_conv ~ Gender + Race_Ethnicity + Planning_Knowledge_num + AI_Experience_num + AI_Tool_Usage_num + genai_first + street_first + Primary_Transportation + Hometown,
  walk_street_model = walk_street ~ Gender + Race_Ethnicity + Planning_Knowledge_num + AI_Experience_num + AI_Tool_Usage_num + genai_first + street_first + Primary_Transportation + Hometown,
  walk_park_model = walk_park ~ Gender + Race_Ethnicity + Planning_Knowledge_num + AI_Experience_num + AI_Tool_Usage_num + genai_first + street_first + Primary_Transportation + Hometown,
  cycle_street_model = cycle_street ~ Gender + Race_Ethnicity + Planning_Knowledge_num + AI_Experience_num + AI_Tool_Usage_num + genai_first + street_first + Primary_Transportation + Hometown,
  cycle_park_model = cycle_park ~ Gender + Race_Ethnicity + Planning_Knowledge_num + AI_Experience_num + AI_Tool_Usage_num + genai_first + street_first + Primary_Transportation + Hometown,
  plan_street_model = plan_street ~ Gender + Race_Ethnicity + Planning_Knowledge_num + AI_Experience_num + AI_Tool_Usage_num + genai_first + street_first + Primary_Transportation + Hometown,
  plan_park_model = plan_park ~ Gender + Race_Ethnicity + Planning_Knowledge_num + AI_Experience_num + AI_Tool_Usage_num + genai_first + street_first + Primary_Transportation + Hometown,
  attach_street_model = attach_street ~ Gender + Race_Ethnicity + Planning_Knowledge_num + AI_Experience_num + AI_Tool_Usage_num + genai_first + street_first + Primary_Transportation + Hometown,
  attach_park_model = attach_park ~ Gender + Race_Ethnicity + Planning_Knowledge_num + AI_Experience_num + AI_Tool_Usage_num + genai_first + street_first + Primary_Transportation + Hometown,
  walk_diff_total_model = walk_diff_total ~ Gender + Race_Ethnicity + Planning_Knowledge_num + AI_Experience_num + AI_Tool_Usage_num + genai_first + street_first + Primary_Transportation + Hometown,
  cycle_diff_total_model = cycle_diff_total ~ Gender + Race_Ethnicity + Planning_Knowledge_num + AI_Experience_num + AI_Tool_Usage_num + genai_first + street_first + Primary_Transportation + Hometown,
  plan_diff_total_model = plan_diff_total ~ Gender + Race_Ethnicity + Planning_Knowledge_num + AI_Experience_num + AI_Tool_Usage_num + genai_first + street_first + Primary_Transportation + Hometown,
  attach_diff_total_model = attach_diff_total ~ Gender + Race_Ethnicity + Planning_Knowledge_num + AI_Experience_num + AI_Tool_Usage_num + genai_first + street_first + Primary_Transportation + Hometown,
  ai_preference_model = AI_or_Conventional_num ~ Gender + Race_Ethnicity + Planning_Knowledge_num + AI_Experience_num + AI_Tool_Usage_num + genai_first + street_first + Primary_Transportation + Hometown
)

# Fit all models
all_models <- purrr::imap(model_formulas, ~safe_lm(.x, df))

# ----------------------
# 4. Model Diagnostics & Comparison
# ----------------------
# Helper: Model diagnostics summary
model_diagnostics <- function(model) {
  if (is.na(model)[1]) return(NA)
  list(
    AIC = AIC(model),
    BIC = BIC(model),
    adj_r2 = summary(model)$adj.r.squared,
    VIF = tryCatch(car::vif(model), error = function(e) NA)
  )
}

diagnostics_list <- purrr::imap(all_models, ~model_diagnostics(.x))
# Save diagnostics as a table
model_diag_df <- purrr::imap_dfr(diagnostics_list, function(x, n) {
  if (is.na(x)[1]) return(data.frame(model=n, AIC=NA, BIC=NA, adj_r2=NA))
  data.frame(model=n, AIC=x$AIC, BIC=x$BIC, adj_r2=x$adj_r2)
})
write_csv(model_diag_df, "reports/models/main_test/model_diagnostics.csv")

# Extract and save VIF values
vif_results_list <- purrr::imap(diagnostics_list, function(x, model_name) {
  if (is.na(x)[1] || is.na(x$VIF)[1]) {
    return(data.frame(model = model_name, term = NA_character_, vif = NA_real_))
  }
  vif_values <- x$VIF
  # Handle both named vector and matrix outputs from vif()
  if (is.matrix(vif_values)) {
    # If matrix, take the first column (usually GVIF)
    vif_vec <- vif_values[, 1]
  } else {
    vif_vec <- vif_values
  }
  data.frame(
    model = model_name,
    term = names(vif_vec),
    vif = as.numeric(vif_vec),
    stringsAsFactors = FALSE
  )
})

all_vif_results <- bind_rows(vif_results_list)
write_csv(all_vif_results, vif_results_path)

# ----------------------
# 5. Extract & Save Results
# ----------------------
# Helper: Extract regression results
format_regression_results <- function(model, model_name) {
  if (is.na(model)[1]) return(NULL)
  coefs <- tidy(model)
  r_squared <- glance(model)
  data.frame(
    model = model_name,
    term = coefs$term,
    estimate = coefs$estimate,
    std_error = coefs$std.error,
    t_value = coefs$statistic,
    p_value = coefs$p.value,
    r_squared = r_squared$r.squared,
    adj_r_squared = r_squared$adj.r.squared,
    f_statistic = r_squared$statistic,
    df = r_squared$df,
    df_residual = r_squared$df.residual,
    p_value_model = r_squared$p.value
  )
}
all_results <- purrr::imap_dfr(all_models, format_regression_results)
write_csv(all_results, regression_results_path)

# Save individual LaTeX tables (kept for reference)
for (i in seq_along(all_models)) {
  model <- all_models[[i]]
  name <- names(all_models)[i]
  if (!is.na(model)[1]) {
    stargazer(
      model,
      title = paste("Regression Results for", name),
      dep.var.labels = name,
      model.numbers = FALSE,
      align = TRUE,
      type = "latex",
      out = paste0(latex_dir, name, ".tex")
    )
  }
}

# ----------------------
# 5.1. Combined LaTeX Tables
# ----------------------

# Helper function to create combined tables
create_combined_table <- function(model_names, title, filename, column_labels = NULL) {
  # Filter models that exist and are not NA
  selected_models <- all_models[model_names]
  valid_models <- selected_models[!sapply(selected_models, function(x) is.na(x)[1])]
  
  if (length(valid_models) > 0) {
    # Create clean column labels if not provided
    if (is.null(column_labels)) {
      column_labels <- names(valid_models) %>%
        str_replace_all("_model$", "") %>%
        str_replace_all("_", " ") %>%
        str_to_title()
    }
    
    stargazer(
      valid_models,
      title = title,
      dep.var.labels = column_labels,
      column.labels = column_labels,
      model.numbers = TRUE,
      align = TRUE,
      type = "latex",
      font.size = "footnotesize",
      table.placement = "H",
      header = FALSE,
      no.space = TRUE,
      column.sep.width = "3pt",
      out = paste0("reports/models/main_test/", filename)
    )
    cat(paste("Saved combined table:", filename, "\n"))
  }
}

# 1. AI vs Conventional Models Table
ai_conv_models <- c("walk_ai_model", "walk_conv_model", "cycle_ai_model", "cycle_conv_model", 
                    "plan_ai_model", "plan_conv_model", "attach_ai_model", "attach_conv_model")
ai_conv_labels <- c("Walk AI", "Walk Conv", "Cycle AI", "Cycle Conv", 
                    "Plan AI", "Plan Conv", "Attach AI", "Attach Conv")

create_combined_table(
  ai_conv_models, 
  "Regression Results: AI vs Conventional Methods",
  "ai_conventional_models.tex",
  ai_conv_labels
)

# 2. Street vs Park Models Table  
street_park_models <- c("walk_street_model", "walk_park_model", "cycle_street_model", "cycle_park_model",
                        "plan_street_model", "plan_park_model", "attach_street_model", "attach_park_model")
street_park_labels <- c("Walk Street", "Walk Park", "Cycle Street", "Cycle Park",
                        "Plan Street", "Plan Park", "Attach Street", "Attach Park")

create_combined_table(
  street_park_models,
  "Regression Results: Street vs Park Contexts", 
  "street_park_models.tex",
  street_park_labels
)

# 3. Difference and Preference Models Table
diff_pref_models <- c("walk_diff_total_model", "cycle_diff_total_model", "plan_diff_total_model", 
                      "attach_diff_total_model", "ai_preference_model")
diff_pref_labels <- c("Walk Diff", "Cycle Diff", "Plan Diff", "Attach Diff", "AI Preference")

create_combined_table(
  diff_pref_models,
  "Regression Results: Treatment Effects and AI Preference",
  "difference_preference_models.tex", 
  diff_pref_labels
)

# 4. Complete Combined Table (if feasible - may be too wide)
# Only create if we have 8 or fewer models to avoid overly wide tables
all_model_names <- names(all_models)
valid_all_models <- all_models[!sapply(all_models, function(x) is.na(x)[1])]

if (length(valid_all_models) <= 8) {
  create_combined_table(
    names(valid_all_models),
    "Complete Regression Results: All Models",
    "all_models_combined.tex"
  )
}

# 5. Summary table by outcome type (alternative organization)
# Walking models
walking_models <- c("walk_ai_model", "walk_conv_model", "walk_street_model", "walk_park_model", "walk_diff_total_model")
walking_labels <- c("AI Method", "Conventional", "Street Context", "Park Context", "Treatment Effect")

create_combined_table(
  walking_models,
  "Walking Behavior: Regression Results Across Methods and Contexts",
  "walking_models.tex",
  walking_labels
)

# Cycling models  
cycling_models <- c("cycle_ai_model", "cycle_conv_model", "cycle_street_model", "cycle_park_model", "cycle_diff_total_model")
cycling_labels <- c("AI Method", "Conventional", "Street Context", "Park Context", "Treatment Effect")

create_combined_table(
  cycling_models,
  "Cycling Behavior: Regression Results Across Methods and Contexts", 
  "cycling_models.tex",
  cycling_labels
)

# Planning models
planning_models <- c("plan_ai_model", "plan_conv_model", "plan_street_model", "plan_park_model", "plan_diff_total_model")
planning_labels <- c("AI Method", "Conventional", "Street Context", "Park Context", "Treatment Effect")

create_combined_table(
  planning_models,
  "Planning Intentions: Regression Results Across Methods and Contexts",
  "planning_models.tex", 
  planning_labels
)

# Attachment models
attachment_models <- c("attach_ai_model", "attach_conv_model", "attach_street_model", "attach_park_model", "attach_diff_total_model")
attachment_labels <- c("AI Method", "Conventional", "Street Context", "Park Context", "Treatment Effect")

create_combined_table(
  attachment_models,
  "Place Attachment: Regression Results Across Methods and Contexts",
  "attachment_models.tex",
  attachment_labels
)

# ----------------------
# 6. Bootstrapped Coefficients (Modularized)
# ----------------------
# Inference follows the standard set in issue #45 and implemented in
# bootstrap_inference.R: significance is bootstrap CI excludes zero AND
# dropout < 1%, and above that threshold the interval is refused rather than
# quantiled from the resamples that retained the term. The version this
# replaced accepted up to 50% dropout silently and then starred the table from
# summary(model)$coefficients[, 4] -- the parametric OLS p-values -- under a
# caption that said bootstrap. That mislabel is the erratum #45 found.
bootstrap_regression <- function(model, data) {
  if (is.na(model)[1]) return(NULL)
  boot_lm(formula(model), data)
}

set.seed(BOOT_SEED)
boot_results_list <- purrr::imap(all_models, ~bootstrap_regression(.x, df))
boot_results_list <- boot_results_list[!sapply(boot_results_list, is.null)]
for (i in seq_along(boot_results_list)) boot_results_list[[i]]$model <- names(boot_results_list)[i]
all_boot_results <- bind_rows(boot_results_list)
write_csv(all_boot_results, boot_results_path)

# Presentation metadata (also used by the coefficient plots in section 7)
all_boot_results <- all_boot_results %>%
  mutate(
    # One mark, not a ladder: under issue #45's rule a coefficient either
    # passes or does not. The p-value ladder this replaced was parametric.
    sig_stars = ifelse(significant, "*", ""),
    model_type = case_when(
      grepl("_ai_model$", model) ~ "AI",
      grepl("_conv_model$", model) ~ "Conventional",
      grepl("_street_model$", model) ~ "Street",
      grepl("_park_model$", model) ~ "Park",
      grepl("_diff_total_model$", model) ~ "Difference Total",
      grepl("_preference_model$", model) ~ "Preference for AI",
      TRUE ~ "Other"
    ),
    outcome = case_when(
      grepl("^walk", model) ~ "Walking",
      grepl("^cycle", model) ~ "Cycling",
      grepl("^plan", model) ~ "Planning",
      grepl("^attach", model) ~ "Attachment",
      grepl("preference_model", model) ~ "AI Preference",
      TRUE ~ "Other"
    ),
    term_clean = gsub("TRUE$", "", gsub("_num$", "", term)),
    model_id = paste(outcome, "-", model_type)
  )

# The eight models the manuscript tabulates as tab:bootstrap_results.
ai_conv_bootstrap_models <- c("walk_ai_model", "walk_conv_model", "cycle_ai_model", "cycle_conv_model",
                              "plan_ai_model", "plan_conv_model", "attach_ai_model", "attach_conv_model")
ai_conv_column_labels <- c("Walk AI", "Walk Conv", "Cycle AI", "Cycle Conv",
                           "Plan AI", "Plan Conv", "Attach AI", "Attach Conv")

# Row order preserved from the submitted table so the latexdiff stays readable.
MAIN_TERM_ORDER <- names(TERM_LABELS)

# ----------------------
# 6.1. Main-text table (tab:bootstrap_results)
# ----------------------
# main.tex inputs models/main_test/bootstrap_ai_conventional_updated.tex, and
# until now NO script produced that file. It was a hand-built restyling of this
# script's output -- which is how the published stars came to be parametric
# while the caption said bootstrap, and why the issue #44 variable renames could
# not simply be re-run. It is generated here, so the table the manuscript prints
# is the table the pipeline computes.
#
# Point estimates and marks only. Issue #45 decision 2 sends the intervals to
# supplementary, because printing them in-text measured at ~+379 words against
# an overdrawn reserve. The sidewaystable + tabularx wrapper is kept exactly as
# the submitted table had it: issue #42 established that this is the one float
# combination texcount reads in full, so changing it would silently move every
# word count in the map.
write_main_bootstrap_table <- function(model_names, column_labels, title, label, filename) {
  wide <- all_boot_results %>%
    filter(model %in% model_names) %>%
    mutate(
      cell = fmt_cell(estimate, significant, stable),
      row_label = label_term(term),
      model = factor(model, levels = model_names)
    ) %>%
    select(term, row_label, model, cell) %>%
    arrange(match(term, MAIN_TERM_ORDER)) %>%
    select(-term) %>%
    pivot_wider(names_from = model, values_from = cell, values_fill = "")

  body <- apply(as.matrix(wide), 1, function(r) paste0(paste(r, collapse = " & "), " \\\\"))

  notes <- paste(note_inference(), note_hometown(df), note_cell_sizes(df))

  latex_content <- c(
    "\\begin{sidewaystable}[htbp]",
    "\\centering",
    paste0("\\caption{", title, "}"),
    paste0("\\label{", label, "}"),
    "\\footnotesize",
    paste0("\\begin{tabularx}{\\textwidth}{l*{", length(column_labels), "}{X}}"),
    "\\toprule",
    paste0(paste(c("Variable", column_labels), collapse = " & "), " \\\\"),
    "\\midrule",
    body,
    "\\bottomrule",
    "\\end{tabularx}",
    "\\begin{minipage}{\\textwidth}",
    "\\footnotesize",
    paste0("\\textit{Notes:} ", notes),
    "\\end{minipage}",
    "\\end{sidewaystable}"
  )
  writeLines(latex_content, paste0("reports/models/main_test/", filename))
  cat(paste("Saved main-text bootstrap table:", filename, "\n"))
}

write_main_bootstrap_table(
  ai_conv_bootstrap_models,
  ai_conv_column_labels,
  "Bootstrap Regression Results: AI vs Conventional Methods",
  "tab:bootstrap_results",
  main_table_path
)

# ----------------------
# 6.2. Supplementary table: full intervals and per-term dropout
# ----------------------
# Long format because dropout is a column, per issue #45 decision 2. Every term
# of every model in the main table appears, including the ones whose interval
# was refused -- a reader can see exactly which coefficients have no usable
# interval and why, which is the fact the main table can only mark with a
# dagger.
write_supp_ci_table <- function(model_names, column_labels, title, label, filename) {
  tab <- all_boot_results %>%
    filter(model %in% model_names) %>%
    mutate(
      model = factor(model, levels = model_names, labels = column_labels),
      row_label = label_term(term),
      est = formatC(estimate, format = "f", digits = 3),
      ci = ifelse(stable,
                  sprintf("[%s, %s]",
                          formatC(boot_ci_lower, format = "f", digits = 3),
                          formatC(boot_ci_upper, format = "f", digits = 3)),
                  "refused"),
      drop = sprintf("%.1f\\%%", 100 * dropout),
      mark = ifelse(significant, "*", "")
    ) %>%
    arrange(model, match(term, MAIN_TERM_ORDER)) %>%
    select(model, row_label, est, ci, drop, mark)

  body <- character(0)
  for (m in levels(tab$model)) {
    rows <- tab %>% filter(model == m)
    if (nrow(rows) == 0) next
    body <- c(body,
              paste0("\\multicolumn{4}{l}{\\textit{", m, "}} \\\\"),
              apply(as.matrix(rows %>%
                                mutate(est = paste0(est, mark)) %>%
                                select(row_label, est, ci, drop)),
                    1, function(r) paste0("\\quad ", paste(r, collapse = " & "), " \\\\")),
              "\\addlinespace")
  }

  notes <- paste(
    note_inference(),
    "An interval is shown as \\emph{refused} where dropout reaches 1\\%: it would have been",
    "computed only from the resamples in which the term survived, and so would be conditioned",
    "on a rare category appearing.",
    note_hometown(df), note_cell_sizes(df)
  )

  latex_content <- c(
    "\\footnotesize",
    "\\begin{longtable}{lrrr}",
    paste0("\\caption{", title, "}"),
    paste0("\\label{", label, "} \\\\"),
    "\\toprule",
    "Variable & Estimate & 95\\% CI & Dropout \\\\",
    "\\midrule",
    "\\endfirsthead",
    "\\toprule",
    "Variable & Estimate & 95\\% CI & Dropout \\\\",
    "\\midrule",
    "\\endhead",
    body,
    "\\bottomrule",
    "\\end{longtable}",
    "\\begin{minipage}{\\textwidth}",
    "\\footnotesize",
    paste0("\\textit{Notes:} ", notes),
    "\\end{minipage}",
    "\\normalsize"
  )
  writeLines(latex_content, paste0("reports/models/main_test/", filename))
  cat(paste("Saved supplementary CI table:", filename, "\n"))
}

write_supp_ci_table(
  ai_conv_bootstrap_models,
  ai_conv_column_labels,
  "Bootstrap regression coefficients with full 95\\% confidence intervals and per-term dropout: AI vs Conventional Methods",
  "tab:bootstrap_ci_supp",
  supp_table_path
)

# ----------------------
# 7. Visualization (Standardized)
# ----------------------
# Helper: Beautiful, standardized plot theme
library(viridis)
beautiful_theme <- theme_light(base_size = 16, base_family = "Helvetica") +
  theme(
    plot.title = element_text(hjust = 0.5, size = 20, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5, size = 14, face = "italic"),
    axis.title.x = element_text(size = 16, face = "bold"),
    axis.title.y = element_text(size = 16, face = "bold"),
    axis.text = element_text(size = 14),
    strip.text = element_text(size = 16, face = "bold"),
    legend.title = element_text(size = 16, face = "bold"),
    legend.text = element_text(size = 14),
    legend.position = "bottom",
    panel.grid.major.y = element_line(color = "#e0e0e0"),
    panel.grid.minor = element_blank(),
    panel.background = element_rect(fill = "#f9f9f9"),
    plot.background = element_rect(fill = "#f9f9f9"),
    panel.border = element_rect(color = "#cccccc", fill = NA)
  )

key_vars <- c(
  "genai_firstTRUE", "street_firstTRUE", 
  "GenderWoman",
  "Planning_Knowledge_num", "AI_Experience_num", "AI_Tool_Usage_num",
  "Primary_TransportationWalk", "Primary_TransportationBike", "Primary_TransportationPublic Transit",
  "HometownRural", "HometownSuburban"
)

boot_coef_plot_data <- all_boot_results %>%
  filter(term %in% key_vars) %>%
  mutate(term_ordered = factor(term, levels = key_vars))

boot_coef_plot <- ggplot(boot_coef_plot_data, aes(x = estimate, y = term_ordered, color = model_type)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray40") +
  geom_point(size = 3) +
  geom_errorbarh(aes(xmin = boot_ci_lower, xmax = boot_ci_upper), height = 0.25, size = 1) +
  geom_text(aes(label = sig_stars, x = ifelse(estimate >= 0, boot_ci_upper, boot_ci_lower)), 
            hjust = ifelse(boot_coef_plot_data$estimate >= 0, -0.3, 1.3), size = 5, fontface = "bold") +
  facet_wrap(~ model_id, ncol = 2, scales = "free_x") +
  labs(
    title = paste0("Regression Coefficients with Bootstrapped 95% CIs (", N_BOOT, " resamples)"),
    subtitle = "Points show coefficient estimates; horizontal lines show 95% confidence intervals",
    x = "Coefficient Estimate",
    y = "Predictor",
    color = "Model Type"
  ) +
  scale_color_viridis_d(option = "D", end = 0.8) +
  scale_y_discrete(labels = function(x) gsub("_num|TRUE", "", x)) +
  coord_cartesian(xlim = c(
    min(boot_coef_plot_data$boot_ci_lower, na.rm = TRUE) * 1.2,
    max(boot_coef_plot_data$boot_ci_upper, na.rm = TRUE) * 1.2
  )) +
  beautiful_theme

ggsave(coef_plot_path, boot_coef_plot, width = 12, height = 16)
ggsave(coef_plot_pdf_path, boot_coef_plot, width = 12, height = 16)

# Experimental conditions plot
boot_exp_plot_data <- boot_coef_plot_data %>%
  filter(term %in% c("genai_firstTRUE", "street_firstTRUE"))

boot_exp_coef_plot <- ggplot(boot_exp_plot_data, aes(x = estimate, y = model_type, color = term_ordered)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray40") +
  geom_point(size = 4) +
  geom_errorbarh(aes(xmin = boot_ci_lower, xmax = boot_ci_upper), height = 0.3, size = 1.2) +
  geom_text(aes(label = sig_stars, x = ifelse(estimate >= 0, boot_ci_upper, boot_ci_lower)), 
            hjust = ifelse(boot_exp_plot_data$estimate >= 0, -0.3, 1.3), size = 6, fontface = "bold") +
  facet_wrap(~ outcome, ncol = 2, scales = "free_x") +
  coord_cartesian(xlim = c(
    min(boot_exp_plot_data$boot_ci_lower, na.rm = TRUE) * 1.2,
    max(boot_exp_plot_data$boot_ci_upper, na.rm = TRUE) * 1.2
  )) +
  labs(
    title = paste0("Order Effects on Outcomes (Bootstrapped, ", N_BOOT, " resamples)"),
    x = "Coefficient Estimate",
    y = "Model Type",
    color = "Condition"
  ) +
  scale_color_viridis_d(option = "C", end = 0.8, labels = c("GenAI First", "Street First")) +
  scale_y_discrete(labels = function(x) gsub("_", " ", x)) +
  beautiful_theme

ggsave(exp_plot_path, boot_exp_coef_plot, width = 10, height = 8)
ggsave(exp_plot_pdf_path, boot_exp_coef_plot, width = 10, height = 8)

# ----------------------
# 8. End of Script
# ----------------------
cat("\nAnalysis complete. Outputs saved to reports/models/main_test/ and reports/figures/main_test/.\n")

