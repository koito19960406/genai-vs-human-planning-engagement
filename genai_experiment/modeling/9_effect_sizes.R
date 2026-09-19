# =============================================================
# 9_effect_sizes.R
# Cohen's d effect size analysis for GenAI experiment
# Computes paired and one-sample effect sizes for all outcomes
# =============================================================

# ----------------------
# 0. Load Packages & Setup
# ----------------------
pacman::p_load(
  dplyr,
  tidyr,
  readr,
  effsize
)

source("genai_experiment/modeling/theme_beautiful.R")

# Configurable paths
processed_data_path <- "data/processed/survey_processed.csv"
effect_sizes_path <- "reports/models/main_test/effect_sizes.csv"

# Create directories if needed
dir.create(dirname(effect_sizes_path), recursive = TRUE, showWarnings = FALSE)

# ----------------------
# 1. Data Loading
# ----------------------
if (!file.exists(processed_data_path)) stop("Processed data not found!")
df <- read_csv(processed_data_path, show_col_types = FALSE)

# ----------------------
# 2. Paired Cohen's d: GenAI vs Traditional
# ----------------------
# For each outcome, compute Cohen's d for the paired difference
# between GenAI and Traditional method attitude changes

paired_comparisons <- list(
  list(var1 = "walk_ai", var2 = "walk_conv", outcome = "Walking"),
  list(var1 = "cycle_ai", var2 = "cycle_conv", outcome = "Cycling"),
  list(var1 = "plan_ai", var2 = "plan_conv", outcome = "Planning"),
  list(var1 = "attach_ai", var2 = "attach_conv", outcome = "Attachment")
)

compute_paired_d <- function(comp) {
  pair_df <- df %>%
    select(all_of(c(comp$var1, comp$var2))) %>%
    drop_na()

  if (nrow(pair_df) < 3) return(NULL)

  result <- cohen.d(pair_df[[comp$var1]], pair_df[[comp$var2]],
                    paired = TRUE, hedges.correction = FALSE)

  tibble(
    comparison = paste0(comp$outcome, ": GenAI vs Traditional"),
    type = "paired",
    n = nrow(pair_df),
    mean_genai = mean(pair_df[[comp$var1]]),
    sd_genai = sd(pair_df[[comp$var1]]),
    mean_trad = mean(pair_df[[comp$var2]]),
    sd_trad = sd(pair_df[[comp$var2]]),
    cohens_d = result$estimate,
    ci_lower = result$conf.int[1],
    ci_upper = result$conf.int[2],
    magnitude = result$magnitude
  )
}

paired_results <- bind_rows(lapply(paired_comparisons, compute_paired_d))

# ----------------------
# 3. One-Sample Cohen's d: Total Change (Wave 3 - Wave 1)
# ----------------------
# Tests whether the overall change from baseline is meaningfully
# different from zero

total_change_vars <- list(
  list(var = "walk_diff_total", outcome = "Walking"),
  list(var = "cycle_diff_total", outcome = "Cycling"),
  list(var = "plan_diff_total", outcome = "Planning"),
  list(var = "attach_diff_total", outcome = "Attachment")
)

compute_onesample_d <- function(comp) {
  values <- df[[comp$var]]
  values <- values[!is.na(values)]

  if (length(values) < 3) return(NULL)

  # One-sample Cohen's d: mean / sd
  d_value <- mean(values) / sd(values)

  # CI via bootstrap
  set.seed(42)
  boot_d <- replicate(1000, {
    s <- sample(values, replace = TRUE)
    mean(s) / sd(s)
  })
  ci <- quantile(boot_d, c(0.025, 0.975))

  magnitude <- case_when(
    abs(d_value) >= 0.8 ~ "large",
    abs(d_value) >= 0.5 ~ "medium",
    abs(d_value) >= 0.2 ~ "small",
    TRUE ~ "negligible"
  )

  tibble(
    comparison = paste0(comp$outcome, ": Total Change (Wave 3 - Wave 1)"),
    type = "one-sample",
    n = length(values),
    mean_genai = mean(values),
    sd_genai = sd(values),
    mean_trad = NA_real_,
    sd_trad = NA_real_,
    cohens_d = d_value,
    ci_lower = ci[1],
    ci_upper = ci[2],
    magnitude = magnitude
  )
}

onesample_results <- bind_rows(lapply(total_change_vars, compute_onesample_d))

# ----------------------
# 4. Combine and Save
# ----------------------
all_results <- bind_rows(paired_results, onesample_results)
write_csv(all_results, effect_sizes_path)

# ----------------------
# 5. Print Summary
# ----------------------
cat("\n========================================\n")
cat("Cohen's d Effect Size Analysis\n")
cat("========================================\n\n")

cat("--- Paired: GenAI vs Traditional ---\n")
for (i in seq_len(nrow(paired_results))) {
  r <- paired_results[i, ]
  cat(sprintf("  %s: d = %.3f [%.3f, %.3f] (%s), n = %d\n",
              r$comparison, r$cohens_d, r$ci_lower, r$ci_upper, r$magnitude, r$n))
}

cat("\n--- One-Sample: Total Change ---\n")
for (i in seq_len(nrow(onesample_results))) {
  r <- onesample_results[i, ]
  cat(sprintf("  %s: d = %.3f [%.3f, %.3f] (%s), n = %d, mean = %.3f\n",
              r$comparison, r$cohens_d, r$ci_lower, r$ci_upper, r$magnitude, r$n, r$mean_genai))
}

cat("\nResults saved to:", effect_sizes_path, "\n")
