# =============================================================
# 3_ttest_analysis.R
# T-test analysis for GenAI experiment
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

source("genai_experiment/modeling/theme_beautiful.R")
source("genai_experiment/modeling/bootstrap_inference.R")  # BOOT_SEED, N_BOOT (issues #45, #49)

# Configurable paths
processed_data_path <- "data/processed/survey_processed.csv"
ttest_results_path <- "reports/models/main_test/paired_ttests.csv"
ttest_boot_path <- "reports/models/main_test/bootstrapped_ttests.csv"
mean_diff_plot_path <- "reports/figures/main_test/mean_diff_bootstrap_plot.png"
ai_pref_plot_path <- "reports/figures/main_test/ai_preference_distribution.png"
ai_pref_plot_pdf_path <- "reports/figures/main_test/ai_preference_distribution.pdf"

# Create directories if needed
for (dir in c(dirname(ttest_results_path), dirname(mean_diff_plot_path))) {
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

# List of all variables used in t-tests
required_vars <- c(
  # Outcomes
  "walk_ai", "walk_conv", "cycle_ai", "cycle_conv", "plan_ai", "plan_conv", "attach_ai", "attach_conv",
  "walk_street", "walk_park", "cycle_street", "cycle_park", "plan_street", "plan_park", "attach_street", "attach_park",
  "walk_total_genai_first", "walk_total_conv_first", "cycle_total_genai_first", "cycle_total_conv_first",
  "plan_total_genai_first", "plan_total_conv_first", "attach_total_genai_first", "attach_total_conv_first",
  "walk_total_street_first", "walk_total_park_first", "cycle_total_street_first", "cycle_total_park_first",
  "plan_total_street_first", "plan_total_park_first", "attach_total_street_first", "attach_total_park_first",
  "AI_or_Conventional_num",
  # Difference variables for one-sample t-tests
  "walk_diff_total", "cycle_diff_total", "plan_diff_total", "attach_diff_total"
)
check_columns(df, required_vars)

# ----------------------
# 2. T-Tests (Modularized)
# ----------------------
# Helper: Paired t-test
paired_ttest <- function(var1, var2, label) {
  pair_df <- df %>% select(!!sym(var1), !!sym(var2)) %>% drop_na()
  if (nrow(pair_df) < 3) return(NULL)
  test_result <- t.test(pair_df[[var1]], pair_df[[var2]], paired = TRUE)
  data.frame(
    comparison = label,
    mean_diff = test_result$estimate,
    t_stat = test_result$statistic,
    df = test_result$parameter,
    p_value = test_result$p.value,
    conf_low = test_result$conf.int[1],
    conf_high = test_result$conf.int[2]
  )
}

# Helper: Independent t-test
independent_ttest <- function(var1, var2, label) {
  var1_data <- df[[var1]][!is.na(df[[var1]])]
  var2_data <- df[[var2]][!is.na(df[[var2]])]
  if (length(var1_data) < 3 || length(var2_data) < 3) return(NULL)
  test_result <- t.test(var1_data, var2_data, paired = FALSE, var.equal = FALSE)
  data.frame(
    comparison = label,
    mean_diff = test_result$estimate[1] - test_result$estimate[2],
    t_stat = test_result$statistic,
    df = test_result$parameter,
    p_value = test_result$p.value,
    conf_low = test_result$conf.int[1],
    conf_high = test_result$conf.int[2]
  )
}

# Helper: One-sample t-test (testing if mean > 0)
onesample_ttest <- function(var, label) {
  var_data <- df[[var]][!is.na(df[[var]])]
  if (length(var_data) < 3) return(NULL)
  test_result <- t.test(var_data, mu = 0, alternative = "greater")
  data.frame(
    comparison = label,
    mean_diff = test_result$estimate,
    t_stat = test_result$statistic,
    df = test_result$parameter,
    p_value = test_result$p.value,
    conf_low = test_result$conf.int[1],
    conf_high = test_result$conf.int[2]
  )
}

# Run all t-tests
ttest_results <- bind_rows(
  paired_ttest("walk_ai", "walk_conv", "Walking: AI vs Conventional"),
  paired_ttest("cycle_ai", "cycle_conv", "Cycling: AI vs Conventional"),
  paired_ttest("plan_ai", "plan_conv", "Planning: AI vs Conventional"),
  paired_ttest("attach_ai", "attach_conv", "Attachment: AI vs Conventional"),
  paired_ttest("walk_street", "walk_park", "Walking: Street vs Park"),
  paired_ttest("cycle_street", "cycle_park", "Cycling: Street vs Park"),
  paired_ttest("plan_street", "plan_park", "Planning: Street vs Park"),
  paired_ttest("attach_street", "attach_park", "Attachment: Street vs Park"),
  independent_ttest("walk_total_genai_first", "walk_total_conv_first", "Walking Total: GenAI First vs Conv First"),
  independent_ttest("cycle_total_genai_first", "cycle_total_conv_first", "Cycling Total: GenAI First vs Conv First"),
  independent_ttest("plan_total_genai_first", "plan_total_conv_first", "Planning Total: GenAI First vs Conv First"),
  independent_ttest("attach_total_genai_first", "attach_total_conv_first", "Attachment Total: GenAI First vs Conv First"),
  independent_ttest("walk_total_street_first", "walk_total_park_first", "Walking Total: Street First vs Park First"),
  independent_ttest("cycle_total_street_first", "cycle_total_park_first", "Cycling Total: Street First vs Park First"),
  independent_ttest("plan_total_street_first", "plan_total_park_first", "Planning Total: Street First vs Park First"),
  independent_ttest("attach_total_street_first", "attach_total_park_first", "Attachment Total: Street First vs Park First"),
  onesample_ttest("walk_diff_total", "Walking Total: GenAI vs Conv Difference"),
  onesample_ttest("cycle_diff_total", "Cycling Total: GenAI vs Conv Difference"),
  onesample_ttest("plan_diff_total", "Planning Total: GenAI vs Conv Difference"),
  onesample_ttest("attach_diff_total", "Attachment Total: GenAI vs Conv Difference")
)
write_csv(ttest_results, ttest_results_path)

# ----------------------
# 2.5. One-Sample T-Tests for Difference Variables (Separate Analysis)
# ----------------------
# Run one-sample t-tests specifically for difference variables
diff_ttest_results <- bind_rows(
  onesample_ttest("walk_diff_total", "Walking: Total Difference (Wave 3 - Wave 1)"),
  onesample_ttest("cycle_diff_total", "Cycling: Total Difference (Wave 3 - Wave 1)"),
  onesample_ttest("plan_diff_total", "Planning: Total Difference (Wave 3 - Wave 1)"),
  onesample_ttest("attach_diff_total", "Attachment: Total Difference (Wave 3 - Wave 1)")
)

# Add descriptive statistics
diff_summary <- df %>%
  select(walk_diff_total, cycle_diff_total, plan_diff_total, attach_diff_total) %>%
  pivot_longer(everything(), names_to = "variable", values_to = "value") %>%
  drop_na() %>%
  group_by(variable) %>%
  summarise(
    n = n(),
    mean = mean(value),
    sd = sd(value),
    median = median(value),
    min = min(value),
    max = max(value),
    .groups = "drop"
  ) %>%
  mutate(
    outcome = case_when(
      variable == "walk_diff_total" ~ "Walking: Total Difference (Wave 3 - Wave 1)",
      variable == "cycle_diff_total" ~ "Cycling: Total Difference (Wave 3 - Wave 1)",
      variable == "plan_diff_total" ~ "Planning: Total Difference (Wave 3 - Wave 1)",
      variable == "attach_diff_total" ~ "Attachment: Total Difference (Wave 3 - Wave 1)"
    )
  )

# Merge t-test results with descriptive statistics
diff_analysis <- diff_ttest_results %>%
  left_join(diff_summary, by = c("comparison" = "outcome")) %>%
  select(comparison, n, mean, sd, median, min, max, t_stat, df, p_value, conf_low, conf_high)

# Save results
write_csv(diff_ttest_results, "reports/models/main_test/difference_ttests.csv")
write_csv(diff_analysis, "reports/models/main_test/difference_analysis_complete.csv")

cat("\nOne-sample t-tests for difference variables completed.")
cat("\nTesting H0: mean = 0 vs H1: mean > 0")
cat("\nResults saved to reports/models/main_test/difference_ttests.csv and difference_analysis_complete.csv\n")

# ----------------------
# 2.6. Difference Variables Histograms
# ----------------------
# Create histograms for each difference variable
diff_vars <- c("walk_diff_total", "cycle_diff_total", "plan_diff_total", "attach_diff_total")
diff_labels <- c("Walking", "Cycling", "Planning", "Attachment")

# Prepare data for plotting
diff_plot_data <- df %>%
  select(all_of(diff_vars)) %>%
  pivot_longer(everything(), names_to = "variable", values_to = "value") %>%
  drop_na() %>%
  mutate(
    outcome = case_when(
      variable == "walk_diff_total" ~ "Walking",
      variable == "cycle_diff_total" ~ "Cycling", 
      variable == "plan_diff_total" ~ "Planning",
      variable == "attach_diff_total" ~ "Attachment"
    )
  )

# Calculate means for each variable
diff_means <- diff_plot_data %>%
  group_by(outcome) %>%
  summarise(mean_val = mean(value, na.rm = TRUE), .groups = "drop")

# Get p-values from t-tests for subtitles
diff_pvalues <- diff_ttest_results %>%
  mutate(
    outcome = case_when(
      str_detect(comparison, "Walking") ~ "Walking",
      str_detect(comparison, "Cycling") ~ "Cycling",
      str_detect(comparison, "Planning") ~ "Planning", 
      str_detect(comparison, "Attachment") ~ "Attachment"
    )
  ) %>%
  select(outcome, p_value)

# Merge means and p-values
diff_plot_summary <- diff_means %>%
  left_join(diff_pvalues, by = "outcome")

# Create faceted histogram
diff_histogram_plot <- diff_plot_data %>%
  left_join(diff_plot_summary, by = "outcome") %>%
  ggplot(aes(x = value)) +
  geom_histogram(binwidth = 0.2, fill = viridis(5)[3], color = "black", alpha = 0.85) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "red", size = 1.2) +
  geom_vline(aes(xintercept = mean_val), linetype = "solid", color = viridis(5)[5], size = 1.2) +
  facet_wrap(~ outcome, scales = "free", ncol = 2) +
  labs(
    title = "Distribution of Total Differences (Wave 3 - Wave 1)",
    subtitle = "Red dashed line = 0, Colored solid line = Mean",
    x = "Difference Score",
    y = "Count"
  ) +
  geom_text(aes(x = Inf, y = Inf, 
               label = paste0("Mean = ", round(mean_val, 2), "\np = ", format.pval(p_value, digits = 3))),
           hjust = 1.1, vjust = 1.1, size = 4, fontface = "bold") +
  theme_beautiful()

# Save the plot
ggsave("reports/figures/main_test/difference_histograms.png", diff_histogram_plot, 
       width = 12, height = 10, bg = "white")
ggsave("reports/figures/main_test/difference_histograms.pdf", diff_histogram_plot, 
       width = 12, height = 10, bg = "white")

cat("\nDifference variable histograms saved to reports/figures/main_test/difference_histograms.png and .pdf\n")

# ----------------------
# 3. Bootstrapped T-Tests (Modularized)
# ----------------------
# Helper: Bootstrap paired difference
bootstrap_paired_diff <- function(data, var1, var2, n_boot = N_BOOT) {
  paired_data <- data %>% select(!!sym(var1), !!sym(var2)) %>% drop_na()
  if (nrow(paired_data) < 3) return(NULL)
  diffs <- paired_data[[var1]] - paired_data[[var2]]
  boot_mean <- function(data, indices) mean(data[indices])
  set.seed(BOOT_SEED)
  boot_results <- boot(diffs, boot_mean, R = n_boot)
  ci <- tryCatch(boot.ci(boot_results, type = "perc"), error = function(e) NULL)
  if (is.null(ci) || is.null(ci$percent)) return(NULL)
  tibble(
    mean_diff = mean(diffs),
    ci_lower = ci$percent[4],
    ci_upper = ci$percent[5],
    n = length(diffs)
  )
}

# Run bootstrapped t-tests
ttest_boot_results <- bind_rows(
  bootstrap_paired_diff(df, "walk_ai", "walk_conv") %>% mutate(outcome = "Walking", comparison = "AI vs Conventional", var1 = "walk_ai", var2 = "walk_conv"),
  bootstrap_paired_diff(df, "cycle_ai", "cycle_conv") %>% mutate(outcome = "Cycling", comparison = "AI vs Conventional", var1 = "cycle_ai", var2 = "cycle_conv"),
  bootstrap_paired_diff(df, "plan_ai", "plan_conv") %>% mutate(outcome = "Planning", comparison = "AI vs Conventional", var1 = "plan_ai", var2 = "plan_conv"),
  bootstrap_paired_diff(df, "attach_ai", "attach_conv") %>% mutate(outcome = "Attachment", comparison = "AI vs Conventional", var1 = "attach_ai", var2 = "attach_conv"),
  bootstrap_paired_diff(df, "walk_street", "walk_park") %>% mutate(outcome = "Walking", comparison = "Street vs Park", var1 = "walk_street", var2 = "walk_park"),
  bootstrap_paired_diff(df, "cycle_street", "cycle_park") %>% mutate(outcome = "Cycling", comparison = "Street vs Park", var1 = "cycle_street", var2 = "cycle_park"),
  bootstrap_paired_diff(df, "plan_street", "plan_park") %>% mutate(outcome = "Planning", comparison = "Street vs Park", var1 = "plan_street", var2 = "plan_park"),
  bootstrap_paired_diff(df, "attach_street", "attach_park") %>% mutate(outcome = "Attachment", comparison = "Street vs Park", var1 = "attach_street", var2 = "attach_park")
)

# Add significance markers.
# One mark, and it means one thing: the bootstrap interval excludes zero. The
# earlier four-level ladder read as a p-value ladder but was a MAGNITUDE ladder
# (*** at |diff| > 0.3, ** at > 0.2, * at > 0.1, all conditional on the interval
# excluding zero), so the cycling contrast carried *** on the strength of its
# effect size, not of its evidence. Both regression tables state that no p-value
# ladder is reported (issues #45, #49); this figure now matches them.
if (nrow(ttest_boot_results) > 0) {
  ttest_boot_results <- ttest_boot_results %>%
    mutate(sig = ifelse(sign(ci_lower) == sign(ci_upper), "*", ""))
  write_csv(ttest_boot_results, ttest_boot_path)
}

# ----------------------
# 4. Visualization (Beautified)
# ----------------------


# Prepare data for mean value comparison plots
mean_comparison_data <- bind_rows(
  # AI vs Conventional
  df %>% select(walk_ai, walk_conv) %>% drop_na() %>% summarise(
    ai_mean = mean(walk_ai),
    ai_lower = mean(walk_ai) - 1.96 * sd(walk_ai) / sqrt(n()),
    ai_upper = mean(walk_ai) + 1.96 * sd(walk_ai) / sqrt(n()),
    conv_mean = mean(walk_conv),
    conv_lower = mean(walk_conv) - 1.96 * sd(walk_conv) / sqrt(n()),
    conv_upper = mean(walk_conv) + 1.96 * sd(walk_conv) / sqrt(n())
  ) %>% mutate(outcome = "Walking", comparison = "AI vs Conventional"),
  df %>% select(cycle_ai, cycle_conv) %>% drop_na() %>% summarise(
    ai_mean = mean(cycle_ai),
    ai_lower = mean(cycle_ai) - 1.96 * sd(cycle_ai) / sqrt(n()),
    ai_upper = mean(cycle_ai) + 1.96 * sd(cycle_ai) / sqrt(n()),
    conv_mean = mean(cycle_conv),
    conv_lower = mean(cycle_conv) - 1.96 * sd(cycle_conv) / sqrt(n()),
    conv_upper = mean(cycle_conv) + 1.96 * sd(cycle_conv) / sqrt(n())
  ) %>% mutate(outcome = "Cycling", comparison = "AI vs Conventional"),
  df %>% select(plan_ai, plan_conv) %>% drop_na() %>% summarise(
    ai_mean = mean(plan_ai),
    ai_lower = mean(plan_ai) - 1.96 * sd(plan_ai) / sqrt(n()),
    ai_upper = mean(plan_ai) + 1.96 * sd(plan_ai) / sqrt(n()),
    conv_mean = mean(plan_conv),
    conv_lower = mean(plan_conv) - 1.96 * sd(plan_conv) / sqrt(n()),
    conv_upper = mean(plan_conv) + 1.96 * sd(plan_conv) / sqrt(n())
  ) %>% mutate(outcome = "Planning", comparison = "AI vs Conventional"),
  df %>% select(attach_ai, attach_conv) %>% drop_na() %>% summarise(
    ai_mean = mean(attach_ai),
    ai_lower = mean(attach_ai) - 1.96 * sd(attach_ai) / sqrt(n()),
    ai_upper = mean(attach_ai) + 1.96 * sd(attach_ai) / sqrt(n()),
    conv_mean = mean(attach_conv),
    conv_lower = mean(attach_conv) - 1.96 * sd(attach_conv) / sqrt(n()),
    conv_upper = mean(attach_conv) + 1.96 * sd(attach_conv) / sqrt(n())
  ) %>% mutate(outcome = "Attachment", comparison = "AI vs Conventional"),
  # Street vs Park
  df %>% select(walk_street, walk_park) %>% drop_na() %>% summarise(
    street_mean = mean(walk_street),
    street_lower = mean(walk_street) - 1.96 * sd(walk_street) / sqrt(n()),
    street_upper = mean(walk_street) + 1.96 * sd(walk_street) / sqrt(n()),
    park_mean = mean(walk_park),
    park_lower = mean(walk_park) - 1.96 * sd(walk_park) / sqrt(n()),
    park_upper = mean(walk_park) + 1.96 * sd(walk_park) / sqrt(n())
  ) %>% mutate(outcome = "Walking", comparison = "Street vs Park"),
  df %>% select(cycle_street, cycle_park) %>% drop_na() %>% summarise(
    street_mean = mean(cycle_street),
    street_lower = mean(cycle_street) - 1.96 * sd(cycle_street) / sqrt(n()),
    street_upper = mean(cycle_street) + 1.96 * sd(cycle_street) / sqrt(n()),
    park_mean = mean(cycle_park),
    park_lower = mean(cycle_park) - 1.96 * sd(cycle_park) / sqrt(n()),
    park_upper = mean(cycle_park) + 1.96 * sd(cycle_park) / sqrt(n())
  ) %>% mutate(outcome = "Cycling", comparison = "Street vs Park"),
  df %>% select(plan_street, plan_park) %>% drop_na() %>% summarise(
    street_mean = mean(plan_street),
    street_lower = mean(plan_street) - 1.96 * sd(plan_street) / sqrt(n()),
    street_upper = mean(plan_street) + 1.96 * sd(plan_street) / sqrt(n()),
    park_mean = mean(plan_park),
    park_lower = mean(plan_park) - 1.96 * sd(plan_park) / sqrt(n()),
    park_upper = mean(plan_park) + 1.96 * sd(plan_park) / sqrt(n())
  ) %>% mutate(outcome = "Planning", comparison = "Street vs Park"),
  df %>% select(attach_street, attach_park) %>% drop_na() %>% summarise(
    street_mean = mean(attach_street),
    street_lower = mean(attach_street) - 1.96 * sd(attach_street) / sqrt(n()),
    street_upper = mean(attach_street) + 1.96 * sd(attach_street) / sqrt(n()),
    park_mean = mean(attach_park),
    park_lower = mean(attach_park) - 1.96 * sd(attach_park) / sqrt(n()),
    park_upper = mean(attach_park) + 1.96 * sd(attach_park) / sqrt(n())
  ) %>% mutate(outcome = "Attachment", comparison = "Street vs Park")
)

# Reshape the data for easier plotting
mean_comp_long <- bind_rows(
  # AI vs Conventional
  mean_comparison_data %>%
    filter(comparison == "AI vs Conventional") %>%
    select(outcome, comparison, ai_mean, ai_lower, ai_upper, conv_mean, conv_lower, conv_upper) %>%
    pivot_longer(
      cols = c(ai_mean, ai_lower, ai_upper, conv_mean, conv_lower, conv_upper),
      names_to = "measure",
      values_to = "value"
    ) %>%
    mutate(
      group = ifelse(grepl("^ai_", measure), "AI", "Conventional"),
      stat_type = case_when(
        grepl("_mean$", measure) ~ "mean",
        grepl("_lower$", measure) ~ "lower",
        grepl("_upper$", measure) ~ "upper"
      )
    ),
  # Street vs Park
  mean_comparison_data %>%
    filter(comparison == "Street vs Park") %>%
    select(outcome, comparison, street_mean, street_lower, street_upper, park_mean, park_lower, park_upper) %>%
    pivot_longer(
      cols = c(street_mean, street_lower, street_upper, park_mean, park_lower, park_upper),
      names_to = "measure",
      values_to = "value"
    ) %>%
    mutate(
      group = ifelse(grepl("^street_", measure), "Street", "Park"),
      stat_type = case_when(
        grepl("_mean$", measure) ~ "mean",
        grepl("_lower$", measure) ~ "lower",
        grepl("_upper$", measure) ~ "upper"
      )
    )
) %>%
  pivot_wider(
    id_cols = c(outcome, comparison, group),
    names_from = stat_type,
    values_from = value
  )

# Join with significance markers
if (nrow(ttest_boot_results) > 0) {
  mean_comp_long <- mean_comp_long %>%
    left_join(
      ttest_boot_results %>% select(outcome, comparison, sig),
      by = c("outcome", "comparison")
    ) %>%
    mutate(sig = ifelse(is.na(sig), "", sig))
}

# Difference plot using bootstrapped results
if (nrow(ttest_boot_results) > 0) {
  # Symmetric y-limits about zero (shared across facets) so the zero reference
  # line sits at the vertical centre of both panels and the tick values mirror
  # each other above and below it.
  y_limit <- ceiling(max(abs(c(ttest_boot_results$ci_lower, ttest_boot_results$ci_upper))) * 4) / 4

  diff_plot <- ggplot(ttest_boot_results, aes(x = outcome, y = mean_diff, fill = comparison)) +
    geom_bar(stat = "identity", position = position_dodge(width = 0.7), color = "black", width = 0.6) +
    geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper), width = 0.2, position = position_dodge(width = 0.7)) +
    geom_text(aes(label = sig, y = ifelse(mean_diff >= 0, ci_upper, ci_lower)),
              vjust = ifelse(ttest_boot_results$mean_diff >= 0, -0.5, 1.5), size = 6, fontface = "bold") +
    facet_wrap(~ comparison, ncol = 2) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "red", linewidth = 0.8) +
    labs(
      title = paste0("Mean Differences with Bootstrapped 95% CIs (",
                     format(N_BOOT, big.mark = ","), " resamples)"),
      x = "Outcome",
      y = "Mean Difference"
    ) +
    scale_y_continuous(breaks = seq(-y_limit, y_limit, by = y_limit / 2)) +
    coord_cartesian(ylim = c(-y_limit, y_limit)) +
    scale_fill_manual(values = c("AI vs Conventional" = "#AB84A5FF", "Street vs Park" = "#75884BFF")) +
    theme_beautiful() +
    theme(
      legend.position = "none",
      plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
      axis.text.x = element_text(angle = 45, hjust = 1)
    )
  # Saved at ~9 in so the type is still legible at the 0.8\textwidth used in the paper
  ggsave(mean_diff_plot_path, diff_plot, width = 9, height = 6, bg = "white")
}

# ----------------------
# 5. AI Preference T-Test & Visualization (Beautified)
# ----------------------
ai_preference_ttest <- t.test(df$AI_or_Conventional_num, mu = 0)
ai_preference_result <- data.frame(
  test = "One-sample t-test for AI preference",
  mean = mean(df$AI_or_Conventional_num, na.rm = TRUE),
  t_stat = ai_preference_ttest$statistic,
  df = ai_preference_ttest$parameter,
  p_value = ai_preference_ttest$p.value,
  conf_low = ai_preference_ttest$conf.int[1],
  conf_high = ai_preference_ttest$conf.int[2]
)
write_csv(ai_preference_result, "reports/models/main_test/ai_preference_ttest.csv")

ai_preference_plot <- df %>%
  drop_na(AI_or_Conventional_num) %>%
  ggplot(aes(x = AI_or_Conventional_num)) +
  geom_histogram(binwidth = 1, fill = "#AB84A5FF", color = "black", alpha = 0.85, center = 0) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "red", size = 1.2) +
  geom_vline(xintercept = mean(df$AI_or_Conventional_num, na.rm = TRUE), 
             linetype = "solid", color = "#75884BFF", size = 1.2) +
  scale_x_continuous(breaks = c(-2, -1, 0, 1, 2),
                    labels = c("Traditional only", "Mostly traditional", "Both equally", "Mostly AI", "AI only")) +
  labs(
    title = "Preference for AI vs Traditional Visualization",
    subtitle = paste0("Mean = ", round(mean(df$AI_or_Conventional_num, na.rm = TRUE), 2), 
                     ", p-value = ", format.pval(ai_preference_ttest$p.value, digits = 3)),
    x = "Preference",
    y = "Count"
  ) +
  theme_beautiful()

ggsave(ai_pref_plot_path, ai_preference_plot, width = 10, height = 10, bg = "white")

cat("\nT-test analysis complete. Outputs saved to reports/models/main_test/ and reports/figures/main_test/.\n")

# Save results
write_excel_csv(ttest_results, "reports/models/main_test/ttest_results.csv")
