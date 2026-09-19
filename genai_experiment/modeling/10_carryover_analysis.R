# =============================================================
# 10_carryover_analysis.R
# Carryover / order-effect analysis for the GenAI experiment
#
# Responds to JGHE Reviewer 1: "The results presented do not take into
# account the cumulative effect resulting from double exposition."
#
# Three analyses:
#   1. Method x order interaction (the carryover test)
#   2. First-session-only between-subjects comparison (unconfounded, but
#      underpowered -> effect sizes with CIs, not p-values)
#   3. Wave 1 -> 2 -> 3 decomposition (where do the gains land?)
#
# Design note: this is a 2 x 2 crossover. Each participant does both methods,
# one per period. In such a design the differential carryover effect is
# estimated by the between-subject contrast of participant totals
# (wave 3 - wave 1) across the two sequences, which is algebraically identical
# to the method x order interaction in the long-format OLS. Both are reported.
#
# Outcomes are 5-point Likert items coded -2 (strongly disagree) .. +2
# (strongly agree), so a change of 1.0 = one Likert category.
#
# Inputs : data/processed/survey_processed.csv  (built by 2_main_variable_construction.R)
# Outputs: reports/models/carryover/*.tex, *.csv
# =============================================================

# ----------------------
# 0. Load Packages & Setup
# ----------------------
pacman::p_load(
  dplyr,
  tidyr,
  readr,
  purrr,
  broom,
  stringr,
  boot
)

set.seed(20250411)  # session window closed 11 April 2025

N_BOOT <- 2000

processed_data_path <- "data/processed/survey_processed.csv"
out_dir <- "reports/models/carryover/"
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

outcomes <- c(
  plan   = "Willingness to participate",
  attach = "Place attachment",
  walk   = "Walking support",
  cycle  = "Cycling support"
)

# ----------------------
# 1. Data Loading & Validation
# ----------------------
if (!file.exists(processed_data_path)) stop("Processed data not found!")
df <- read_csv(processed_data_path, show_col_types = FALSE)

required_vars <- c(
  "ResponseId", "genai_first", "street_first", "location_1", "location_2",
  paste0(rep(names(outcomes), each = 3), "_wave_", 1:3),
  paste0(rep(names(outcomes), each = 3), c("_diff_1", "_diff_2", "_diff_total")),
  "Gender", "Planning_Knowledge_num", "AI_Experience_num", "AI_Tool_Usage_num"
)
missing_cols <- setdiff(required_vars, names(df))
if (length(missing_cols) > 0) {
  stop(paste("Missing columns:", paste(missing_cols, collapse = ", ")))
}

cat("Participants analysed:", nrow(df), "\n")
cat("GenAI first:", sum(df$genai_first), " Conventional first:", sum(!df$genai_first), "\n")

# Long format: one row per participant-session (2 rows per participant).
# period 1 = wave 1 -> wave 2, period 2 = wave 2 -> wave 3.
make_long <- function(data, outcome) {
  bind_rows(
    data %>% transmute(
      ResponseId,
      period   = 1L,
      change   = .data[[paste0(outcome, "_diff_1")]],
      method   = if_else(genai_first, "GenAI", "Conventional"),
      location = location_1,
      genai_first, street_first, Gender,
      Planning_Knowledge_num, AI_Experience_num, AI_Tool_Usage_num
    ),
    data %>% transmute(
      ResponseId,
      period   = 2L,
      change   = .data[[paste0(outcome, "_diff_2")]],
      method   = if_else(genai_first, "Conventional", "GenAI"),
      location = location_2,
      genai_first, street_first, Gender,
      Planning_Knowledge_num, AI_Experience_num, AI_Tool_Usage_num
    )
  ) %>%
    mutate(
      # GenAI = 1 / Conventional = 0 ; second = 1 / first = 0
      method = factor(method, levels = c("Conventional", "GenAI")),
      second = as.integer(period == 2L),
      location = factor(location, levels = c("Park", "Street"))
    ) %>%
    arrange(ResponseId, period)
}

long_list <- map(set_names(names(outcomes)), ~make_long(df, .x))

# ----------------------
# 2. Shared helpers
# ----------------------

# Percentile bootstrap CI for an arbitrary statistic of a numeric vector
boot_ci_stat <- function(x, stat = mean, R = N_BOOT) {
  x <- x[!is.na(x)]
  reps <- replicate(R, stat(sample(x, length(x), replace = TRUE)))
  as.numeric(quantile(reps, c(0.025, 0.975), na.rm = TRUE))
}

# Hedges' g for two independent samples, with percentile bootstrap CI
hedges_g <- function(x, y, R = N_BOOT) {
  x <- x[!is.na(x)]; y <- y[!is.na(y)]
  g_of <- function(a, b) {
    n1 <- length(a); n2 <- length(b)
    s_pool <- sqrt(((n1 - 1) * var(a) + (n2 - 1) * var(b)) / (n1 + n2 - 2))
    if (!is.finite(s_pool) || s_pool == 0) return(NA_real_)
    d <- (mean(a) - mean(b)) / s_pool
    J <- 1 - 3 / (4 * (n1 + n2) - 9)   # small-sample bias correction
    d * J
  }
  est <- g_of(x, y)
  reps <- replicate(R, g_of(sample(x, length(x), replace = TRUE),
                            sample(y, length(y), replace = TRUE)))
  ci <- as.numeric(quantile(reps, c(0.025, 0.975), na.rm = TRUE))
  c(g = est, lo = ci[1], hi = ci[2])
}

# Cohen's d_z for a paired / one-sample difference, with percentile bootstrap CI
cohens_dz <- function(d, R = N_BOOT) {
  d <- d[!is.na(d)]
  dz_of <- function(v) if (sd(v) == 0) NA_real_ else mean(v) / sd(v)
  est <- dz_of(d)
  reps <- replicate(R, dz_of(sample(d, length(d), replace = TRUE)))
  ci <- as.numeric(quantile(reps, c(0.025, 0.975), na.rm = TRUE))
  c(dz = est, lo = ci[1], hi = ci[2])
}

# Cluster (participant-level) bootstrap for an lm on the long data.
# Resampling whole participants respects the repeated-measures structure.
cluster_boot_lm <- function(formula, data, id_col = "ResponseId", R = N_BOOT) {
  fit <- lm(formula, data = data)
  coef_names <- names(coef(fit))
  ids <- unique(data[[id_col]])
  split_rows <- split(seq_len(nrow(data)), data[[id_col]])
  reps <- matrix(NA_real_, nrow = R, ncol = length(coef_names),
                 dimnames = list(NULL, coef_names))
  for (b in seq_len(R)) {
    draw <- sample(ids, length(ids), replace = TRUE)
    rows <- unlist(split_rows[as.character(draw)], use.names = FALSE)
    bfit <- tryCatch(lm(formula, data = data[rows, , drop = FALSE]),
                     error = function(e) NULL)
    if (is.null(bfit)) next
    cf <- coef(bfit)
    reps[b, names(cf)[names(cf) %in% coef_names]] <-
      cf[names(cf)[names(cf) %in% coef_names]]
  }
  data.frame(
    term         = coef_names,
    estimate     = as.numeric(coef(fit)),
    boot_se      = apply(reps, 2, sd, na.rm = TRUE),
    boot_ci_lower = apply(reps, 2, quantile, probs = 0.025, na.rm = TRUE),
    boot_ci_upper = apply(reps, 2, quantile, probs = 0.975, na.rm = TRUE),
    row.names = NULL, stringsAsFactors = FALSE
  )
}

fmt <- function(x, k = 2) formatC(x, format = "f", digits = k)
fmt_ci <- function(est, lo, hi, k = 2) {
  sprintf("%s [%s, %s]", fmt(est, k), fmt(lo, k), fmt(hi, k))
}
fmt_p <- function(p) ifelse(p < 0.001, "$<$0.001", formatC(p, format = "f", digits = 3))

# Minimal LaTeX table writer (booktabs), matching the style used elsewhere
# in reports/models/.
#
# FITTING THE PAGE (issue #22). Issue #22 was the first document to typeset any
# of these, and every one of them ran off the right margin: wave_decomposition
# by 175pt and position_effect by 178pt on a 397pt text block,
# first_session_comparison by 80pt. Two fits, chosen per table, because one size
# does not serve both shapes:
#   rotate = TRUE  -- sidewaystable, for the seven-column tables. Landscape gives
#                     654pt, so they set at full footnotesize with nothing shrunk
#                     and nothing wrapped. Matches the two regression tables.
#   fit    = TRUE  -- \resizebox to \textwidth, for the five-column tables, which
#                     overrun by little enough to scale to ~8pt.
# What was tried and rejected: tabularx X columns. They fit, but they break a
# cell mid-interval, so "0.39 [0.06, 0.73]" sets across two lines and the reader
# cannot scan a column of intervals. Legibility of the numbers wins.
write_latex_table <- function(tab, caption, label, note, filename,
                              align = NULL, font = "\\footnotesize",
                              rotate = FALSE, fit = FALSE) {
  ncols <- ncol(tab)
  if (is.null(align)) align <- paste0("l", strrep("c", ncols - 1))
  env <- if (rotate) "sidewaystable" else "table"
  body <- c(
    paste0("\\begin{tabular}{", align, "}"),
    "\\toprule",
    paste0(paste(names(tab), collapse = " & "), " \\\\"),
    "\\midrule",
    apply(tab, 1, function(r) paste0(paste(r, collapse = " & "), " \\\\")),
    "\\bottomrule",
    "\\end{tabular}"
  )
  if (fit) body <- c("\\resizebox{\\textwidth}{!}{%", body, "}")
  lines <- c(
    if (rotate) "\\begin{sidewaystable}[htbp]" else "\\begin{table}[H]",
    "\\centering",
    paste0("\\caption{", caption, "}"),
    paste0("\\label{", label, "}"),
    font,
    body,
    "\\begin{minipage}{\\textwidth}",
    "\\footnotesize",
    paste0("\\textit{Notes:} ", note),
    "\\end{minipage}",
    paste0("\\end{", env, "}")
  )
  writeLines(lines, file.path(out_dir, filename))
  cat("Saved:", filename, "\n")
}

# =============================================================
# ANALYSIS 1: Method x order interaction (the carryover test)
# =============================================================
cat("\n=== Analysis 1: method x order interaction ===\n")

f_unadj <- change ~ method * second
f_adj   <- change ~ method * second + location + Gender +
  Planning_Knowledge_num + AI_Experience_num + AI_Tool_Usage_num

interaction_results <- imap_dfr(long_list, function(dat, out) {
  bind_rows(
    cluster_boot_lm(f_unadj, dat) %>% mutate(spec = "Unadjusted"),
    cluster_boot_lm(f_adj, dat)   %>% mutate(spec = "Adjusted")
  ) %>% mutate(outcome = out, .before = 1)
})
write_csv(interaction_results, file.path(out_dir, "interaction_models.csv"))

# Exact between-subjects carryover test: contrast of participant totals
# (wave 3 - wave 1) across sequences. Welch two-sample t-test.
carryover_test <- imap_dfr(set_names(names(outcomes)), function(out, .) {
  tot <- df[[paste0(out, "_diff_total")]]
  g1 <- tot[df$genai_first]     # GenAI -> Conventional
  g0 <- tot[!df$genai_first]    # Conventional -> GenAI
  tt <- t.test(g0, g1)          # interaction sign: Conv-first minus GenAI-first
  g  <- hedges_g(g0, g1)
  data.frame(
    outcome = out,
    n_genai_first = length(g1), n_conv_first = length(g0),
    mean_total_genai_first = mean(g1), sd_total_genai_first = sd(g1),
    mean_total_conv_first  = mean(g0), sd_total_conv_first  = sd(g0),
    diff = mean(g0) - mean(g1),
    ci_lower = tt$conf.int[1], ci_upper = tt$conf.int[2],
    t_stat = unname(tt$statistic), df_welch = unname(tt$parameter),
    p_value = tt$p.value,
    hedges_g = unname(g["g"]), g_ci_lower = unname(g["lo"]), g_ci_upper = unname(g["hi"]),
    stringsAsFactors = FALSE
  )
})
write_csv(carryover_test, file.path(out_dir, "carryover_sequence_test.csv"))

# Cell means underlying the interaction
cell_means <- imap_dfr(long_list, function(dat, out) {
  dat %>%
    group_by(method, period) %>%
    summarise(n = n(), mean = mean(change), sd = sd(change), .groups = "drop") %>%
    mutate(outcome = out, .before = 1)
})
write_csv(cell_means, file.path(out_dir, "cell_means.csv"))

# --- LaTeX: interaction models -------------------------------------------
int_tab <- interaction_results %>%
  filter(term %in% c("methodGenAI", "second", "methodGenAI:second")) %>%
  mutate(
    term_clean = recode(term,
      "methodGenAI"        = "GenAI (vs.\\ Conventional)",
      "second"             = "Second session (vs.\\ first)",
      "methodGenAI:second" = "GenAI $\\times$ Second session"),
    cell = fmt_ci(estimate, boot_ci_lower, boot_ci_upper),
    col  = unname(outcomes[outcome])
  ) %>%
  select(spec, term_clean, col, cell) %>%
  pivot_wider(names_from = col, values_from = cell) %>%
  arrange(match(spec, c("Unadjusted", "Adjusted")),
          match(term_clean, c("GenAI (vs.\\ Conventional)",
                              "Second session (vs.\\ first)",
                              "GenAI $\\times$ Second session")))

int_mat <- as.matrix(int_tab %>% select(-spec))
panel_row <- function(label) c(paste0("\\textit{", label, "}"), rep("", ncol(int_mat) - 1))
int_body <- as.data.frame(
  rbind(panel_row("Panel A: unadjusted"), int_mat[1:3, ],
        panel_row("Panel B: adjusted"),   int_mat[4:6, ]),
  stringsAsFactors = FALSE
)
names(int_body) <- c("Term", unname(outcomes))

write_latex_table(
  int_body,
  caption = "Method $\\times$ order interaction: test for carryover between the two sessions",
  label = "tab:carryover_interaction",
  note = paste(
    "OLS on student-session change scores (110 observations, $N=55$). Coefficients",
    "with 95\\% student-level cluster bootstrap CIs (2{,}000 resamples of whole",
    "students). Outcomes are $-2$ to $+2$ Likert items, so 1.00 = one response",
    "category. Panel B adds site, gender, planning knowledge, AI familiarity and AI",
    "image-tool use. The interaction is the differential carryover effect."),
  filename = "carryover_interaction.tex",
  fit = TRUE                    # supplementary; 5 columns of long labels
)

# --- LaTeX: exact sequence (carryover) test ------------------------------
seq_tab <- carryover_test %>%
  transmute(
    Outcome = unname(outcomes[outcome]),
    `GenAI first M (SD)` = sprintf("%s (%s)", fmt(mean_total_genai_first), fmt(sd_total_genai_first)),
    `Conv.\\ first M (SD)` = sprintf("%s (%s)", fmt(mean_total_conv_first), fmt(sd_total_conv_first)),
    `Difference [95\\% CI]` = fmt_ci(diff, ci_lower, ci_upper),
    `$t$ (df)` = sprintf("%s (%s)", fmt(t_stat), fmt(df_welch, 1)),
    `$p$` = fmt_p(p_value),
    `Hedges' $g$ [95\\% CI]` = fmt_ci(hedges_g, g_ci_lower, g_ci_upper)
  )

write_latex_table(
  seq_tab,
  caption = "Exact carryover test: total change (wave 3 $-$ wave 1) by method order",
  label = "tab:carryover_sequence",
  note = paste(
    "Welch two-sample $t$-tests comparing students who received the GenAI agent",
    "first ($n=28$) with those who received the traditional facilitator first ($n=27$).",
    "Difference = Conventional-first minus GenAI-first, i.e.\\ the differential",
    "carryover effect, identical to the GenAI $\\times$ Second session interaction in",
    "Table~\\ref{tab:carryover_interaction}. Hedges' $g$ uses the pooled SD with the",
    "small-sample correction; its CI is a percentile bootstrap (2{,}000 resamples)."),
  filename = "carryover_sequence_test.tex",
  rotate = TRUE                 # supplementary; 7 columns
)

# --- LaTeX: cell means ---------------------------------------------------
cell_tab <- cell_means %>%
  mutate(cell = sprintf("%s (%s)", fmt(mean), fmt(sd)),
         colname = paste0(method, ", session ", period),
         Outcome = unname(outcomes[outcome])) %>%
  select(Outcome, colname, cell) %>%
  pivot_wider(names_from = colname, values_from = cell) %>%
  select(Outcome,
         `GenAI, session 1`, `Conventional, session 2`,
         `Conventional, session 1`, `GenAI, session 2`)
names(cell_tab) <- c("Outcome", "GenAI 1st", "Conv.\\ 2nd", "Conv.\\ 1st", "GenAI 2nd")

write_latex_table(
  cell_tab,
  caption = "Mean change per session by method and position (the four crossover cells)",
  label = "tab:carryover_cells",
  note = paste(
    "Mean (SD) within-session change on the $-2$ to $+2$ Likert scale. Columns 2--3 are",
    "the GenAI-first sequence ($n=28$); columns 4--5 are the Conventional-first",
    "sequence ($n=27$). Session-1 changes are measured wave 1 $\\rightarrow$ wave 2;",
    "session-2 changes wave 2 $\\rightarrow$ wave 3."),
  filename = "carryover_cell_means.tex"
)

# =============================================================
# ANALYSIS 2: First-session-only between-subjects comparison
# =============================================================
cat("\n=== Analysis 2: first-session-only comparison ===\n")

first_session <- imap_dfr(set_names(names(outcomes)), function(out, .) {
  d1 <- df[[paste0(out, "_diff_1")]]
  a <- d1[df$genai_first]    # GenAI in session 1
  b <- d1[!df$genai_first]   # Conventional in session 1
  tt <- t.test(a, b)
  g  <- hedges_g(a, b)
  data.frame(
    outcome = out,
    n_genai = length(a), mean_genai = mean(a), sd_genai = sd(a),
    n_conv  = length(b), mean_conv  = mean(b), sd_conv  = sd(b),
    diff = mean(a) - mean(b),
    ci_lower = tt$conf.int[1], ci_upper = tt$conf.int[2],
    t_stat = unname(tt$statistic), df_welch = unname(tt$parameter),
    p_value = tt$p.value,
    hedges_g = unname(g["g"]), g_ci_lower = unname(g["lo"]), g_ci_upper = unname(g["hi"]),
    stringsAsFactors = FALSE
  )
})
write_csv(first_session, file.path(out_dir, "first_session_comparison.csv"))

# Smallest effect detectable at 80% power with these arm sizes
mdes <- power.t.test(n = 27, sig.level = 0.05, power = 0.80,
                     type = "two.sample")$delta
cat("Minimum detectable standardised effect (n=27/arm, 80% power):",
    round(mdes, 3), "\n")

fs_tab <- first_session %>%
  transmute(
    Outcome = unname(outcomes[outcome]),
    `GenAI M (SD)` = sprintf("%s (%s)", fmt(mean_genai), fmt(sd_genai)),
    `Conventional M (SD)` = sprintf("%s (%s)", fmt(mean_conv), fmt(sd_conv)),
    `Difference [95\\% CI]` = fmt_ci(diff, ci_lower, ci_upper),
    `Hedges' $g$ [95\\% CI]` = fmt_ci(hedges_g, g_ci_lower, g_ci_upper)
  )

write_latex_table(
  fs_tab,
  caption = "Unconfounded between-students comparison using each student's first session only",
  label = "tab:first_session",
  note = paste0(
    "Change from wave 1 to wave 2 only, before any exposure to the second method.",
    " GenAI $n=28$, Conventional $n=27$; difference is GenAI minus Conventional.",
    " Hedges' $g$ is small-sample corrected, with a percentile bootstrap CI",
    " (2{,}000 resamples). These are effect sizes; the design has 80\\% power",
    " to detect only effects of ", fmt(mdes, 2), " or larger, in either direction.")
  ,
  filename = "first_session_comparison.tex",
  fit = TRUE                    # 5 columns, 80pt over -- scales to ~8pt
)

# =============================================================
# ANALYSIS 3: Wave 1 -> 2 -> 3 decomposition
# =============================================================
cat("\n=== Analysis 3: wave decomposition ===\n")

wave_decomp <- imap_dfr(set_names(names(outcomes)), function(out, .) {
  w1 <- df[[paste0(out, "_wave_1")]]
  w2 <- df[[paste0(out, "_wave_2")]]
  w3 <- df[[paste0(out, "_wave_3")]]
  d1 <- w2 - w1; d2 <- w3 - w2; dt <- w3 - w1
  ci1 <- boot_ci_stat(d1); ci2 <- boot_ci_stat(d2); cit <- boot_ci_stat(dt)
  # Is the second-session change systematically smaller than the first?
  tt <- t.test(d1, d2, paired = TRUE)
  dz1 <- cohens_dz(d1); dz2 <- cohens_dz(d2); dzt <- cohens_dz(dt)
  data.frame(
    outcome = out,
    mean_w1 = mean(w1), sd_w1 = sd(w1),
    mean_w2 = mean(w2), sd_w2 = sd(w2),
    mean_w3 = mean(w3), sd_w3 = sd(w3),
    ceiling_w1 = mean(w1 == 2), ceiling_w2 = mean(w2 == 2), ceiling_w3 = mean(w3 == 2),
    delta1 = mean(d1), d1_lo = ci1[1], d1_hi = ci1[2],
    dz1 = unname(dz1["dz"]), dz1_lo = unname(dz1["lo"]), dz1_hi = unname(dz1["hi"]),
    delta2 = mean(d2), d2_lo = ci2[1], d2_hi = ci2[2],
    dz2 = unname(dz2["dz"]), dz2_lo = unname(dz2["lo"]), dz2_hi = unname(dz2["hi"]),
    delta_total = mean(dt), dt_lo = cit[1], dt_hi = cit[2],
    dzt = unname(dzt["dz"]), dzt_lo = unname(dzt["lo"]), dzt_hi = unname(dzt["hi"]),
    d1_minus_d2 = mean(d1 - d2),
    d1md2_ci_lower = tt$conf.int[1], d1md2_ci_upper = tt$conf.int[2],
    d1md2_t = unname(tt$statistic), d1md2_df = unname(tt$parameter),
    d1md2_p = tt$p.value,
    stringsAsFactors = FALSE
  )
})
write_csv(wave_decomp, file.path(out_dir, "wave_decomposition.csv"))

# Same decomposition split by sequence
wave_by_sequence <- imap_dfr(set_names(names(outcomes)), function(out, .) {
  map_dfr(c(TRUE, FALSE), function(gf) {
    sub <- df[df$genai_first == gf, ]
    d1 <- sub[[paste0(out, "_diff_1")]]
    d2 <- sub[[paste0(out, "_diff_2")]]
    c1 <- boot_ci_stat(d1); c2 <- boot_ci_stat(d2)
    data.frame(
      outcome = out,
      sequence = if (gf) "GenAI -> Conventional" else "Conventional -> GenAI",
      n = nrow(sub),
      delta1 = mean(d1), d1_lo = c1[1], d1_hi = c1[2],
      delta2 = mean(d2), d2_lo = c2[1], d2_hi = c2[2],
      stringsAsFactors = FALSE
    )
  })
})
write_csv(wave_by_sequence, file.path(out_dir, "wave_by_sequence.csv"))

wd_tab <- wave_decomp %>%
  transmute(
    Outcome = unname(outcomes[outcome]),
    `Wave 1` = sprintf("%s (%s)", fmt(mean_w1), fmt(sd_w1)),
    `Wave 2` = sprintf("%s (%s)", fmt(mean_w2), fmt(sd_w2)),
    `Wave 3` = sprintf("%s (%s)", fmt(mean_w3), fmt(sd_w3)),
    `$\\Delta_1$ (1$\\rightarrow$2) [95\\% CI]` = fmt_ci(delta1, d1_lo, d1_hi),
    `$\\Delta_2$ (2$\\rightarrow$3) [95\\% CI]` = fmt_ci(delta2, d2_lo, d2_hi),
    `$\\Delta_{total}$ (1$\\rightarrow$3) [95\\% CI]` = fmt_ci(delta_total, dt_lo, dt_hi)
  )

write_latex_table(
  wd_tab,
  caption = "Wave-by-wave decomposition of change ($N=55$)",
  label = "tab:wave_decomposition",
  note = paste(
    "Mean (SD) on the $-2$ to $+2$ Likert scale at each wave (1 before, 2 between,",
    "3 after both sessions). $\\Delta$ columns give mean within-student change with",
    "95\\% percentile bootstrap CIs (2{,}000 resamples). $\\Delta_1$ and $\\Delta_2$ pool",
    "both methods by position, so differences reflect position, not method."),
  filename = "wave_decomposition.tex",
  rotate = TRUE                 # 7 columns, 175pt over -- needs landscape
)

pos_tab <- wave_decomp %>%
  transmute(
    Outcome = unname(outcomes[outcome]),
    `$\\Delta_1$ ($d_z$ [95\\% CI])` = fmt_ci(dz1, dz1_lo, dz1_hi),
    `$\\Delta_2$ ($d_z$ [95\\% CI])` = fmt_ci(dz2, dz2_lo, dz2_hi),
    `$\\Delta_1-\\Delta_2$ [95\\% CI]` = fmt_ci(d1_minus_d2, d1md2_ci_lower, d1md2_ci_upper),
    `$t$ (df)` = sprintf("%s (%s)", fmt(d1md2_t), fmt(d1md2_df, 0)),
    `$p$` = fmt_p(d1md2_p),
    `\\% at ceiling, W1/W2/W3` = sprintf("%s / %s / %s",
      fmt(100 * ceiling_w1, 0), fmt(100 * ceiling_w2, 0), fmt(100 * ceiling_w3, 0))
  )

write_latex_table(
  pos_tab,
  caption = "Is the second session's change smaller? Position effects and ceiling occupancy",
  label = "tab:position_effect",
  note = paste(
    "$d_z$ = within-student standardised change (mean change divided by the SD of",
    "the change), with percentile bootstrap CIs (2{,}000 resamples).",
    "$\\Delta_1-\\Delta_2$ is a paired $t$-test of each student's first-session",
    "change against their own second; positive means the second session added less.",
    "``\\% at ceiling'' is the share already answering ``Strongly agree'' ($+2$)."),
  filename = "position_effect.tex",
  rotate = TRUE                 # 7 columns, 178pt over -- needs landscape
)

seq_split_tab <- wave_by_sequence %>%
  transmute(
    Outcome = unname(outcomes[outcome]),
    Sequence = str_replace(sequence, "->", "$\\\\rightarrow$"),
    `$n$` = n,
    `$\\Delta_1$ [95\\% CI]` = fmt_ci(delta1, d1_lo, d1_hi),
    `$\\Delta_2$ [95\\% CI]` = fmt_ci(delta2, d2_lo, d2_hi)
  ) %>%
  mutate(Outcome = if_else(duplicated(Outcome), "", Outcome))

write_latex_table(
  seq_split_tab,
  caption = "Wave decomposition split by method order",
  label = "tab:wave_by_sequence",
  note = paste(
    "Mean within-session change with 95\\% percentile bootstrap confidence intervals",
    "(2{,}000 resamples), separately for the two counterbalanced sequences.",
    "$\\Delta_1$ = wave 1 $\\rightarrow$ wave 2 (first method), $\\Delta_2$ = wave 2",
    "$\\rightarrow$ wave 3 (second method)."),
  filename = "wave_by_sequence.tex"
)

# ----------------------
# 4. Console summary
# ----------------------
cat("\n---------------- SUMMARY ----------------\n")
cat("\n[1] Method x order interaction (unadjusted, cluster bootstrap 95% CI):\n")
print(interaction_results %>%
        filter(spec == "Unadjusted", term == "methodGenAI:second") %>%
        select(outcome, estimate, boot_ci_lower, boot_ci_upper) %>%
        mutate(across(where(is.numeric), ~round(., 3))))
cat("\n[1b] Exact carryover test on total change:\n")
print(carryover_test %>%
        select(outcome, mean_total_genai_first, mean_total_conv_first, diff,
               ci_lower, ci_upper, t_stat, df_welch, p_value, hedges_g,
               g_ci_lower, g_ci_upper) %>%
        mutate(across(where(is.numeric), ~round(., 3))))
cat("\n[2] First session only:\n")
print(first_session %>%
        select(outcome, mean_genai, mean_conv, diff, ci_lower, ci_upper,
               hedges_g, g_ci_lower, g_ci_upper, p_value) %>%
        mutate(across(where(is.numeric), ~round(., 3))))
cat("\n[3] Wave decomposition:\n")
print(wave_decomp %>%
        select(outcome, mean_w1, mean_w2, mean_w3, delta1, d1_lo, d1_hi,
               delta2, d2_lo, d2_hi, delta_total, dt_lo, dt_hi,
               d1_minus_d2, d1md2_ci_lower, d1md2_ci_upper, d1md2_p,
               ceiling_w1, ceiling_w2, ceiling_w3) %>%
        mutate(across(where(is.numeric), ~round(., 3))))
cat("\n[3b] By sequence:\n")
print(wave_by_sequence %>% mutate(across(where(is.numeric), ~round(., 3))))

cat("\nCarryover analysis complete. Outputs saved to", out_dir, "\n")
