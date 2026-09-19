# =============================================================
# 12_primary_w1w2.R
# Primary estimand: change from wave 1 to wave 2 (a single first exposure)
#
# Issue #9 moved the paper's primary estimand from W1 -> W3 (both sessions
# combined) to W1 -> W2 (the first session only). On that estimand the mode
# contrast is BETWEEN participants: those with genai_first == TRUE met the
# GenAI agent in session 1, the rest met the human facilitator. Nobody has
# been exposed to the other method yet, so the contrast is carryover-free.
#
# Note what the two experimental indicators mean on this estimand, because it
# is not what they mean on W1 -> W3:
#   genai_first  = MODE of the first session (GenAI vs. Conventional)
#   street_first = SITE of the first session (Street vs. Park)
# On the total-change models in 4_regression_analysis.R the same two terms are
# order effects. Same columns, different estimand, different meaning.
#
# Sign convention throughout: GenAI minus Conventional (issue #15).
#
# ---- Choice of primary specification (author decision, issue #41) ----------
# The primary model is an ANCOVA: mode + site + the participant's own wave-1
# score. It is NOT the 15-covariate set carried over from the W1 -> W3 models.
#
# That set was carried in order to ESTIMATE subgroup terms, and issue #45
# retired subgroup interpretation entirely, so its purpose is gone -- while
# fitting it at N=55 leaves 39 residual df and inflates the mode interval past
# zero on both headline outcomes. It is not buying fit: on cycling support the
# full set reaches R^2 = 0.204 against the ANCOVA's 0.448 while spending 12
# more parameters. Adjusting for baseline instead is the standard treatment of
# change scores (Vickers & Altman 2001), handles regression to the mean, and
# speaks to the ceiling occupancy documented in tab:position_effect.
#
# Both alternatives are still fitted and reported as robustness panels, so the
# reader sees the mode contrast under all three specifications. Baseline is
# balanced across arms (all p > 0.20), so this is an efficiency choice, not a
# confounding correction -- but the GenAI arm did start 0.27 higher on cycling
# support, which is why the ANCOVA shrinks that contrast while tightening it.
#
# Inference follows the standard set by issue #45: a coefficient counts as
# significant iff its bootstrap CI excludes zero AND its dropout -- the share
# of resamples in which the term was inestimable -- is below 1%. Parametric
# OLS p-values are not used and no p-value ladder is reported. Where dropout
# reaches 1% the interval is refused rather than computed from the surviving
# resamples, because conditioning on a rare category appearing biases it.
#
# Inputs : data/processed/survey_processed.csv (built by 2_main_variable_construction.R;
#          the W1 -> W2 change scores are the *_diff_1 columns, lines 84-87 there)
# Outputs: reports/models/main_test/primary_w1w2_results.csv
#          reports/models/main_test/primary_w1w2.tex        (main text)
#          reports/models/main_test/primary_w1w2_full.tex   (supplementary)
# =============================================================

# ----------------------
# 0. Load Packages & Setup
# ----------------------
pacman::p_load(
  dplyr,
  tidyr,
  readr,
  purrr,
  forcats,
  stringr
)

set.seed(20250411)  # session window closed 11 April 2025

N_BOOT <- 2000
DROPOUT_MAX <- 0.01   # issue #45: refuse an interval at or above this

processed_data_path <- "data/processed/survey_processed.csv"
out_dir <- "reports/models/main_test/"
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

# Full names are used in the column headers, matching reports/models/carryover/,
# so the note carries no outcome glossary. The short forms remain for any use
# that needs a narrow label.
outcomes <- c(
  plan   = "Willingness",
  attach = "Attachment",
  walk   = "Walking",
  cycle  = "Cycling"
)
outcomes_long <- c(
  plan   = "Willingness to participate",
  attach = "Place attachment",
  walk   = "Walking support",
  cycle  = "Cycling support"
)
outcome_key <- paste0(
  "Columns are the four outcomes: ",
  paste(sprintf("%s = %s", unname(outcomes), tolower(unname(outcomes_long))),
        collapse = "; "), "."
)

SPEC_LEVELS <- c("Unadjusted", "Primary", "Full covariates")

# ----------------------
# 1. Data Loading & Validation
# ----------------------
if (!file.exists(processed_data_path)) stop("Processed data not found!")
df <- read_csv(processed_data_path, show_col_types = FALSE)

covariates <- c(
  "Gender", "Race_Ethnicity", "Planning_Knowledge_num", "AI_Experience_num",
  "AI_Tool_Usage_num", "genai_first", "street_first", "Primary_Transportation",
  "Hometown"
)
required_vars <- c(
  paste0(names(outcomes), "_diff_1"),
  paste0(names(outcomes), "_wave_1"),
  covariates
)
missing_cols <- setdiff(required_vars, names(df))
if (length(missing_cols) > 0) {
  stop(paste("Missing columns:", paste(missing_cols, collapse = ", ")))
}

# Same reference level as 4_regression_analysis.R
df <- df %>%
  mutate(
    Race_Ethnicity = fct_relevel(factor(Race_Ethnicity), "White"),
    Primary_Transportation = factor(Primary_Transportation),
    Hometown = factor(Hometown),
    Gender = factor(Gender)
  )

cat("Participants analysed:", nrow(df), "\n")
cat("GenAI in session 1:", sum(df$genai_first),
    " Conventional in session 1:", sum(!df$genai_first), "\n")

# ----------------------
# 2. Model specifications
# ----------------------
# Unadjusted: the mode contrast alone. Its genai_firstTRUE coefficient is
# algebraically the difference in mean first-session change between the two
# arms, so it must equal the `diff` column of
# reports/models/carryover/first_session_comparison.csv exactly. Checked below.
f_unadj <- function(out) {
  as.formula(paste0(out, "_diff_1 ~ genai_first"))
}

# Primary: ANCOVA on the participant's own wave-1 score, plus the site of the
# first session. Both right-hand-side terms other than baseline are randomised
# design factors, so this adjusts for the one thing the design does not
# control -- where each participant started.
f_primary <- function(out) {
  as.formula(paste0(out, "_diff_1 ~ genai_first + street_first + ", out, "_wave_1"))
}

# Robustness: the covariate set inherited from the W1 -> W3 models in
# 4_regression_analysis.R, reported so the reader can see what it does. Hometown
# stays in as an unreported control per issue #39, despite its n=2 rural
# reference level -- the dropout column is what flags the consequences of that,
# rather than dropping the term.
f_full <- function(out) {
  as.formula(paste0(
    out, "_diff_1 ~ Gender + Race_Ethnicity + Planning_Knowledge_num + ",
    "AI_Experience_num + AI_Tool_Usage_num + genai_first + street_first + ",
    "Primary_Transportation + Hometown"
  ))
}

# ----------------------
# 3. Bootstrap under the issue #45 standard
# ----------------------
# One row per participant on this estimand, so ordinary case resampling is
# correct -- no clustering is needed (contrast 10_carryover_analysis.R, whose
# long format puts two rows per participant).
#
# Dropout is counted per term: the share of resamples in which the term was
# absent from the refit or came back NA (a level vanished from the resample,
# or the refit went collinear). Fits that error out count as dropout for
# every term.
boot_lm <- function(formula, data, R = N_BOOT) {
  fit <- lm(formula, data = data)
  coef_names <- names(coef(fit))
  reps <- matrix(NA_real_, nrow = R, ncol = length(coef_names),
                 dimnames = list(NULL, coef_names))
  n <- nrow(data)
  for (b in seq_len(R)) {
    idx <- sample(n, n, replace = TRUE)
    bfit <- tryCatch(lm(formula, data = data[idx, , drop = FALSE]),
                     error = function(e) NULL)
    if (is.null(bfit)) next
    cf <- coef(bfit)
    keep <- intersect(names(cf), coef_names)
    reps[b, keep] <- cf[keep]
  }

  dropout <- apply(reps, 2, function(x) mean(is.na(x)))
  lo <- apply(reps, 2, quantile, probs = 0.025, na.rm = TRUE)
  hi <- apply(reps, 2, quantile, probs = 0.975, na.rm = TRUE)

  # Refuse the interval where the term dropped out too often, rather than
  # quoting one computed from the resamples that happened to retain it.
  unstable <- dropout >= DROPOUT_MAX
  lo[unstable] <- NA_real_
  hi[unstable] <- NA_real_

  data.frame(
    term = coef_names,
    estimate = as.numeric(coef(fit)),
    boot_ci_lower = as.numeric(lo),
    boot_ci_upper = as.numeric(hi),
    dropout = as.numeric(dropout),
    stable = !unstable,
    # The whole inference rule, in one column.
    significant = !unstable & !is.na(lo) & !is.na(hi) &
      ((lo > 0 & hi > 0) | (lo < 0 & hi < 0)),
    n_obs = nobs(fit),
    r_squared = summary(fit)$r.squared,
    df_residual = df.residual(fit),
    row.names = NULL, stringsAsFactors = FALSE
  )
}

results <- imap_dfr(set_names(names(outcomes)), function(out, .) {
  bind_rows(
    boot_lm(f_unadj(out), df)   %>% mutate(spec = "Unadjusted"),
    boot_lm(f_primary(out), df) %>% mutate(spec = "Primary"),
    boot_lm(f_full(out), df)    %>% mutate(spec = "Full covariates")
  ) %>% mutate(outcome = out, .before = 1)
}) %>%
  mutate(spec = factor(spec, levels = SPEC_LEVELS))

write_csv(results, file.path(out_dir, "primary_w1w2_results.csv"))

# ----------------------
# 4. Identity check against the carryover script
# ----------------------
# The unadjusted mode contrast is the same quantity Analysis 2 of
# 10_carryover_analysis.R computes as a Welch test. If these disagree, one of
# the two constructions has the sign or the arm assignment wrong.
fs_path <- "reports/models/carryover/first_session_comparison.csv"
if (file.exists(fs_path)) {
  fs <- read_csv(fs_path, show_col_types = FALSE)
  check <- results %>%
    filter(spec == "Unadjusted", term == "genai_firstTRUE") %>%
    select(outcome, ols_diff = estimate) %>%
    left_join(fs %>% select(outcome, welch_diff = diff), by = "outcome") %>%
    mutate(abs_gap = abs(ols_diff - welch_diff))
  cat("\n=== Identity check: unadjusted OLS contrast vs. first_session_comparison ===\n")
  print(check %>% mutate(across(where(is.numeric), ~round(., 6))))
  if (any(check$abs_gap > 1e-8, na.rm = TRUE)) {
    stop("Unadjusted W1->W2 mode contrast does not match first_session_comparison.csv")
  }
  cat("All four match to within 1e-8.\n")
} else {
  warning("first_session_comparison.csv not found - identity check skipped. ",
          "Run 10_carryover_analysis.R first.")
}

# ----------------------
# 5. Formatting helpers
# ----------------------
# Two decimals, matching the carryover tables in reports/models/carryover/.
fmt <- function(x, k = 2) formatC(x, format = "f", digits = k)

# Thousands separator in the LaTeX house style used by reports/models/
N_BOOT_TEX <- formatC(N_BOOT, format = "d", big.mark = "{,}")

# A cell carries the estimate, its interval, and a star iff the whole #45 rule
# passes. An unstable term shows its estimate and says so instead of quoting
# an interval conditioned on the category surviving.
fmt_cell <- function(est, lo, hi, stable, sig, k = 2) {
  ifelse(
    !stable,
    sprintf("%s [unstable]", fmt(est, k)),
    sprintf("%s [%s, %s]%s", fmt(est, k), fmt(lo, k), fmt(hi, k),
            ifelse(sig, "*", ""))
  )
}

term_labels <- c(
  "(Intercept)" = "Intercept",
  "genai_firstTRUE" = "GenAI (vs.\\ Conventional)",
  "street_firstTRUE" = "Site: Street (vs.\\ Park)",
  "GenderWoman" = "Gender: Woman",
  # Abbreviated to keep the Term column narrow; the note gives the full
  # category names alongside their cell sizes.
  "Race_EthnicityAsian" = "Race: Asian",
  "Race_EthnicityBlack or African American" = "Race: Black",
  "Race_EthnicityHispanic or Latino/a/x" = "Race: Hispanic",
  "Race_EthnicityAnother race or ethnicity not listed" = "Race: not listed",
  "Planning_Knowledge_num" = "Planning knowledge",
  "AI_Experience_num" = "AI familiarity",
  "AI_Tool_Usage_num" = "AI tool usage",
  "Primary_TransportationPersonal car" = "Transport: personal car",
  "Primary_TransportationRideshare (Uber/Lyft)" = "Transport: rideshare",
  "Primary_TransportationWalking" = "Transport: walking",
  "HometownSuburban" = "Hometown: suburban",
  "HometownUrban" = "Hometown: urban"
)
# The baseline term is outcome-specific (plan_wave_1, cycle_wave_1, ...) but
# means the same thing in every column, so it gets one shared row label.
label_term <- function(x) {
  ifelse(str_detect(x, "_wave_1$"), "Baseline (wave 1)",
         ifelse(x %in% names(term_labels), term_labels[x], x))
}

write_latex_table <- function(tab, caption, label, note, filename,
                              align = NULL, font = "\\footnotesize",
                              fit = FALSE) {
  ncols <- ncol(tab)
  # fit = TRUE wraps the tabular in \resizebox{\textwidth}: the first document
  # to typeset this table (issue #22) ran it 97pt off a 397pt text block, which
  # scales to ~8pt. tabularx X columns were tried and rejected -- they break a
  # cell mid-interval, so "0.35 [0.04, 0.68]" sets across two lines. Same call
  # as 10_carryover_analysis.R.
  if (is.null(align)) align <- paste0("l", strrep("c", ncols - 1))
  body <- c(
    paste0("\\begin{tabular}{", align, "}"),
    "\\toprule",
    paste0(paste(names(tab), collapse = " & "), " \\\\"),
    "\\midrule",
    # A row may be a full-width banner (a \multicolumn in the first cell with
    # the rest NA); those must not emit trailing ampersands, or the banner
    # widens the first column to its own length.
    apply(tab, 1, function(r) {
      r <- r[!is.na(r)]
      paste0(paste(r, collapse = " & "), " \\\\")
    }),
    "\\bottomrule",
    "\\end{tabular}"
  )
  if (fit) body <- c("\\resizebox{\\textwidth}{!}{%", body, "}")
  lines <- c(
    "\\begin{table}[H]",
    "\\centering",
    paste0("\\caption{", caption, "}"),
    paste0("\\label{", label, "}"),
    font,
    body,
    "\\begin{minipage}{\\textwidth}",
    "\\footnotesize",
    paste0("\\textit{Notes:} ", note),
    "\\end{minipage}",
    "\\end{table}"
  )
  writeLines(lines, file.path(out_dir, filename))
  cat("Saved:", filename, "\n")
}

# ----------------------
# 6. Main-text table: primary spec, with the other two as robustness panels
# ----------------------
# Which terms each panel shows: the primary panel reports its whole model
# (three terms), the robustness panels report the mode contrast only, since
# that is the quantity being checked for sensitivity.
panel_terms <- list(
  "Unadjusted"      = "genai_firstTRUE",
  "Primary"         = c("genai_firstTRUE", "street_firstTRUE", "_wave_1$"),
  "Full covariates" = "genai_firstTRUE"
)

build_panel <- function(spec_name) {
  keep <- panel_terms[[spec_name]]
  rows <- results %>%
    filter(spec == spec_name,
           term %in% keep | str_detect(term, "_wave_1$") & "_wave_1$" %in% keep) %>%
    mutate(
      term_clean = label_term(term),
      cell = fmt_cell(estimate, boot_ci_lower, boot_ci_upper, stable, significant),
      col = unname(outcomes[outcome])
    ) %>%
    select(term_clean, col, cell) %>%
    pivot_wider(names_from = col, values_from = cell)
  # Preserve the intended row order within the panel
  ord <- c("GenAI (vs.\\ Conventional)", "Site: Street (vs.\\ Park)",
           "Baseline (wave 1)")
  rows %>% slice(match(intersect(ord, rows$term_clean), term_clean))
}

panel_label <- function(lbl, ncol) {
  c(sprintf("\\multicolumn{%d}{l}{\\textit{%s}}", ncol, lbl),
    rep(NA_character_, ncol - 1))
}

panels <- list(
  list(lbl = "Panel A: primary (baseline-adjusted)", spec = "Primary"),
  list(lbl = "Panel B: unadjusted", spec = "Unadjusted"),
  list(lbl = "Panel C: full covariate set", spec = "Full covariates")
)

main_body <- map_dfr(panels, function(p) {
  blk <- build_panel(p$spec)
  hdr <- as.data.frame(as.list(panel_label(p$lbl, ncol(blk))), stringsAsFactors = FALSE)
  names(hdr) <- names(blk)
  bind_rows(hdr, blk)
})
names(main_body) <- c("Term", unname(outcomes_long))

write_latex_table(
  main_body,
  caption = paste(
    "Primary comparison: change over a single first exposure",
    "(wave 1 $\\rightarrow$ wave 2) by mode"),
  label = "tab:primary_w1w2",
  note = paste0(
    "OLS on each student's first-session change score, one observation per ",
    "student ($N=", nrow(df), "$); the GenAI group is $n=", sum(df$genai_first),
    "$ and the conventional group $n=", sum(!df$genai_first), "$, so the mode ",
    "contrast is between students and free of carryover. Entries are ",
    "coefficients with 95\\% bootstrap confidence intervals in brackets (",
    N_BOOT_TEX, " resamples, seeded). Outcomes are 5-point Likert items coded ",
    "$-2$ to $+2$, so 1.00 = one response category, and a positive coefficient ",
    "favours the GenAI agent. An asterisk marks an interval that excludes zero ",
    "where the term was estimable in over 99\\% of resamples; it is not a ",
    "$p$-value. Panel A adjusts for each student's own wave-1 score, the one ",
    "source of variation the counterbalancing does not control. Panels B and C ",
    "repeat the contrast unadjusted and under the 15-covariate set of ",
    "Table~\\ref{tab:bootstrap_results}, whose full coefficients are in the ",
    "supplementary material."),
  filename = "primary_w1w2.tex",
  fit = TRUE                    # 5 columns, 97pt over -- scales to ~8pt
)

# ----------------------
# 7. Supplementary table: full covariate set, all coefficients, with dropout
# ----------------------
full_rows <- results %>%
  filter(spec == "Full covariates") %>%
  mutate(
    cell = fmt_cell(estimate, boot_ci_lower, boot_ci_upper, stable, significant),
    term_clean = label_term(term),
    col = unname(outcomes[outcome])
  )

term_order <- full_rows %>%
  filter(outcome == "plan") %>%
  pull(term_clean)

full_tab <- full_rows %>%
  select(term_clean, col, cell) %>%
  pivot_wider(names_from = col, values_from = cell) %>%
  slice(match(term_order, term_clean))

# Dropout is a property of the design, not of the outcome -- the same terms
# drop out of the same resamples whichever change score is on the left. Report
# it once, from the planning model.
dropout_col <- full_rows %>%
  filter(outcome == "plan") %>%
  transmute(term_clean, dropout_pct = sprintf("%.1f", 100 * dropout))

full_tab <- full_tab %>%
  left_join(dropout_col, by = "term_clean") %>%
  rename(Term = term_clean, `Dropout (\\%)` = dropout_pct)

write_latex_table(
  full_tab,
  caption = paste(
    "Robustness: the full covariate set applied to the primary comparison",
    "(wave 1 $\\rightarrow$ wave 2)"),
  label = "tab:primary_w1w2_full",
  note = paste0(
    outcome_key, " ",
    # No \ref to tab:primary_w1w2 here: this table is \input by supplementary.tex
    # and that label lives in main.tex, so the reference cannot resolve.
    "All coefficients from Panel C of the main text's primary table, with 95\\% ",
    "bootstrap confidence intervals (", N_BOOT_TEX, " resamples, seeded) and the ",
    "per-term dropout rate. This specification is reported for continuity with ",
    "the moderator models and is not the primary one: at $N=", nrow(df),
    "$ it leaves ", results %>% filter(spec == "Full covariates") %>%
      slice(1) %>% pull(df_residual),
    " residual degrees of freedom, and it widens the mode interval on all four ",
    "outcomes while fitting worse on three of them despite twelve additional ",
    "parameters. An asterisk marks a coefficient whose interval excludes ",
    "zero and whose dropout is below 1\\%. Dropout is driven by the reference ",
    "categories rather than by a group's own size: hometown is referenced to ",
    "rural ($n=2$) and primary transportation to cycling/scootering ($n=3$), so ",
    "terms contrasted against them are inestimable whenever the reference ",
    "vanishes from a resample. Race/ethnicity cell sizes: White 18, Asian 19, ",
    "Hispanic or Latino/a/x 11, Black or African American 5, another race or ",
    "ethnicity not listed 2. \\textbf{Subgroup coefficients are reported for ",
    "completeness and are not interpreted.}"),
  font = "\\scriptsize",   # six columns: needs the extra headroom
  filename = "primary_w1w2_full.tex",
  fit = TRUE                    # supplementary; long term labels
)

# ----------------------
# 8. Console summary
# ----------------------
cat("\n---------------- SUMMARY ----------------\n")
cat("\nMode contrast (GenAI minus Conventional) on W1 -> W2, by specification:\n")
print(results %>%
        filter(term == "genai_firstTRUE") %>%
        arrange(outcome, spec) %>%
        select(outcome, spec, estimate, boot_ci_lower, boot_ci_upper,
               dropout, significant, r_squared, df_residual) %>%
        mutate(across(where(is.numeric), ~round(., 4))))

cat("\nTerms failing the dropout threshold (full covariate set only):\n")
print(results %>%
        filter(!stable) %>%
        distinct(spec, term) %>%
        arrange(spec, term))

cat("\nDone. Outputs in", out_dir, "\n")
