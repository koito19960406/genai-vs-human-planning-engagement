# =============================================================
# bootstrap_inference.R
# The inference standard adopted in issue #45, shared by the two regression
# suites the manuscript reports -- 4_regression_analysis.R (Table
# tab:bootstrap_results) and 6_change_in_interest.R (Table
# tab:interest_regression) -- so that the two sections cannot drift apart.
#
# THE RULE. A coefficient counts as significant iff its bootstrap percentile
# confidence interval excludes zero AND its dropout is below DROPOUT_MAX.
# Dropout is the share of resamples in which the term was inestimable: the
# level vanished from the resample, the refit went collinear, or the fit
# errored. Parametric OLS p-values are not used for inference anywhere.
#
# WHY DROPOUT AND NOT A CELL-SIZE FLOOR. A resample of 55 with replacement
# omits a two-person category in (1 - 2/55)^55 = 13% of draws, and the term
# then ceases to exist in the refit. Quantiling only the draws that happen to
# retain it conditions the interval on the category appearing, which biases it
# narrow and lets it exclude zero when an unconditional interval would not.
# The bias is not about a group's own size: two of the three categorical
# controls in these models have a tiny REFERENCE level (Hometown: Rural n = 2,
# Primary Transportation: Cycling/scootering n = 3), so it reaches terms whose
# own group is large -- Hometown: Suburban covers 33 people and drops out 13.6%
# of the time. Above the threshold the interval is refused rather than computed
# from the survivors.
#
# 12_primary_w1w2.R carries its own copy of this rule (issue #41, written while
# these two files were being edited). The three are deliberately identical.
# =============================================================

BOOT_SEED <- 20250411   # the session window closed 11 April 2025; matches 10_ and 12_
N_BOOT <- 2000          # matches 10_carryover_analysis.R and 12_primary_w1w2.R
DROPOUT_MAX <- 0.01     # issue #45: refuse an interval at or above this

# ON THE DRAW COUNT, because it was raised to 50,000 and then deliberately put
# back. A handful of coefficients lie close enough to zero that which side of it
# the 2.5% quantile lands on is decided partly by resampling error rather than
# by the data. Measured on attach_conv / AI Familiarity: across 20 seeds at
# R = 2,000 the lower bound ranges [-0.008, +0.040], so the mark appears in 15
# runs of 20; its true value, from runs at R = 50,000, is +0.008. Raising the
# count does stabilise that mark -- and buys exactly one star, on a coefficient
# whose interval is [+0.009, +1.30] and which no claim in the paper should rest
# on. It also splits the manuscript across two resample counts (10_ and 12_ are
# frozen at 2,000, their numbers being quoted in issues #9, #13 and #41) and
# invites a reviewer to ask why, where the honest answer advertises that a
# starred result is a knife edge.
#
# So the count stays conventional and the borderline cases are handled by
# disclosure instead: every interval and dropout is in the supplementary tables,
# which note that a few marks are sensitive to resampling error. With the seed
# fixed the published tables reproduce exactly.

# Bootstrap an lm by case resampling. Returns one row per coefficient.
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
    if (is.null(bfit)) next          # an errored fit is dropout for every term
    cf <- coef(bfit)
    keep <- intersect(names(cf), coef_names)
    reps[b, keep] <- cf[keep]
  }

  dropout <- apply(reps, 2, function(x) mean(is.na(x)))
  lo <- apply(reps, 2, quantile, probs = 0.025, na.rm = TRUE)
  hi <- apply(reps, 2, quantile, probs = 0.975, na.rm = TRUE)

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
    # Kept for the record and for the erratum comparison in issue #45. NOT the
    # inference: this is the quantity the submitted table starred under a
    # caption that said bootstrap.
    p_value_parametric = as.numeric(summary(fit)$coefficients[, 4]),
    n_obs = nobs(fit),
    r_squared = summary(fit)$r.squared,
    adj_r_squared = summary(fit)$adj.r.squared,
    df_residual = df.residual(fit),
    row.names = NULL, stringsAsFactors = FALSE
  )
}

# ----------------------
# Shared presentation
# ----------------------
# Canonical display names settled in issue #44: QID94 is a familiarity
# self-rating about AI tools in general, QID95 is specific to AI image
# generation. Shared so the two tables cannot disagree about what a variable
# is called, which is the defect #44 found.
TERM_LABELS <- c(
  "(Intercept)"                                        = "Intercept",
  "AI_Experience_num"                                  = "AI Familiarity",
  "AI_Tool_Usage_num"                                  = "AI Image-Tool Use",
  "GenderWoman"                                        = "Gender: Woman",
  "HometownSuburban"                                   = "Hometown: Suburban",
  "HometownUrban"                                      = "Hometown: Urban",
  "Planning_Knowledge_num"                             = "Planning Knowledge",
  "Primary_TransportationPersonal car"                 = "Primary Transportation: Personal car",
  "Primary_TransportationRideshare (Uber/Lyft)"        = "Primary Transportation: Rideshare",
  "Primary_TransportationWalking"                      = "Primary Transportation: Walking",
  "Race_EthnicityAnother race or ethnicity not listed" = "Race/Ethnicity: Another race",
  "Race_EthnicityAsian"                                = "Race/Ethnicity: Asian",
  "Race_EthnicityBlack or African American"            = "Race/Ethnicity: Black or African American",
  "Race_EthnicityHispanic or Latino/a/x"               = "Race/Ethnicity: Hispanic or Latino/a/x",
  "genai_firstTRUE"                                    = "GenAI First",
  "street_firstTRUE"                                   = "Street First"
)

label_term <- function(x) ifelse(x %in% names(TERM_LABELS), TERM_LABELS[x], x)

# A main-text cell: the point estimate, a star iff the whole rule passes, a
# dagger iff no interval could be quoted. Without the dagger a reader cannot
# tell a coefficient with a genuine zero-spanning interval from one that has
# no usable interval at all.
fmt_cell <- function(estimate, significant, stable, k = 3) {
  mark <- ifelse(!stable, "$^{\\dagger}$", ifelse(significant, "*", ""))
  paste0(formatC(estimate, format = "f", digits = k), mark)
}

# The three shared note blocks. Both regression tables carry all three, per
# issue #45 decision 3 and issue #39's precedent (a table note, not body prose).
note_inference <- function(n_boot = N_BOOT) paste0(
  "Bootstrap regression coefficients (", formatC(n_boot, format = "d", big.mark = "{,}"),
  " resamples, seed ", BOOT_SEED, "). A coefficient is starred (*) iff its bootstrap ",
  "95\\% confidence interval excludes zero \\emph{and} its dropout---the share of resamples ",
  "in which the term was inestimable---is below 1\\%; $^{\\dagger}$ marks a term whose dropout ",
  "reaches 1\\%, for which no interval is quoted. No p-value ladder is reported. ",
  "Full intervals and per-term dropout are in the supplementary material."
)

# Built from the data rather than transcribed, so it cannot go stale.
note_cell_sizes <- function(data) {
  levs <- c("White", "Asian", "Hispanic or Latino/a/x",
            "Black or African American", "Another race or ethnicity not listed")
  counts <- table(as.character(data$Race_Ethnicity))
  shown <- sprintf("%s %d", c(levs[1:4], "another race or ethnicity not listed"),
                   as.integer(counts[levs]))
  paste0("Race/ethnicity cell sizes: ", paste(shown, collapse = ", "),
         ". Subgroup coefficients are reported for completeness and are not interpreted.")
}

# Issue #39 fixed the first clause; issue #45 requires the second, because the
# n = 2 rural reference is what makes every Suburban interval unreliable.
note_hometown <- function(data) paste0(
  "The reference category for hometown is rural ($n = ",
  sum(as.character(data$Hometown) == "Rural"),
  "$); hometown coefficients are reported for completeness and are not interpreted."
)
