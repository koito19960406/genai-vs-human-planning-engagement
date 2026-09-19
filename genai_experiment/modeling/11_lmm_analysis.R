# =============================================================
# 11_lmm_analysis.R
# Linear mixed model (LMM) rebuild of the primary analyses, with a
# side-by-side comparison against the currently reported results.
#
# EXPLORATORY. Nothing in the manuscript changes as a result of this
# script. It writes only to reports/models/lmm/ and does not touch any
# output produced by 3_ttest_analysis.R, 4_regression_analysis.R or
# 9_effect_sizes.R.
#
# Design: within-subjects, N = 55. Each participant completed BOTH
# methods (GenAI agent, traditional facilitator), ~3 min each, at two
# Austin sites (6th Street, Zilker Park). Method order and location
# order are counterbalanced. Four outcomes measured at three waves
# (before / between / after) on 5-point Likert scales coded -2..2.
#
# Two long-format representations of the same data are modelled:
#   sess  - session grain, 2 rows/participant, outcome = change score
#           for that session (wave2-wave1, wave3-wave2). Has an explicit
#           `method` factor, so it expresses the ticket's
#           "method x order x wave" specification literally.
#   wavelong - wave grain, 3 rows/participant, outcome = the Likert level
#           at that wave. Method is encoded by wave x order.
#
# Models fitted per outcome:
#   D0  change ~ method + (1|pid)                     [unadjusted]
#   D1  change ~ method*position + location*position
#                + covariates + (1|pid)               [adjusted]
#   DCS gls(change ~ method, corCompSymm(|pid))       [allows rho < 0]
#   DN  change ~ 0 + method + method:(covariates)     [per-method slopes,
#                + (1|pid)                             direct counterpart
#                                                      to the two OLS columns]
#   L0  score ~ wave*genai_first + wave*street_first + (1|pid)  [unadjusted]
#   L1  L0 + covariates                                          [adjusted]
#
# Inference uses lmerTest / Satterthwaite degrees of freedom.
# =============================================================

# ----------------------
# 0. Load Packages & Setup
# ----------------------
pacman::p_load(
  dplyr,
  tidyr,
  readr,
  purrr,
  stringr,
  forcats,
  lme4,
  lmerTest,
  emmeans,
  nlme,
  broom.mixed
)

emmeans::emm_options(lmerTest.limit = 5000, pbkrtest.limit = 5000)

# Configurable paths -- everything under reports/models/main_test/ is
# READ-ONLY here; it is the "current manuscript result" side of the
# comparison.
processed_data_path   <- "data/processed/survey_processed.csv"
manuscript_ttest_path <- "reports/models/main_test/paired_ttests.csv"
manuscript_es_path    <- "reports/models/main_test/effect_sizes.csv"
out_dir               <- "reports/models/lmm"

dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

out_path <- function(...) file.path(out_dir, ...)

# ----------------------
# 0.1 Warning / convergence log
# ----------------------
log_lines <- character(0)
log_msg <- function(...) {
  msg <- paste0(...)
  log_lines <<- c(log_lines, msg)
  cat(msg, "\n")
}

fit_records <- list()

# Fit an lmer, capturing convergence / singularity messages instead of
# letting them scroll past.
fit_lmer <- function(formula, data, label) {
  warns <- character(0)
  model <- withCallingHandlers(
    lmerTest::lmer(formula, data = data, REML = TRUE),
    warning = function(w) {
      warns <<- c(warns, conditionMessage(w))
      invokeRestart("muffleWarning")
    },
    message = function(m) {
      msg <- trimws(conditionMessage(m))
      if (nzchar(msg)) warns <<- c(warns, msg)
      invokeRestart("muffleMessage")
    }
  )
  singular <- lme4::isSingular(model, tol = 1e-4)
  if (singular) {
    warns <- c(warns, "singular fit: random-intercept variance estimated at the boundary (0)")
  }
  vc <- as.data.frame(lme4::VarCorr(model))
  tau2 <- vc$vcov[vc$grp == "pid"]
  sigma2 <- vc$vcov[vc$grp == "Residual"]
  mf <- model@frame
  fit_records[[label]] <<- tibble(
    model = label,
    n_obs = nrow(mf),
    n_groups = nlevels(mf$pid),
    tau2 = tau2,
    sigma2 = sigma2,
    icc = tau2 / (tau2 + sigma2),
    singular = singular,
    warnings = if (length(warns)) paste(unique(warns), collapse = " | ") else ""
  )
  for (w in unique(warns)) log_msg("[", label, "] ", w)
  model
}

# ----------------------
# 1. Data Loading & Long-Format Construction
# ----------------------
if (!file.exists(processed_data_path)) stop("Processed data not found!")
df <- read_csv(processed_data_path, show_col_types = FALSE)

stopifnot(nrow(df) == 55)

covariates <- c(
  "Gender", "Race_Ethnicity", "Planning_Knowledge_num",
  "AI_Experience_num", "AI_Tool_Usage_num",
  "Primary_Transportation", "Hometown"
)

outcomes <- tibble(
  stem  = c("walk", "cycle", "plan", "attach"),
  label = c("Walking", "Cycling", "Planning", "Attachment")
)

df <- df %>%
  mutate(
    pid = factor(row_number()),
    # Same reference levels as 4_regression_analysis.R
    Race_Ethnicity = fct_relevel(factor(Race_Ethnicity), "White"),
    Gender = factor(Gender),
    Primary_Transportation = factor(Primary_Transportation),
    Hometown = factor(Hometown),
    genai_first_f = factor(genai_first, levels = c(FALSE, TRUE),
                           labels = c("ConvFirst", "GenAIFirst")),
    street_first_f = factor(street_first, levels = c(FALSE, TRUE),
                            labels = c("ParkFirst", "StreetFirst"))
  )

person_vars <- c("pid", covariates, "genai_first_f", "street_first_f")

# --- session grain (2 rows per participant, change scores) -----------
make_session_rows <- function(k) {
  df %>%
    select(all_of(person_vars),
           location_k = !!sym(paste0("location_", k)),
           walk   = !!sym(paste0("walk_diff_", k)),
           cycle  = !!sym(paste0("cycle_diff_", k)),
           plan   = !!sym(paste0("plan_diff_", k)),
           attach = !!sym(paste0("attach_diff_", k))) %>%
    mutate(
      position = k,
      method = if (k == 1) {
        if_else(genai_first_f == "GenAIFirst", "GenAI", "Traditional")
      } else {
        if_else(genai_first_f == "GenAIFirst", "Traditional", "GenAI")
      }
    )
}

sess <- bind_rows(make_session_rows(1), make_session_rows(2)) %>%
  mutate(
    method   = factor(method, levels = c("Traditional", "GenAI")),
    location = factor(location_k, levels = c("Park", "Street")),
    position = factor(position, levels = c(1, 2), labels = c("First", "Second"))
  ) %>%
  select(-location_k)

# Integrity check: the session-grain reshape must reproduce the wide
# *_ai / *_conv columns the manuscript analyses use.
for (i in seq_len(nrow(outcomes))) {
  st <- outcomes$stem[i]
  ai_from_long <- sess %>%
    filter(method == "GenAI") %>%
    arrange(as.integer(as.character(pid))) %>%
    pull(!!sym(st))
  conv_from_long <- sess %>%
    filter(method == "Traditional") %>%
    arrange(as.integer(as.character(pid))) %>%
    pull(!!sym(st))
  stopifnot(identical(ai_from_long, df[[paste0(st, "_ai")]]))
  stopifnot(identical(conv_from_long, df[[paste0(st, "_conv")]]))
}
log_msg("[data] session-grain reshape verified against *_ai / *_conv columns")

# --- wave grain (3 rows per participant, Likert levels) --------------
make_wave_rows <- function(k) {
  df %>%
    select(all_of(person_vars),
           walk   = !!sym(paste0("walk_wave_", k)),
           cycle  = !!sym(paste0("cycle_wave_", k)),
           plan   = !!sym(paste0("plan_wave_", k)),
           attach = !!sym(paste0("attach_wave_", k))) %>%
    mutate(wave = k)
}

wavelong <- bind_rows(make_wave_rows(1), make_wave_rows(2), make_wave_rows(3)) %>%
  mutate(wave = factor(wave, levels = 1:3, labels = c("W1", "W2", "W3")))

log_msg("[data] N participants = ", nlevels(df$pid),
        "; session rows = ", nrow(sess), "; wave rows = ", nrow(wavelong))
log_msg(paste(
  "[note] emmeans prints 'Results may be misleading due to involvement in",
  "interactions' whenever a marginal mean is taken over a factor that also appears",
  "in an interaction (method over position, location over position). That is the",
  "intended quantity here -- the method effect averaged over presentation order,",
  "which is what the paired t-test estimates -- and proportional weighting is used",
  "so the average matches the realised counterbalancing."))

# ----------------------
# 2. Helpers
# ----------------------
cov_terms <- paste(covariates, collapse = " + ")

verdict <- function(p, alpha = 0.05) {
  if (is.na(p)) return(NA_character_)
  if (p < alpha) "sig." else "n.s."
}

stars <- function(p) {
  case_when(
    is.na(p)   ~ "",
    p < 0.001  ~ "***",
    p < 0.01   ~ "**",
    p < 0.05   ~ "*",
    p < 0.1    ~ "$^{\\dagger}$",
    TRUE       ~ ""
  )
}

# Extract a named linear combination from an emmGrid as a tidy row.
lincom <- function(emm, coefs, name) {
  ct <- contrast(emm, setNames(list(coefs), name))
  s  <- summary(ct, infer = c(TRUE, TRUE))
  tibble(
    contrast = name,
    estimate = s$estimate,
    se       = s$SE,
    df       = s$df,
    ci_low   = s$lower.CL,
    ci_high  = s$upper.CL,
    statistic = s$t.ratio,
    p_value  = s$p.value
  )
}

# Total SD implied by a random-intercept model: sqrt(tau^2 + sigma^2).
# For the session-grain change-score models this is the LMM analogue of
# the pooled SD that effsize::cohen.d(paired = TRUE) divides by, so
# estimate / total_sd is directly comparable to the manuscript's d.
total_sd <- function(model) {
  vc <- as.data.frame(lme4::VarCorr(model))
  sqrt(sum(vc$vcov))
}

# ----------------------
# 3. Manuscript-side benchmarks (read-only)
# ----------------------
ms_ttests <- read_csv(manuscript_ttest_path, show_col_types = FALSE)
ms_es     <- read_csv(manuscript_es_path, show_col_types = FALSE)

ms_method <- ms_ttests %>%
  filter(str_detect(comparison, "AI vs Conventional")) %>%
  mutate(label = str_remove(comparison, ": AI vs Conventional")) %>%
  # Manuscript reports AI - Conventional; effect_sizes.csv reports the
  # same contrast. Sign convention is GenAI minus Traditional.
  select(label, ms_estimate = mean_diff, ms_t = t_stat, ms_df = df, ms_p = p_value,
         ms_ci_low = conf_low, ms_ci_high = conf_high) %>%
  left_join(
    ms_es %>%
      filter(type == "paired") %>%
      mutate(label = str_remove(comparison, ": GenAI vs Traditional")) %>%
      select(label, ms_d = cohens_d, ms_d_low = ci_lower, ms_d_high = ci_upper,
             ms_d_magnitude = magnitude),
    by = "label"
  )

ms_location <- ms_ttests %>%
  filter(str_detect(comparison, "Street vs Park")) %>%
  mutate(label = str_remove(comparison, ": Street vs Park")) %>%
  select(label, ms_estimate = mean_diff, ms_p = p_value)

ms_total <- ms_ttests %>%
  filter(str_detect(comparison, "GenAI vs Conv Difference")) %>%
  mutate(label = str_remove(comparison, " Total: GenAI vs Conv Difference")) %>%
  select(label, ms_estimate = mean_diff, ms_t = t_stat, ms_df = df, ms_p = p_value) %>%
  left_join(
    ms_es %>%
      filter(type == "one-sample") %>%
      mutate(label = str_remove(comparison, ": Total Change \\(Wave 3 - Wave 1\\)")) %>%
      select(label, ms_d = cohens_d),
    by = "label"
  )

ms_order_method <- ms_ttests %>%
  filter(str_detect(comparison, "GenAI First vs Conv First")) %>%
  mutate(label = str_remove(comparison, " Total: GenAI First vs Conv First")) %>%
  select(label, ms_estimate = mean_diff, ms_p = p_value)

ms_order_location <- ms_ttests %>%
  filter(str_detect(comparison, "Street First vs Park First")) %>%
  mutate(label = str_remove(comparison, " Total: Street First vs Park First")) %>%
  select(label, ms_estimate = mean_diff, ms_p = p_value)

# ----------------------
# 4. Model Fitting
# ----------------------
models <- list()
method_rows   <- list()
location_rows <- list()
total_rows    <- list()
order_rows    <- list()
fixef_rows    <- list()
covar_rows    <- list()

for (i in seq_len(nrow(outcomes))) {
  st  <- outcomes$stem[i]
  lab <- outcomes$label[i]
  log_msg("\n=== ", lab, " ===")

  # ---- D0: unadjusted session-grain LMM -----------------------------
  f_d0 <- as.formula(paste0(st, " ~ method + (1 | pid)"))
  d0 <- fit_lmer(f_d0, sess, paste0("D0_", st))

  # ---- D1: adjusted session-grain LMM -------------------------------
  # method x position is, by construction, the between-subject order
  # factor (genai_first); location x position is street_first. So the
  # two interactions carry the full "method x order" and
  # "location x order" carryover information and genai_first /
  # street_first are NOT entered separately (they would be collinear).
  f_d1 <- as.formula(paste0(
    st, " ~ method * position + location * position + ", cov_terms, " + (1 | pid)"
  ))
  d1 <- fit_lmer(f_d1, sess, paste0("D1_", st))

  # ---- DCS: GLS with compound symmetry (rho may be negative) --------
  # lme4 constrains the random-intercept variance to be >= 0. The two
  # change scores are strongly negatively correlated within participant,
  # so D0/D1 sit on that boundary. gls() with corCompSymm can represent
  # a negative within-participant correlation and therefore reproduces
  # the paired t-test's standard error.
  dcs <- tryCatch({
    nlme::gls(as.formula(paste0(st, " ~ method")),
              data = sess,
              correlation = nlme::corCompSymm(form = ~ 1 | pid),
              method = "REML")
  }, error = function(e) {
    log_msg("[DCS_", st, "] gls failed: ", conditionMessage(e))
    NULL
  })

  # ---- DN: per-method covariate slopes ------------------------------
  # `0 + method + method:(...)` makes every coefficient a within-method
  # slope, so each column of the manuscript's bootstrap OLS table has an
  # exact LMM counterpart. Because both methods share an identical
  # design matrix, GLS point estimates coincide with equation-by-equation
  # OLS; what changes is the standard error (pooled residual variance and
  # a shared participant random intercept) and hence the p-value.
  f_dn <- as.formula(paste0(
    st, " ~ 0 + method + method:(", cov_terms,
    " + genai_first_f + street_first_f) + (1 | pid)"
  ))
  dn <- fit_lmer(f_dn, sess, paste0("DN_", st))

  # ---- L0 / L1: wave-grain LMMs on the Likert levels ----------------
  f_l0 <- as.formula(paste0(
    st, " ~ wave * genai_first_f + wave * street_first_f + (1 | pid)"
  ))
  l0 <- fit_lmer(f_l0, wavelong, paste0("L0_", st))

  f_l1 <- as.formula(paste0(
    st, " ~ wave * genai_first_f + wave * street_first_f + ", cov_terms, " + (1 | pid)"
  ))
  l1 <- fit_lmer(f_l1, wavelong, paste0("L1_", st))

  models[[st]] <- list(d0 = d0, d1 = d1, dcs = dcs, dn = dn, l0 = l0, l1 = l1)

  # ------------------------------------------------------------------
  # 4.1 Method contrast (GenAI - Traditional)
  # ------------------------------------------------------------------
  emm_d0 <- emmeans(d0, ~ method, weights = "proportional")
  c_d0 <- lincom(emm_d0, c(-1, 1), "GenAI - Traditional")

  emm_d1 <- emmeans(d1, ~ method, weights = "proportional")
  c_d1 <- lincom(emm_d1, c(-1, 1), "GenAI - Traditional")

  # Wave-grain counterpart. GenAI's effect is (W2-W1) for participants
  # who got GenAI first and (W3-W2) for the others; Traditional is the
  # mirror image. Average over the two order groups.
  emm_l <- function(m) emmeans(m, ~ wave * genai_first_f, weights = "proportional")
  method_coefs <- function(emm) {
    g <- as.data.frame(emm@grid)
    k <- function(w, o) which(g$wave == w & g$genai_first_f == o)
    v <- numeric(nrow(g))
    # GenAI effect
    v[k("W2", "GenAIFirst")] <- v[k("W2", "GenAIFirst")] + 0.5
    v[k("W1", "GenAIFirst")] <- v[k("W1", "GenAIFirst")] - 0.5
    v[k("W3", "ConvFirst")]  <- v[k("W3", "ConvFirst")]  + 0.5
    v[k("W2", "ConvFirst")]  <- v[k("W2", "ConvFirst")]  - 0.5
    # minus Traditional effect
    v[k("W3", "GenAIFirst")] <- v[k("W3", "GenAIFirst")] - 0.5
    v[k("W2", "GenAIFirst")] <- v[k("W2", "GenAIFirst")] + 0.5
    v[k("W2", "ConvFirst")]  <- v[k("W2", "ConvFirst")]  - 0.5
    v[k("W1", "ConvFirst")]  <- v[k("W1", "ConvFirst")]  + 0.5
    v
  }
  e_l0 <- emm_l(l0); c_l0 <- lincom(e_l0, method_coefs(e_l0), "GenAI - Traditional")
  e_l1 <- emm_l(l1); c_l1 <- lincom(e_l1, method_coefs(e_l1), "GenAI - Traditional")

  # GLS row
  if (!is.null(dcs)) {
    tt <- summary(dcs)$tTable
    c_dcs <- tibble(
      contrast = "GenAI - Traditional",
      estimate = tt["methodGenAI", "Value"],
      se = tt["methodGenAI", "Std.Error"],
      df = dcs$dims$N - dcs$dims$p,
      ci_low = NA_real_, ci_high = NA_real_,
      statistic = tt["methodGenAI", "t-value"],
      p_value = tt["methodGenAI", "p-value"]
    )
    rho_dcs <- as.numeric(coef(dcs$modelStruct$corStruct, unconstrained = FALSE))
  } else {
    c_dcs <- tibble(contrast = "GenAI - Traditional", estimate = NA_real_, se = NA_real_,
                    df = NA_real_, ci_low = NA_real_, ci_high = NA_real_,
                    statistic = NA_real_, p_value = NA_real_)
    rho_dcs <- NA_real_
  }

  sd_d0 <- total_sd(d0)

  method_rows[[st]] <- bind_rows(
    c_d0  %>% mutate(source = "LMM D0 (unadjusted, change scores)"),
    c_dcs %>% mutate(source = "GLS compound symmetry (unadjusted)"),
    c_d1  %>% mutate(source = "LMM D1 (adjusted, change scores)"),
    c_l0  %>% mutate(source = "LMM L0 (unadjusted, wave levels)"),
    c_l1  %>% mutate(source = "LMM L1 (adjusted, wave levels)")
  ) %>%
    mutate(label = lab, outcome = st, lmm_d = estimate / sd_d0, rho_cs = rho_dcs) %>%
    select(label, outcome, source, everything())

  # ------------------------------------------------------------------
  # 4.2 Location contrast (Street - Park)
  # ------------------------------------------------------------------
  f_loc0 <- as.formula(paste0(st, " ~ location + (1 | pid)"))
  loc0 <- fit_lmer(f_loc0, sess, paste0("LOC0_", st))
  c_loc0 <- lincom(emmeans(loc0, ~ location, weights = "proportional"),
                   c(-1, 1), "Street - Park")

  emm_locd1 <- emmeans(d1, ~ location, weights = "proportional")
  c_locd1 <- lincom(emm_locd1, c(-1, 1), "Street - Park")

  location_rows[[st]] <- bind_rows(
    c_loc0  %>% mutate(source = "LMM (unadjusted, change scores)"),
    c_locd1 %>% mutate(source = "LMM D1 (adjusted, change scores)")
  ) %>% mutate(label = lab, outcome = st)

  # ------------------------------------------------------------------
  # 4.3 Total change W3 - W1
  # ------------------------------------------------------------------
  tot_coefs <- function(emm) {
    g <- as.data.frame(emm@grid)
    v <- numeric(nrow(g))
    nF <- sum(g$wave == "W1")  # cells per wave
    v[g$wave == "W3"] <-  1 / nF
    v[g$wave == "W1"] <- -1 / nF
    v
  }
  e_t0 <- emmeans(l0, ~ wave * genai_first_f * street_first_f, weights = "proportional")
  c_t0 <- lincom(e_t0, tot_coefs(e_t0), "W3 - W1")
  e_t1 <- emmeans(l1, ~ wave * genai_first_f * street_first_f, weights = "proportional")
  c_t1 <- lincom(e_t1, tot_coefs(e_t1), "W3 - W1")

  vc_l0 <- as.data.frame(lme4::VarCorr(l0))
  sd_w3w1 <- sqrt(2 * vc_l0$vcov[vc_l0$grp == "Residual"])

  total_rows[[st]] <- bind_rows(
    c_t0 %>% mutate(source = "LMM L0 (unadjusted, wave levels)"),
    c_t1 %>% mutate(source = "LMM L1 (adjusted, wave levels)")
  ) %>% mutate(label = lab, outcome = st, lmm_d = estimate / sd_w3w1)

  # ------------------------------------------------------------------
  # 4.4 Order effects and carryover
  # ------------------------------------------------------------------
  # (a) total change contrasted between method-order groups
  ord_coefs <- function(emm, byvar, lvl_hi, lvl_lo) {
    g <- as.data.frame(emm@grid)
    v <- numeric(nrow(g))
    hi <- g[[byvar]] == lvl_hi
    lo <- g[[byvar]] == lvl_lo
    v[g$wave == "W3" & hi] <-  1 / sum(g$wave == "W3" & hi)
    v[g$wave == "W1" & hi] <- -1 / sum(g$wave == "W1" & hi)
    v[g$wave == "W3" & lo] <- -1 / sum(g$wave == "W3" & lo)
    v[g$wave == "W1" & lo] <-  1 / sum(g$wave == "W1" & lo)
    v
  }
  c_ord_m <- lincom(e_t0, ord_coefs(e_t0, "genai_first_f", "GenAIFirst", "ConvFirst"),
                    "Total change: GenAI-first - Conv-first")
  c_ord_l <- lincom(e_t0, ord_coefs(e_t0, "street_first_f", "StreetFirst", "ParkFirst"),
                    "Total change: Street-first - Park-first")

  # (b) period (session-position) effect and method x position carryover
  emm_mp <- emmeans(d1, ~ method * position, weights = "proportional")
  g_mp <- as.data.frame(emm_mp@grid)
  kmp <- function(m, p) which(g_mp$method == m & g_mp$position == p)
  v_period <- numeric(nrow(g_mp))
  v_period[kmp("GenAI", "Second")] <- 0.5; v_period[kmp("Traditional", "Second")] <- 0.5
  v_period[kmp("GenAI", "First")]  <- -0.5; v_period[kmp("Traditional", "First")] <- -0.5
  c_period <- lincom(emm_mp, v_period, "Session 2 - Session 1 (period effect)")

  v_carry <- numeric(nrow(g_mp))
  v_carry[kmp("GenAI", "Second")] <- 1; v_carry[kmp("Traditional", "Second")] <- -1
  v_carry[kmp("GenAI", "First")]  <- -1; v_carry[kmp("Traditional", "First")] <- 1
  c_carry <- lincom(emm_mp, v_carry, "Method x position (carryover)")

  order_rows[[st]] <- bind_rows(c_ord_m, c_ord_l, c_period, c_carry) %>%
    mutate(label = lab, outcome = st)

  # ------------------------------------------------------------------
  # 4.5 Fixed effects of the adjusted wave-level model
  # ------------------------------------------------------------------
  fixef_rows[[st]] <- broom.mixed::tidy(l1, effects = "fixed") %>%
    mutate(label = lab, outcome = st)

  # ------------------------------------------------------------------
  # 4.6 Per-method covariate coefficients: OLS vs LMM
  # ------------------------------------------------------------------
  ols_form <- function(resp) {
    as.formula(paste0(resp, " ~ ", cov_terms, " + genai_first_f + street_first_f"))
  }
  for (meth in c("GenAI", "Traditional")) {
    resp <- paste0(st, if (meth == "GenAI") "_ai" else "_conv")
    ols <- lm(ols_form(resp), data = df)
    ols_tab <- broom::tidy(ols) %>%
      transmute(term_raw = term, ols_estimate = estimate, ols_se = std.error,
                ols_p = p.value)

    lmm_tab <- broom.mixed::tidy(dn, effects = "fixed") %>%
      filter(str_detect(term, paste0("^method", meth))) %>%
      transmute(
        term_raw = str_remove(term, paste0("^method", meth, ":?")),
        term_raw = if_else(term_raw == "", "(Intercept)", term_raw),
        lmm_estimate = estimate, lmm_se = std.error, lmm_df = df, lmm_p = p.value
      )

    covar_rows[[paste(st, meth)]] <- full_join(ols_tab, lmm_tab, by = "term_raw") %>%
      mutate(label = lab, outcome = st, method = meth)
  }
}

method_comparison_raw   <- bind_rows(method_rows)
location_comparison_raw <- bind_rows(location_rows)
total_comparison_raw    <- bind_rows(total_rows)
order_comparison_raw    <- bind_rows(order_rows)
fixef_all               <- bind_rows(fixef_rows)
covar_all               <- bind_rows(covar_rows)
variance_components     <- bind_rows(fit_records)

# ----------------------
# 5. Assemble comparison tables
# ----------------------

# 5.1 Method effect: manuscript paired t-test vs LMM ------------------
method_comparison <- ms_method %>%
  left_join(
    method_comparison_raw %>%
      select(label, source, estimate, se, df, p_value, lmm_d),
    by = "label"
  ) %>%
  mutate(
    ms_verdict  = map_chr(ms_p, verdict),
    lmm_verdict = map_chr(p_value, verdict),
    agrees = ms_verdict == lmm_verdict,
    flag = if_else(agrees, "", "DISAGREEMENT")
  ) %>%
  mutate(label = factor(label, levels = outcomes$label)) %>%
  arrange(label, source)

# 5.2 Location effect --------------------------------------------------
location_comparison <- ms_location %>%
  left_join(
    location_comparison_raw %>% select(label, source, estimate, se, df, p_value),
    by = "label"
  ) %>%
  mutate(
    ms_verdict = map_chr(ms_p, verdict),
    lmm_verdict = map_chr(p_value, verdict),
    agrees = ms_verdict == lmm_verdict,
    flag = if_else(agrees, "", "DISAGREEMENT")
  ) %>%
  mutate(label = factor(label, levels = outcomes$label)) %>%
  arrange(label, source)

# 5.3 Total change -----------------------------------------------------
# NOTE: the manuscript's one-sample test is ONE-SIDED (alternative =
# "greater"); the LMM contrast is two-sided. Both are carried so the
# sidedness difference is explicit rather than buried.
total_comparison <- ms_total %>%
  left_join(
    total_comparison_raw %>% select(label, source, estimate, se, df, p_value, lmm_d),
    by = "label"
  ) %>%
  mutate(
    p_one_sided = if_else(estimate > 0, p_value / 2, 1 - p_value / 2),
    ms_verdict = map_chr(ms_p, verdict),
    lmm_verdict_two = map_chr(p_value, verdict),
    lmm_verdict_one = map_chr(p_one_sided, verdict),
    agrees = ms_verdict == lmm_verdict_one,
    flag = if_else(agrees, "", "DISAGREEMENT")
  ) %>%
  mutate(label = factor(label, levels = outcomes$label)) %>%
  arrange(label, source)

# 5.4 Order effects ----------------------------------------------------
order_comparison <- order_comparison_raw %>%
  left_join(ms_order_method %>% rename(ms_estimate_m = ms_estimate, ms_p_m = ms_p),
            by = "label") %>%
  left_join(ms_order_location %>% rename(ms_estimate_l = ms_estimate, ms_p_l = ms_p),
            by = "label") %>%
  mutate(
    ms_estimate = case_when(
      str_detect(contrast, "GenAI-first")  ~ ms_estimate_m,
      str_detect(contrast, "Street-first") ~ ms_estimate_l,
      TRUE ~ NA_real_
    ),
    ms_p = case_when(
      str_detect(contrast, "GenAI-first")  ~ ms_p_m,
      str_detect(contrast, "Street-first") ~ ms_p_l,
      TRUE ~ NA_real_
    ),
    ms_verdict = map_chr(ms_p, verdict),
    lmm_verdict = map_chr(p_value, verdict),
    agrees = if_else(is.na(ms_verdict), NA, ms_verdict == lmm_verdict),
    flag = case_when(is.na(agrees) ~ "no manuscript counterpart",
                     agrees ~ "", TRUE ~ "DISAGREEMENT")
  ) %>%
  select(-ms_estimate_m, -ms_p_m, -ms_estimate_l, -ms_p_l) %>%
  mutate(label = factor(label, levels = outcomes$label)) %>%
  arrange(label, contrast)

# 5.5 Covariate coefficients ------------------------------------------
term_labels <- c(
  "(Intercept)" = "Intercept",
  "GenderWoman" = "Gender: Woman",
  "Race_EthnicityAnother race or ethnicity not listed" = "Race/Ethnicity: Another race",
  "Race_EthnicityAsian" = "Race/Ethnicity: Asian",
  "Race_EthnicityBlack or African American" = "Race/Ethnicity: Black or African American",
  "Race_EthnicityHispanic or Latino/a/x" = "Race/Ethnicity: Hispanic or Latino/a/x",
  "Planning_Knowledge_num" = "Planning Knowledge",
  "AI_Experience_num" = "AI Familiarity",
  "AI_Tool_Usage_num" = "AI Image-Tool Use",
  "Primary_TransportationPersonal car" = "Primary Transportation: Personal car",
  "Primary_TransportationRideshare (Uber/Lyft)" = "Primary Transportation: Rideshare",
  "Primary_TransportationWalking" = "Primary Transportation: Walking",
  "HometownSuburban" = "Hometown: Suburban",
  "HometownUrban" = "Hometown: Urban",
  "genai_first_fGenAIFirst" = "GenAI First",
  "street_first_fStreetFirst" = "Street First"
)

covariate_comparison <- covar_all %>%
  mutate(
    term = if_else(term_raw %in% names(term_labels),
                   unname(term_labels[term_raw]), term_raw),
    ols_verdict = map_chr(ols_p, verdict),
    lmm_verdict = map_chr(lmm_p, verdict),
    agrees = ols_verdict == lmm_verdict,
    flag = if_else(agrees, "", "DISAGREEMENT"),
    est_matches = abs(ols_estimate - lmm_estimate) < 1e-6
  ) %>%
  mutate(label = factor(label, levels = outcomes$label),
         method = factor(method, levels = c("GenAI", "Traditional")),
         term = factor(term, levels = unname(term_labels))) %>%
  arrange(label, method, term) %>%
  select(label, outcome, method, term, term_raw,
         ols_estimate, ols_se, ols_p, ols_verdict,
         lmm_estimate, lmm_se, lmm_df, lmm_p, lmm_verdict,
         est_matches, agrees, flag)

covariate_disagreements <- covariate_comparison %>% filter(!agrees)

# ----------------------
# 6. Write CSV outputs
# ----------------------
write_csv(method_comparison,    out_path("method_effect_comparison.csv"))
write_csv(location_comparison,  out_path("location_effect_comparison.csv"))
write_csv(total_comparison,     out_path("total_change_comparison.csv"))
write_csv(order_comparison,     out_path("order_effect_comparison.csv"))
write_csv(covariate_comparison, out_path("covariate_comparison.csv"))
write_csv(covariate_disagreements, out_path("covariate_disagreements.csv"))
write_csv(fixef_all,            out_path("lmm_wave_model_fixed_effects.csv"))
write_csv(variance_components,  out_path("variance_components.csv"))

# ----------------------
# 7. LaTeX output helpers
# ----------------------
esc <- function(x) {
  x <- as.character(x)
  x[is.na(x)] <- ""
  x <- gsub("\\\\", "\\\\textbackslash{}", x)
  x <- gsub("([&%$#_{}])", "\\\\\\1", x)
  x <- gsub("\\^", "\\\\textasciicircum{}", x)
  x <- gsub("~", "\\\\textasciitilde{}", x)
  x
}

fmt <- function(x, digits = 3) {
  ifelse(is.na(x), "--", formatC(x, format = "f", digits = digits))
}

fmt_p <- function(p) {
  ifelse(is.na(p), "--",
         ifelse(p < 0.001, "$<$0.001", formatC(p, format = "f", digits = 3)))
}

flag_cell <- function(flag) {
  ifelse(flag == "DISAGREEMENT", "\\textbf{FLAG}",
         ifelse(flag == "", "", esc(flag)))
}

write_latex_table <- function(body_matrix, header, file, caption, label,
                              colspec = NULL, note = NULL,
                              env = "table", fontsize = "footnotesize") {
  ncol_tab <- length(header)
  if (is.null(colspec)) colspec <- paste0("l", strrep("r", ncol_tab - 1))
  lines <- c(
    paste0("\\begin{", env, "}[htbp]"),
    "\\centering",
    paste0("\\caption{", caption, "}"),
    paste0("\\label{", label, "}"),
    paste0("\\", fontsize),
    paste0("\\begin{tabular}{", colspec, "}"),
    "\\toprule",
    paste0(paste(header, collapse = " & "), " \\\\"),
    "\\midrule"
  )
  for (i in seq_len(nrow(body_matrix))) {
    lines <- c(lines, paste0(paste(body_matrix[i, ], collapse = " & "), " \\\\"))
  }
  lines <- c(lines, "\\bottomrule", "\\end{tabular}")
  if (!is.null(note)) {
    lines <- c(lines,
               "\\begin{minipage}{\\textwidth}", "\\footnotesize",
               "\\vspace{2pt}", paste0("\\textit{Notes:} ", note),
               "\\end{minipage}")
  }
  lines <- c(lines, paste0("\\end{", env, "}"))
  writeLines(lines, file)
  cat("Wrote", file, "\n")
}

write_latex_longtable <- function(body_matrix, header, file, caption, label,
                                  colspec = NULL, note = NULL,
                                  fontsize = "scriptsize") {
  ncol_tab <- length(header)
  if (is.null(colspec)) colspec <- paste0("ll", strrep("r", ncol_tab - 2))
  head_line <- paste0(paste(header, collapse = " & "), " \\\\")
  cont_line <- paste0("\\midrule \\multicolumn{", ncol_tab,
                      "}{r}{\\textit{continued on next page}} \\\\")
  lines <- c(
    paste0("{\\", fontsize),
    paste0("\\begin{longtable}{", colspec, "}"),
    paste0("\\caption{", caption, "}"),
    paste0("\\label{", label, "} \\\\"),
    "\\toprule", head_line, "\\midrule", "\\endfirsthead",
    "\\toprule", head_line, "\\midrule", "\\endhead",
    cont_line,
    "\\endfoot", "\\bottomrule", "\\endlastfoot"
  )
  for (i in seq_len(nrow(body_matrix))) {
    lines <- c(lines, paste0(paste(body_matrix[i, ], collapse = " & "), " \\\\"))
  }
  lines <- c(lines, "\\end{longtable}")
  if (!is.null(note)) {
    lines <- c(lines, "\\vspace{-6pt}",
               paste0("\\noindent\\textit{Notes:} ", note))
  }
  lines <- c(lines, "}")
  writeLines(lines, file)
  cat("Wrote", file, "\n")
}

# ----------------------
# 8. LaTeX tables
# ----------------------

# 8.1 Method effect ----------------------------------------------------
mt <- method_comparison %>%
  mutate(
    Outcome = as.character(label),
    `Manuscript est.` = paste0(fmt(ms_estimate), stars(ms_p)),
    `Manuscript p` = fmt_p(ms_p),
    `Manuscript d` = fmt(ms_d),
    `Manuscript verdict` = ms_verdict,
    `LMM specification` = source,
    `LMM est.` = paste0(fmt(estimate), stars(p_value)),
    `LMM SE` = fmt(se),
    `LMM df` = fmt(df, 1),
    `LMM p` = fmt_p(p_value),
    `LMM d` = fmt(lmm_d),
    `LMM verdict` = lmm_verdict,
    Flag = flag_cell(flag)
  )

mt_body <- as.matrix(mt %>% select(Outcome, `Manuscript est.`, `Manuscript p`,
                                   `Manuscript d`, `Manuscript verdict`,
                                   `LMM specification`, `LMM est.`, `LMM SE`,
                                   `LMM df`, `LMM p`, `LMM d`, `LMM verdict`, Flag))
mt_body[, "Outcome"] <- esc(mt_body[, "Outcome"])
mt_body[, "LMM specification"] <- esc(mt_body[, "LMM specification"])

write_latex_table(
  mt_body,
  header = c("Outcome", "Est.", "$p$", "$d$", "Verdict",
             "LMM specification", "Est.", "SE", "df", "$p$", "$d$", "Verdict", "Flag"),
  file = out_path("method_effect_comparison.tex"),
  caption = "Method effect (GenAI $-$ Traditional): currently reported paired $t$-test versus linear mixed model counterparts",
  label = "tab:lmm_method_comparison",
  colspec = paste0("l", strrep("r", 4), "l", strrep("r", 6), "l"),
  note = paste(
    "Left block reproduces the paired $t$-test and paired Cohen's $d$ currently reported",
    "(\\texttt{paired\\_ttests.csv}, \\texttt{effect\\_sizes.csv}), $N=55$, $df=54$.",
    "Right block gives the mixed-model counterpart of the same contrast, obtained with",
    "\\texttt{emmeans} using proportional weighting and Satterthwaite degrees of freedom.",
    "D0/D1 are session-grain models on change scores; L0/L1 are wave-grain models on the",
    "Likert levels; the GLS row uses a compound-symmetry correlation that, unlike",
    "\\texttt{lme4}, admits a negative within-participant correlation.",
    "$d$ for the LMM rows is the contrast divided by $\\sqrt{\\tau^2+\\sigma^2}$ from D0,",
    "the mixed-model analogue of the pooled SD used by the reported Cohen's $d$.",
    "Significance: $^{\\dagger}p<0.1$; $^{*}p<0.05$; $^{**}p<0.01$; $^{***}p<0.001$."
  ),
  env = "sidewaystable"
)

# 8.2 Total change -----------------------------------------------------
tt <- total_comparison %>%
  mutate(
    Outcome = as.character(label),
    `Manuscript est.` = paste0(fmt(ms_estimate), stars(ms_p)),
    `Manuscript p` = fmt_p(ms_p),
    `Manuscript d` = fmt(ms_d),
    `Manuscript verdict` = ms_verdict,
    Spec = source,
    `LMM est.` = paste0(fmt(estimate), stars(p_value)),
    `LMM SE` = fmt(se),
    `LMM p (2-sided)` = fmt_p(p_value),
    `LMM p (1-sided)` = fmt_p(p_one_sided),
    `LMM d` = fmt(lmm_d),
    `LMM verdict` = lmm_verdict_one,
    Flag = flag_cell(flag)
  )

tt_body <- as.matrix(tt %>% select(Outcome, `Manuscript est.`, `Manuscript p`,
                                   `Manuscript d`, `Manuscript verdict`, Spec,
                                   `LMM est.`, `LMM SE`, `LMM p (2-sided)`,
                                   `LMM p (1-sided)`, `LMM d`, `LMM verdict`, Flag))
tt_body[, "Outcome"] <- esc(tt_body[, "Outcome"])
tt_body[, "Spec"] <- esc(tt_body[, "Spec"])

write_latex_table(
  tt_body,
  header = c("Outcome", "Est.", "$p$", "$d$", "Verdict", "LMM specification",
             "Est.", "SE", "$p$ (2-sided)", "$p$ (1-sided)", "$d$", "Verdict", "Flag"),
  file = out_path("total_change_comparison.tex"),
  caption = "Total change (Wave 3 $-$ Wave 1): currently reported one-sample $t$-test versus linear mixed model counterparts",
  label = "tab:lmm_total_change_comparison",
  colspec = paste0("l", strrep("r", 4), "l", strrep("r", 6), "l"),
  note = paste(
    "The currently reported test is a \\emph{one-sided} one-sample $t$-test",
    "(\\texttt{alternative = \"greater\"}); the mixed-model contrast is two-sided by",
    "default, so both are shown and the verdict column compares like with like.",
    "L0/L1 are wave-grain random-intercept models on the Likert levels.",
    "$d$ for the LMM rows is the contrast divided by $\\sqrt{2\\sigma^2}$ from L0.",
    "Significance: $^{\\dagger}p<0.1$; $^{*}p<0.05$; $^{**}p<0.01$; $^{***}p<0.001$."
  ),
  env = "sidewaystable"
)

# 8.3 Order / location / carryover -------------------------------------
ot <- bind_rows(
  order_comparison %>%
    transmute(Outcome = as.character(label), Contrast = contrast,
              ms_estimate, ms_p, ms_verdict, estimate, se, df, p_value,
              lmm_verdict, flag),
  location_comparison %>%
    transmute(Outcome = as.character(label),
              Contrast = paste0("Session change: Street - Park (", source, ")"),
              ms_estimate, ms_p, ms_verdict, estimate, se, df, p_value,
              lmm_verdict, flag)
) %>%
  mutate(Outcome = factor(Outcome, levels = outcomes$label)) %>%
  arrange(Outcome, Contrast)

ot_body <- as.matrix(ot %>% transmute(
  Outcome = esc(as.character(Outcome)),
  Contrast = esc(Contrast),
  `MS est.` = fmt(ms_estimate),
  `MS p` = fmt_p(ms_p),
  `MS verdict` = ifelse(is.na(ms_verdict), "--", ms_verdict),
  `LMM est.` = paste0(fmt(estimate), stars(p_value)),
  `LMM SE` = fmt(se),
  `LMM df` = fmt(df, 1),
  `LMM p` = fmt_p(p_value),
  `LMM verdict` = lmm_verdict,
  Flag = flag_cell(flag)
))

write_latex_longtable(
  ot_body,
  header = c("Outcome", "Contrast", "MS est.", "MS $p$", "MS verdict",
             "LMM est.", "SE", "df", "$p$", "Verdict", "Flag"),
  file = out_path("order_effect_comparison.tex"),
  caption = "Order, location and carryover effects: currently reported tests versus linear mixed model counterparts",
  label = "tab:lmm_order_comparison",
  colspec = "llrrlrrrrll",
  note = paste(
    "``MS'' columns are the currently reported independent-samples $t$-tests on total",
    "change (Welch) and paired $t$-tests on session change; ``--'' marks contrasts with",
    "no counterpart in the current analysis. The method $\\times$ position (carryover)",
    "contrast has no manuscript counterpart and is new to the mixed-model rebuild."
  )
)

# 8.4 Covariate comparison (full) --------------------------------------
cc_body <- as.matrix(covariate_comparison %>% transmute(
  Outcome = esc(as.character(label)),
  Method = esc(as.character(method)),
  Term = esc(as.character(term)),
  `OLS est.` = paste0(fmt(ols_estimate), stars(ols_p)),
  `OLS SE` = fmt(ols_se),
  `OLS p` = fmt_p(ols_p),
  `LMM est.` = paste0(fmt(lmm_estimate), stars(lmm_p)),
  `LMM SE` = fmt(lmm_se),
  `LMM p` = fmt_p(lmm_p),
  Flag = flag_cell(flag)
))

write_latex_longtable(
  cc_body,
  header = c("Outcome", "Method", "Term", "OLS est.", "SE", "$p$",
             "LMM est.", "SE", "$p$", "Flag"),
  file = out_path("covariate_comparison.tex"),
  caption = "Every coefficient of the currently reported per-method OLS models beside its linear mixed model counterpart",
  label = "tab:lmm_covariate_comparison",
  colspec = "lllrrrrrrl",
  note = paste(
    "OLS columns are the models behind Table~\\ref{tab:bootstrap_results}",
    "(\\texttt{4\\_regression\\_analysis.R}), fitted separately to the GenAI and",
    "Traditional change scores. LMM columns come from a single model per outcome,",
    "\\texttt{y \\textasciitilde{} 0 + method + method:(covariates) + (1|pid)}, whose",
    "coefficients are the within-method slopes. Because both methods share an identical",
    "design matrix, the point estimates coincide by construction; what differs is the",
    "standard error, which pools the residual variance across methods and adds a",
    "participant random intercept.",
    "Significance: $^{\\dagger}p<0.1$; $^{*}p<0.05$; $^{**}p<0.01$; $^{***}p<0.001$."
  )
)

# 8.5 Covariate disagreements only -------------------------------------
if (nrow(covariate_disagreements) > 0) {
  cd_body <- as.matrix(covariate_disagreements %>% transmute(
    Outcome = esc(as.character(label)),
    Method = esc(as.character(method)),
    Term = esc(as.character(term)),
    `OLS est.` = fmt(ols_estimate),
    `OLS p` = fmt_p(ols_p),
    `OLS verdict` = ols_verdict,
    `LMM est.` = fmt(lmm_estimate),
    `LMM p` = fmt_p(lmm_p),
    `LMM verdict` = lmm_verdict
  ))
  write_latex_table(
    cd_body,
    header = c("Outcome", "Method", "Term", "OLS est.", "OLS $p$", "OLS verdict",
               "LMM est.", "LMM $p$", "LMM verdict"),
    file = out_path("covariate_disagreements.tex"),
    caption = "Coefficients whose significance verdict changes between the currently reported OLS models and the mixed model",
    label = "tab:lmm_covariate_disagreements",
    colspec = "lllrrlrrl",
    note = "Verdicts compared at $\\alpha = 0.05$. Point estimates are identical by construction; only the standard errors differ."
  )
} else {
  writeLines(
    c("% No covariate coefficient changed its significance verdict between",
      "% the OLS models and the mixed model at alpha = 0.05."),
    out_path("covariate_disagreements.tex")
  )
}

# 8.6 Wave-level model fixed effects -----------------------------------
wave_term_labels <- c(
  term_labels,
  "waveW2" = "Wave 2",
  "waveW3" = "Wave 3",
  "waveW2:genai_first_fGenAIFirst" = "Wave 2 x GenAI First",
  "waveW3:genai_first_fGenAIFirst" = "Wave 3 x GenAI First",
  "waveW2:street_first_fStreetFirst" = "Wave 2 x Street First",
  "waveW3:street_first_fStreetFirst" = "Wave 3 x Street First"
)

fx_wide <- fixef_all %>%
  mutate(
    cell = paste0(fmt(estimate), stars(p.value), " (", fmt(std.error), ")"),
    term_pretty = if_else(term %in% names(wave_term_labels),
                          unname(wave_term_labels[term]), term)
  ) %>%
  select(term_pretty, label, cell) %>%
  pivot_wider(names_from = label, values_from = cell, values_fill = "--")

# Only the term column is escaped: the value cells already contain LaTeX
# (significance daggers) and must be passed through verbatim.
fx_body <- as.matrix(fx_wide)
fx_body[, "term_pretty"] <- esc(fx_wide$term_pretty)

write_latex_table(
  fx_body,
  header = c("Term", colnames(fx_wide)[-1]),
  file = out_path("lmm_wave_model_fixed_effects.tex"),
  caption = "Adjusted wave-level linear mixed model (L1): fixed effects",
  label = "tab:lmm_wave_fixed_effects",
  colspec = paste0("l", strrep("r", ncol(fx_wide) - 1)),
  note = paste(
    "Model: \\texttt{score \\textasciitilde{} wave * genai\\_first + wave * street\\_first",
    "+ covariates + (1|participant)}, fitted to the Likert levels ($-2$ to $2$) at all",
    "three waves, $N = 55$ participants, 165 observations. Treatment contrasts;",
    "reference cell is Wave 1, Conventional-first, Park-first, White, Man,",
    "Cycling/scootering, Rural. Standard errors in parentheses;",
    "$p$-values from Satterthwaite approximation.",
    "Significance: $^{\\dagger}p<0.1$; $^{*}p<0.05$; $^{**}p<0.01$; $^{***}p<0.001$."
  ),
  env = "sidewaystable"
)

# 8.7 Variance components ---------------------------------------------
vc_body <- as.matrix(variance_components %>% transmute(
  Model = esc(model),
  `Obs.` = as.character(n_obs),
  `Participants` = as.character(n_groups),
  `$\\tau^2$` = fmt(tau2, 4),
  `$\\sigma^2$` = fmt(sigma2, 4),
  ICC = fmt(icc, 3),
  `Singular` = if_else(singular, "yes", "no")
))

write_latex_table(
  vc_body,
  header = c("Model", "Obs.", "Participants", "$\\tau^2$", "$\\sigma^2$", "ICC", "Singular"),
  file = out_path("variance_components.tex"),
  caption = "Variance components and singularity status of every fitted mixed model",
  label = "tab:lmm_variance_components",
  colspec = "lrrrrrl",
  note = paste(
    "$\\tau^2$ is the participant random-intercept variance, $\\sigma^2$ the residual",
    "variance, ICC $= \\tau^2/(\\tau^2+\\sigma^2)$. Models prefixed D operate on",
    "session-level change scores, models prefixed L on wave-level Likert levels.",
    "A singular fit means $\\tau^2$ was estimated at the boundary of zero:",
    "the two within-participant change scores are negatively correlated",
    "($r \\approx -0.5$ for every outcome), which a non-negative random-intercept",
    "variance cannot represent."
  )
)

# ----------------------
# 9. Markdown summary
# ----------------------
md <- c(
  "# LMM rebuild vs currently reported results",
  "",
  "Generated by `genai_experiment/modeling/11_lmm_analysis.R`. Exploratory only.",
  paste0("N = ", nlevels(df$pid), " participants; ", nrow(sess),
         " session-level rows; ", nrow(wavelong), " wave-level rows."),
  "",
  "## 1. Method effect (GenAI - Traditional)",
  "",
  "| Outcome | MS est. | MS p | MS d | MS verdict | LMM spec | LMM est. | LMM p | LMM d | LMM verdict | Flag |",
  "|---|---|---|---|---|---|---|---|---|---|---|"
)
md <- c(md, method_comparison %>%
  transmute(row = paste0("| ", label, " | ", fmt(ms_estimate), " | ", fmt(ms_p),
                         " | ", fmt(ms_d), " | ", ms_verdict, " | ", source, " | ",
                         fmt(estimate), " | ", fmt(p_value), " | ", fmt(lmm_d),
                         " | ", lmm_verdict, " | ",
                         if_else(flag == "", "agree", "**DISAGREEMENT**"), " |")) %>%
  pull(row))

md <- c(md, "", "## 2. Total change (Wave 3 - Wave 1)", "",
        "| Outcome | MS est. | MS p (1-sided) | MS d | MS verdict | LMM spec | LMM est. | LMM p (2-sided) | LMM p (1-sided) | LMM d | LMM verdict | Flag |",
        "|---|---|---|---|---|---|---|---|---|---|---|---|")
md <- c(md, total_comparison %>%
  transmute(row = paste0("| ", label, " | ", fmt(ms_estimate), " | ", fmt(ms_p, 5),
                         " | ", fmt(ms_d), " | ", ms_verdict, " | ", source, " | ",
                         fmt(estimate), " | ", fmt(p_value, 5), " | ",
                         fmt(p_one_sided, 5), " | ", fmt(lmm_d), " | ",
                         lmm_verdict_one, " | ",
                         if_else(flag == "", "agree", "**DISAGREEMENT**"), " |")) %>%
  pull(row))

md <- c(md, "", "## 3. Order, location and carryover", "",
        "| Outcome | Contrast | MS est. | MS p | MS verdict | LMM est. | LMM p | LMM verdict | Flag |",
        "|---|---|---|---|---|---|---|---|---|")
md <- c(md, ot %>%
  transmute(row = paste0("| ", Outcome, " | ", Contrast, " | ", fmt(ms_estimate),
                         " | ", fmt(ms_p), " | ",
                         if_else(is.na(ms_verdict), "-", ms_verdict), " | ",
                         fmt(estimate), " | ", fmt(p_value), " | ", lmm_verdict,
                         " | ", if_else(flag == "", "agree",
                                        if_else(flag == "DISAGREEMENT",
                                                "**DISAGREEMENT**", flag)), " |")) %>%
  pull(row))

md <- c(md, "", "## 4. Covariate coefficients: verdict changes", "")
if (nrow(covariate_disagreements) > 0) {
  md <- c(md,
          paste0(nrow(covariate_disagreements), " of ", nrow(covariate_comparison),
                 " coefficients change significance verdict at alpha = 0.05.",
                 " Point estimates are identical by construction."),
          "",
          "| Outcome | Method | Term | Est. | OLS p | OLS verdict | LMM p | LMM verdict |",
          "|---|---|---|---|---|---|---|---|")
  md <- c(md, covariate_disagreements %>%
    transmute(row = paste0("| ", label, " | ", method, " | ", term, " | ",
                           fmt(ols_estimate), " | ", fmt(ols_p), " | ", ols_verdict,
                           " | ", fmt(lmm_p), " | ", lmm_verdict, " |")) %>%
    pull(row))
} else {
  md <- c(md, "No coefficient changes its significance verdict at alpha = 0.05.")
}

md <- c(md, "", "## 5. Variance components and convergence", "",
        "| Model | Obs | Participants | tau^2 | sigma^2 | ICC | Singular |",
        "|---|---|---|---|---|---|---|")
md <- c(md, variance_components %>%
  transmute(row = paste0("| ", model, " | ", n_obs, " | ", n_groups, " | ",
                         fmt(tau2, 4), " | ", fmt(sigma2, 4), " | ", fmt(icc),
                         " | ", if_else(singular, "**yes**", "no"), " |")) %>%
  pull(row))

writeLines(md, out_path("lmm_comparison_summary.md"))
cat("Wrote", out_path("lmm_comparison_summary.md"), "\n")

# ----------------------
# 10. Convergence log
# ----------------------
log_msg("")
log_msg("Singular fits: ",
        paste(variance_components$model[variance_components$singular], collapse = ", "))
log_msg("Non-singular fits: ",
        paste(variance_components$model[!variance_components$singular], collapse = ", "))

writeLines(
  c("Convergence and warning log for 11_lmm_analysis.R",
    paste0("Generated: ", format(Sys.time(), "%Y-%m-%d %H:%M:%S")),
    "",
    log_lines,
    "",
    "R session:",
    capture.output(sessionInfo())),
  out_path("convergence_log.txt")
)
cat("Wrote", out_path("convergence_log.txt"), "\n")

cat("\nLMM analysis complete. Outputs in", out_dir, "\n")
