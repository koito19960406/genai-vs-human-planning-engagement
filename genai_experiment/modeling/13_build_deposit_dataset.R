# ==============================================================================
# 13_build_deposit_dataset.R
#
# Builds the reduced, de-identified analysis dataset promised by the Data
# Availability Statement (main.tex) for the Figshare deposit.
#
# Input :  data/processed/survey_processed.csv   (55 x 180, NOT for release)
# Output:  data/deposit/survey_analysis_reduced.csv
#          data/deposit/column_manifest.csv      (disposition of all 180 columns)
#
# Every column of the input falls into exactly one of four dispositions, listed
# explicitly below so the deposit README can be checked against this file.
# ==============================================================================

if (!requireNamespace("pacman", quietly = TRUE)) install.packages("pacman")
pacman::p_load(dplyr, readr, tibble)

in_path   <- "data/processed/survey_processed.csv"
out_dir   <- "data/deposit"
out_path  <- file.path(out_dir, "survey_analysis_reduced.csv")
man_path  <- file.path(out_dir, "column_manifest.csv")

if (!file.exists(in_path)) stop("Processed data not found: ", in_path)
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

df <- read_csv(in_path, show_col_types = FALSE)
stopifnot(nrow(df) == 55, ncol(df) == 180)

# ------------------------------------------------------------------------------
# 1. Direct identifiers and session metadata
#    ResponseId is a live join key to the retained identity file and to the
#    pre-strip blob in git history; it is replaced, not merely deleted, because
#    scripts 6, 7 and 10 need a per-participant key (10 clusters its bootstrap
#    on it).
# ------------------------------------------------------------------------------
drop_identifiers <- c(
  "ResponseId",                                    # replaced by participant_id
  "IPAddress", "LocationLatitude", "LocationLongitude",
  "StartDate", "EndDate", "RecordedDate", "Duration (in seconds)",
  "RecipientLastName", "RecipientFirstName", "RecipientEmail",
  "ExternalReference", "Status"
)

# Per-page response latencies: 55 distinct millisecond values per participant.
drop_paradata <- c(
  "Q127_First Click", "Q127_Last Click", "Q127_Page Submit", "Q127_Click Count",
  "Q128_First Click", "Q128_Last Click", "Q128_Page Submit", "Q128_Click Count"
)

# Constant across all 55 rows: no information, no reason to ship.
drop_constant <- c("Progress", "Finished", "DistributionChannel", "UserLanguage")

# ------------------------------------------------------------------------------
# 2. Quasi-identifiers that no deposited script reads
#    College, Year_of_Study and Housing_Location are referenced only by
#    2_main_variable_construction.R, which reads the raw .xlsx and is not
#    deposited. College is the single largest re-identification lever in the
#    file (two n=1 cells; adding it to race x gender x hometown x year takes
#    uniques from 15 to 34), so it costs nothing to drop and buys the most.
#    QID92 (prior planning participation) is likewise unread and distinguishes
#    the 4 participants who did not answer "None of the above".
# ------------------------------------------------------------------------------
drop_unused_qids <- c(
  "College", "Year_of_Study", "Year_of_Study_num", "Housing_Location", "QID92"
)

# ------------------------------------------------------------------------------
# 3. Duplicates, empties and unread substantive items
#    The .y columns are byte-identical to their .x twins (verified).
# ------------------------------------------------------------------------------
drop_redundant <- c(
  "AI_Strengths.y", "AI_Strengths_Other.y",
  "AI_Limitations.y", "AI_Limitations_Other.y",
  "Engagement_Interest_Type.x", "Engagement_Interest_Type.y",
  "AI_Strengths_Split", "AI_Limitations_Split", "Engagement_Interest_Split",
  "QID100",                    # single value, "''"
  "QID133", "QID134"           # per-method effectiveness ratings, read by nothing
)

# ------------------------------------------------------------------------------
# 4. Free text: values emptied, headers kept.
#    Kept as columns because 5_strength_weakness_effectiveness.R and
#    7_future_suggestion.R both stop() on missing columns; emptying releases no
#    prose while leaving those two scripts runnable.
#    Note the multi-select label columns (Traditional_Strengths, AI_Limitations.x,
#    QID148, QID150, ...) are NOT free text and are retained in full.
# ------------------------------------------------------------------------------
empty_freetext <- c(
  "QID152",                        # 34 open-ended responses (9 already quoted in main.tex)
  "QID151",                        # 1 response
  "Traditional_Strengths_Other",   # 1
  "Traditional_Limitations_Other", # 2
  "AI_Strengths_Other.x",          # 1
  "AI_Limitations_Other.x"         # 2
)

to_drop <- c(drop_identifiers, drop_paradata, drop_constant,
             drop_unused_qids, drop_redundant)

stopifnot(all(to_drop %in% names(df)), all(empty_freetext %in% names(df)))

# ------------------------------------------------------------------------------
# Build
#
# Row order in the input is chronological, and the first six rows are the six
# Singapore-recorded sessions -- so order alone reconstructs the session
# schedule even after the timestamps are dropped. Rows are therefore shuffled
# and participant_id assigned AFTER the shuffle, so the id carries no sequence
# information.
#
# Do NOT be tempted to rank participant_id by ResponseId instead. That would
# make 10_carryover_analysis.R -- which does arrange(ResponseId, period) and
# clusters its bootstrap on the key -- resample in the published order and
# reproduce its intervals exactly. But Qualtrics ResponseIds are partly
# time-ordered in their leading character: five of the six Singapore sessions
# begin "R_9" and sort to the last five positions. Ranking on them leaks the
# very cluster the shuffle exists to hide.
#
# Cost of the shuffle, measured: every point estimate, mean, SD, t, df, p and
# effect size in the carryover outputs is bit-identical; only bootstrap
# interval endpoints and SEs move, by <= 0.045.
# ------------------------------------------------------------------------------
set.seed(20260811)
shuffled <- df[sample(nrow(df)), ]

reduced <- shuffled %>%
  select(-all_of(to_drop)) %>%
  mutate(across(all_of(empty_freetext), ~ NA_character_)) %>%
  mutate(participant_id = sprintf("p%02d", seq_len(nrow(.)))) %>%
  relocate(participant_id)

# Scripts 6, 7 and 10 address the key by name.
reduced$ResponseId <- reduced$participant_id

write_csv(reduced, out_path, na = "")

# ------------------------------------------------------------------------------
# Manifest: one row per input column, so the README's claims are checkable
# ------------------------------------------------------------------------------
disposition <- function(col) {
  # ResponseId is dropped as an identifier, but its NAME is reused above for the
  # pseudonymous key, because scripts 6, 7 and 10 address the key by that name.
  # Saying only "dropped" would contradict the file, which does carry the column.
  if (col == "ResponseId")        return("dropped: direct identifier; column name reused for the pseudonymous key (= participant_id)")
  if (col %in% drop_identifiers)  return("dropped: direct identifier / session metadata")
  if (col %in% drop_paradata)     return("dropped: page-timing paradata")
  if (col %in% drop_constant)     return("dropped: constant")
  if (col %in% drop_unused_qids)  return("dropped: quasi-identifier, unread by deposited scripts")
  if (col %in% drop_redundant)    return("dropped: duplicate, empty or unread")
  if (col %in% empty_freetext)    return("emptied: free text (column header retained)")
  "retained"
}

manifest <- tibble(
  column      = names(df),
  disposition = vapply(names(df), disposition, character(1))
) %>%
  add_row(column = "participant_id",
          disposition = "added: pseudonymous key replacing ResponseId",
          .before = 1)

write_csv(manifest, man_path)

cat(sprintf("Wrote %s: %d rows x %d cols (from 55 x 180)\n",
            out_path, nrow(reduced), ncol(reduced)))
cat(sprintf("Dropped %d columns, emptied %d, added participant_id\n",
            length(to_drop), length(empty_freetext)))
