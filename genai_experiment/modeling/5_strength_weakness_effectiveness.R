# =============================================================
# 5_strength_weakness_effectiveness.R
# Analysis of strengths, weaknesses, and effectiveness of visualization methods
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
  wordcloud,
  tm,
  RColorBrewer,
  viridis,
  corrplot
)

source("genai_experiment/modeling/theme_beautiful.R")

# Configurable paths
processed_data_path <- "data/processed/survey_processed.csv"
model_dir <- "reports/models/strength_weakness/"
fig_dir <- "reports/figures/strength_weakness/"

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
  "Traditional_Strengths", "Traditional_Limitations", 
  "Traditional_Strengths_Other", "Traditional_Limitations_Other", 
  "AI_Strengths.x", "AI_Limitations.x", 
  "AI_Strengths_Other.x", "AI_Limitations_Other.x", 
  "AI_Influence"
)
check_columns(df, required_vars)

# ----------------------
# 2. Data Preparation Functions
# ----------------------
# Function to count and prepare multi-select data for plotting
prepare_multiselect_data <- function(df, prefix, value_label) {
  columns <- names(df)[startsWith(names(df), prefix)]
  other_col <- columns[grep("Other", columns)]
  columns <- setdiff(columns, other_col)
  counts <- sapply(columns, function(col) sum(df[[col]], na.rm = TRUE))
  result_df <- data.frame(
    option = columns,
    count = as.numeric(counts),
    method = if(grepl("Trad", prefix)) "Traditional" else "AI",
    type = value_label
  ) %>%
    mutate(
      option = case_when(
        option == "Trad_Strength_Speed" ~ "Speed of creating visualizations",
        option == "Trad_Strength_Accuracy" ~ "Accuracy in representing ideas",
        option == "Trad_Strength_Ease" ~ "Ease of use",
        option == "Trad_Strength_Creative_Freedom" ~ "Creative freedom",
        option == "Trad_Strength_Quality" ~ "Quality of final visualization",
        option == "Trad_Strength_Control" ~ "Control over details",
        option == "Trad_Limitation_Time" ~ "Time-consuming",
        option == "Trad_Limitation_Artistic" ~ "Limited by artistic ability",
        option == "Trad_Limitation_Perspective" ~ "Difficulty showing perspective/scale",
        option == "Trad_Limitation_Appeal" ~ "Final result lacked visual appeal",
        option == "Trad_Limitation_Changes" ~ "Hard to make changes/iterations",
        option == "Trad_Limitation_Realistic" ~ "Not realistic enough",
        option == "Trad_Limitation_Detail" ~ "Limited detail",
        option == "AI_Strength_Speed" ~ "Speed of creating visualizations",
        option == "AI_Strength_Accuracy" ~ "Accuracy in representing ideas",
        option == "AI_Strength_Ease" ~ "Ease of use",
        option == "AI_Strength_Creative_Inspiration" ~ "Creative inspiration",
        option == "AI_Strength_Quality" ~ "Quality of final visualization",
        option == "AI_Strength_Realistic" ~ "Ability to create realistic images",
        option == "AI_Limitation_Control" ~ "Difficulty controlling specific details",
        option == "AI_Limitation_Accuracy" ~ "Did not accurately represent ideas",
        option == "AI_Limitation_Generic" ~ "Results were too generic",
        option == "AI_Limitation_Learning" ~ "Learning curve for effective prompting",
        option == "AI_Limitation_Unexpected" ~ "Unexpected elements in images",
        option == "AI_Limitation_Creative" ~ "Limited creative freedom",
        option == "AI_Limitation_Technical" ~ "Technical issues or limitations",
        TRUE ~ option
      )
    ) %>%
    mutate(
      percentage = count / nrow(df) * 100,
      option = fct_reorder(option, count)
    )
  return(result_df)
}

# Function to create word clouds with better formatting
create_wordcloud <- function(text_data, title, filename) {
  # Remove NAs and empty strings
  text_data <- text_data[!is.na(text_data) & text_data != ""]
  
  if(length(text_data) > 0) {
    # Create corpus
    corpus <- Corpus(VectorSource(text_data))
    
    # Text preprocessing
    corpus <- corpus %>%
      tm_map(content_transformer(tolower)) %>%
      tm_map(removePunctuation) %>%
      tm_map(removeNumbers) %>%
      tm_map(removeWords, stopwords("english")) %>%
      tm_map(stripWhitespace)
    
    # Create document term matrix
    dtm <- TermDocumentMatrix(corpus)
    matrix <- as.matrix(dtm)
    words <- sort(rowSums(matrix), decreasing = TRUE)
    word_df <- data.frame(word = names(words), freq = words)
    
    # Set up plotting device
    png(
      filename = filename,
      width = 800,
      height = 800,
      res = 100
    )
    
    # Create word cloud with better parameters
    wordcloud(
      words = word_df$word, 
      freq = word_df$freq, 
      min.freq = 2,
      max.words = 100,
      random.order = FALSE, 
      rot.per = 0.35,
      colors = viridis(8),
      scale = c(4, 0.5)
    )
    
    # Add title
    title(main = title, col.main = "black", font.main = 2)
    
    # Close device
    dev.off()
  } else {
    warning(paste("No data available for", title))
  }
}

# ----------------------
# 3. Analysis of Strengths and Limitations
# ----------------------
# Prepare the data for all strengths and weaknesses
traditional_strengths <- prepare_multiselect_data(df, "Trad_Strength_", "Strength")
traditional_limitations <- prepare_multiselect_data(df, "Trad_Limitation_", "Limitation")
ai_strengths <- prepare_multiselect_data(df, "AI_Strength_", "Strength")
ai_limitations <- prepare_multiselect_data(df, "AI_Limitation_", "Limitation")

# Combine all data for plotting
all_data <- bind_rows(
  traditional_strengths,
  traditional_limitations,
  ai_strengths,
  ai_limitations
)

# save to csv
write_csv(all_data, paste0(model_dir, "strengths_limitations_data.csv"))

# Plot all strengths and limitations in one figure with four facets
p1 <- ggplot(all_data, aes(x = percentage, y = option, fill = method)) +
  geom_col(color = "black", alpha = 0.85) +
  facet_grid(type ~ method, scales = "free_y", space = "free") +
  scale_fill_manual(values = c("AI" = "#AB84A5FF", "Traditional" = "#75884BFF")) +
  labs(
    title = "Strengths and Limitations of Traditional vs. AI Methods",
    subtitle = "Percentage of respondents who selected each option",
    x = "Percentage (%)",
    y = "",
    fill = "Method"
  ) +
  theme_beautiful()

ggsave(paste0(fig_dir, "strengths_limitations_comparison.png"), p1, width = 14, height = 12, dpi = 300)

# ----------------------
# 4. Analysis of AI Influence
# ----------------------
# Robust mapping for AI Influence
AI_Influence_numeric <- case_when(
  str_detect(df$AI_Influence, "Significantly limited") ~ -2,
  str_detect(df$AI_Influence, "Somewhat limited") ~ -1,
  str_detect(df$AI_Influence, "Neither limited nor expanded|Had no impact") ~ 0,
  str_detect(df$AI_Influence, "Somewhat expanded") ~ 1,
  str_detect(df$AI_Influence, "Significantly expanded") ~ 2,
  TRUE ~ NA_real_
)
df$AI_Influence_numeric <- AI_Influence_numeric

# Order the factor levels of AI_Influence based on the numeric mapping
influence_levels <- df %>%
  select(AI_Influence, AI_Influence_numeric) %>%
  distinct() %>%
  arrange(AI_Influence_numeric) %>%
  pull(AI_Influence)

df$AI_Influence <- factor(df$AI_Influence, levels = influence_levels)

# Create histogram of AI influence with sorted bars
p2 <- ggplot(df, aes(x = AI_Influence)) +
  geom_bar(stat = "count", fill = "#75884BFF", color = "white", alpha = 0.9) +
  labs(
      title = "Perceived Influence of AI on Thinking",
      x = "Influence of AI",
      y = "Number of Participants"
  ) +
  theme_beautiful() +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 15))

ggsave(paste0(fig_dir, "ai_influence_histogram.png"), p2, width = 10, height = 10, dpi = 300)

# Create word clouds for "Other" responses
if ("Traditional_Strengths_Other" %in% names(df)) {
  create_wordcloud(
    df$Traditional_Strengths_Other, 
    "Other Strengths of Traditional Drawing Method", 
    paste0(fig_dir, "traditional_strengths_other_wordcloud.png")
  )
}
if ("Traditional_Limitations_Other" %in% names(df)) {
  create_wordcloud(
    df$Traditional_Limitations_Other, 
    "Other Limitations of Traditional Drawing Method", 
    paste0(fig_dir, "traditional_limitations_other_wordcloud.png")
  )
}
if ("AI_Strengths_Other" %in% names(df)) {
  create_wordcloud(
    df$AI_Strengths_Other, 
    "Other Strengths of AI-Assisted Visualization", 
    paste0(fig_dir, "ai_strengths_other_wordcloud.png")
  )
}
if ("AI_Limitations_Other" %in% names(df)) {
  create_wordcloud(
    df$AI_Limitations_Other, 
    "Other Limitations of AI-Assisted Visualization", 
    paste0(fig_dir, "ai_limitations_other_wordcloud.png")
  )
}

# ----------------------
# 6. Statistical Analysis
# ----------------------
# T-test to assess if the mean influence is significantly different from 0 (no impact)
t_test_influence <- t.test(df$AI_Influence_numeric, mu = 0)

# Save t-test results
sink(paste0(model_dir, "ai_influence_ttest.txt"))
print("T-test Results for AI Influence on Thinking")
print("------------------------------------------")
print(t_test_influence)
sink()

# ----------------------
# 7. End of Script
# ----------------------
cat("\nAnalysis complete. Outputs saved to", model_dir, "and", fig_dir, "\n")



