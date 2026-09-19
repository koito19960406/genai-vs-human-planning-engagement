# =============================================================
# 7_future_suggestion.R
# Analysis of future suggestions and participation barriers
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
  wordcloud,
  tm,
  RColorBrewer,
  viridis,
  tibble
)

source("genai_experiment/modeling/theme_beautiful.R")

# Configurable paths
processed_data_path <- "data/processed/survey_processed.csv"
model_dir <- "reports/models/future_suggestion/"
fig_dir <- "reports/figures/future_suggestion/"

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
  "QID148", "QID149", "QID150", "QID151", "Gender", "Race_Ethnicity", "AI_Experience_num", "Planning_Knowledge_num", "ResponseId"
)
check_columns(df, required_vars)

# Rename for clarity
# (If already renamed, this is idempotent)
df <- df %>%
  rename(
    AI_Improvements = QID148,
    AI_Improvements_Other = QID149,
    Participation_Barriers = QID150,
    Participation_Barriers_Other = QID151
  )

# ----------------------
# 2. Analysis of AI Tool Improvement Suggestions
# ----------------------
df_ai_improvements <- df %>%
  mutate(
    AI_Improvements_Split = strsplit(as.character(AI_Improvements), ",")
  ) %>%
  mutate(
    Improvement_Interface = map_int(AI_Improvements_Split, ~as.integer(any(grepl("Easier interface", .)))),
    Improvement_Accuracy = map_int(AI_Improvements_Split, ~as.integer(any(grepl("More accurate representation of specific ideas", .)))),
    Improvement_Control = map_int(AI_Improvements_Split, ~as.integer(any(grepl("Better control over image details", .)))),
    Improvement_Speed = map_int(AI_Improvements_Split, ~as.integer(any(grepl("Faster generation time", .)))),
    Improvement_Options = map_int(AI_Improvements_Split, ~as.integer(any(grepl("More diverse design options", .)))),
    Improvement_Maps = map_int(AI_Improvements_Split, ~as.integer(any(grepl("Integration with maps/real locations", .)))),
    Improvement_Tutorial = map_int(AI_Improvements_Split, ~as.integer(any(grepl("Better instructions/tutorials", .)))),
    Improvement_Other = map_int(AI_Improvements_Split, ~as.integer(any(grepl("Other", .))))
  )

improvement_data <- df_ai_improvements %>%
  select(starts_with("Improvement_")) %>%
  pivot_longer(
    cols = everything(),
    names_to = "improvement_type",
    values_to = "selected"
  ) %>%
  mutate(
    improvement_type = case_when(
      improvement_type == "Improvement_Interface" ~ "Easier interface",
      improvement_type == "Improvement_Accuracy" ~ "More accurate representation of ideas",
      improvement_type == "Improvement_Control" ~ "Better control over image details",
      improvement_type == "Improvement_Speed" ~ "Faster generation time",
      improvement_type == "Improvement_Options" ~ "More diverse design options",
      improvement_type == "Improvement_Maps" ~ "Integration with maps/real locations",
      improvement_type == "Improvement_Tutorial" ~ "Better instructions/tutorials",
      improvement_type == "Improvement_Other" ~ "Other",
      TRUE ~ improvement_type
    )
  ) %>%
  group_by(improvement_type) %>%
  summarise(
    count = sum(selected, na.rm = TRUE),
    percentage = count / nrow(df) * 100
  ) %>%
  arrange(desc(count)) %>%
  mutate(improvement_type = factor(improvement_type, levels = improvement_type))

# Beautified plot
p1 <- ggplot(improvement_data, aes(x = improvement_type, y = percentage)) +
  geom_col(color = "black", alpha = 0.85, fill = "#D8B847FF") +
  coord_flip() +
  labs(
    title = "AI Improvement Suggestions from Participants",
    x = "",
    y = "Percentage of Participants (%)",
    caption = "Participants could select multiple options"
  ) +
  theme_beautiful() +
  theme(legend.position = "none")

ggsave(paste0(fig_dir, "ai_improvement_suggestions.png"), p1, width = 12, height = 8, dpi = 300)
write_excel_csv(improvement_data, paste0(model_dir, "ai_improvement_suggestions.csv"))

# ----------------------
# 3. Analysis of Participation Barriers
# ----------------------
df_barriers <- df %>%
  mutate(
    Participation_Barriers_Split = strsplit(as.character(Participation_Barriers), ",")
  ) %>%
  mutate(
    Barrier_Time = map_int(Participation_Barriers_Split, ~as.integer(any(grepl("Lack of time", .)))),
    Barrier_Knowledge = map_int(Participation_Barriers_Split, ~as.integer(any(grepl("Lack of knowledge about urban planning", .)))),
    Barrier_Skepticism = map_int(Participation_Barriers_Split, ~as.integer(any(grepl("Skepticism that my input will matter", .)))),
    Barrier_Interest = map_int(Participation_Barriers_Split, ~as.integer(any(grepl("Lack of interest in the topics", .)))),
    Barrier_Location = map_int(Participation_Barriers_Split, ~as.integer(any(grepl("Inconvenient meeting times/locations", .)))),
    Barrier_Process = map_int(Participation_Barriers_Split, ~as.integer(any(grepl("Complicated participation processes", .)))),
    Barrier_Technical = map_int(Participation_Barriers_Split, ~as.integer(any(grepl("Technical barriers", .)))),
    Barrier_Other = map_int(Participation_Barriers_Split, ~as.integer(any(grepl("Other", .))))
  )

barrier_data <- df_barriers %>%
  select(starts_with("Barrier_")) %>%
  pivot_longer(
    cols = everything(),
    names_to = "barrier_type",
    values_to = "selected"
  ) %>%
  mutate(
    barrier_type = case_when(
      barrier_type == "Barrier_Time" ~ "Lack of time",
      barrier_type == "Barrier_Knowledge" ~ "Lack of knowledge about urban planning",
      barrier_type == "Barrier_Skepticism" ~ "Skepticism that my input will matter",
      barrier_type == "Barrier_Interest" ~ "Lack of interest in the topics",
      barrier_type == "Barrier_Location" ~ "Inconvenient meeting times/locations",
      barrier_type == "Barrier_Process" ~ "Complicated participation processes",
      barrier_type == "Barrier_Technical" ~ "Technical barriers",
      barrier_type == "Barrier_Other" ~ "Other",
      TRUE ~ barrier_type
    )
  ) %>%
  group_by(barrier_type) %>%
  summarise(
    count = sum(selected, na.rm = TRUE),
    percentage = count / nrow(df) * 100
  ) %>%
  arrange(desc(count)) %>%
  mutate(barrier_type = factor(barrier_type, levels = barrier_type))

p2 <- ggplot(barrier_data, aes(x = barrier_type, y = percentage)) +
  geom_col(color = "black", alpha = 0.85, fill = "#5B859EFF") +
  coord_flip() +
  labs(
    title = "Barriers to Participation in Urban Planning",
    x = "",
    y = "Percentage of Participants (%)",
    caption = "Participants could select up to three barriers"
  ) +
  theme_beautiful() +
  theme(legend.position = "none")

ggsave(paste0(fig_dir, "participation_barriers.png"), p2, width = 12, height = 8, dpi = 300)
write_excel_csv(barrier_data, paste0(model_dir, "participation_barriers.csv"))

# ----------------------
# 4. Analysis of Free-form Text Responses
# ----------------------
create_wordcloud <- function(text_data, title, filename) {
  text_data <- text_data[!is.na(text_data) & text_data != ""]
  if(length(text_data) > 0) {
    corpus <- Corpus(VectorSource(text_data))
    corpus <- corpus %>%
      tm_map(content_transformer(tolower)) %>%
      tm_map(removePunctuation) %>%
      tm_map(removeNumbers) %>%
      tm_map(removeWords, stopwords("english")) %>%
      tm_map(stripWhitespace)
    dtm <- TermDocumentMatrix(corpus)
    word_freqs <- sort(rowSums(as.matrix(dtm)), decreasing = TRUE)
    word_df <- data.frame(word = names(word_freqs), freq = word_freqs)
    png(filename, width = 800, height = 600)
    set.seed(123)
    if(nrow(word_df) > 0) {
      wordcloud(
        words = word_df$word, 
        freq = word_df$freq, 
        min.freq = 1,
        max.words = 50, 
        random.order = FALSE, 
        rot.per = 0.35,
        colors = viridis(8)
      )
      title(main = title)
    } else {
      plot.new()
      text(0.5, 0.5, "No sufficient data for word cloud", cex = 1.5)
      title(main = title)
    }
    dev.off()
    return(word_df)
  } else {
    png(filename, width = 800, height = 600)
    plot.new()
    text(0.5, 0.5, "No data available", cex = 1.5)
    title(main = title)
    dev.off()
    return(data.frame(word = character(), freq = numeric()))
  }
}

# Word clouds for free-form text responses
create_wordcloud(
  df$AI_Improvements_Other, 
  "Other Suggested Improvements for AI Tools", 
  paste0(fig_dir, "ai_improvements_other_wordcloud.png")
)
create_wordcloud(
  df$Participation_Barriers_Other, 
  "Other Barriers to Participation", 
  paste0(fig_dir, "participation_barriers_other_wordcloud.png")
)

# Save free-form text responses
data.frame(
  type = c(rep("AI Improvements", sum(!is.na(df$AI_Improvements_Other) & df$AI_Improvements_Other != "")),
           rep("Participation Barriers", sum(!is.na(df$Participation_Barriers_Other) & df$Participation_Barriers_Other != ""))),
  text = c(df$AI_Improvements_Other[!is.na(df$AI_Improvements_Other) & df$AI_Improvements_Other != ""],
           df$Participation_Barriers_Other[!is.na(df$Participation_Barriers_Other) & df$Participation_Barriers_Other != ""])
) %>%
  mutate(text_length = nchar(text)) %>%
  write_excel_csv(paste0(model_dir, "free_text_responses.csv"))

# ----------------------
# 5. Cross-tabulation of Improvements and Barriers with Demographics
# ----------------------
improvement_demographics <- df_ai_improvements %>%
  select(
    ResponseId, Gender, Race_Ethnicity, AI_Experience_num, Planning_Knowledge_num,
    starts_with("Improvement_")
  )

improvement_by_gender <- improvement_demographics %>%
  group_by(Gender) %>%
  summarise(
    n = n(),
    Interface = sum(Improvement_Interface, na.rm = TRUE) / n() * 100,
    Accuracy = sum(Improvement_Accuracy, na.rm = TRUE) / n() * 100,
    Control = sum(Improvement_Control, na.rm = TRUE) / n() * 100,
    Speed = sum(Improvement_Speed, na.rm = TRUE) / n() * 100,
    Options = sum(Improvement_Options, na.rm = TRUE) / n() * 100,
    Maps = sum(Improvement_Maps, na.rm = TRUE) / n() * 100,
    Tutorial = sum(Improvement_Tutorial, na.rm = TRUE) / n() * 100
  )
write_excel_csv(improvement_by_gender, paste0(model_dir, "improvement_by_gender.csv"))

barrier_demographics <- df_barriers %>%
  select(
    ResponseId, Gender, Race_Ethnicity, AI_Experience_num, Planning_Knowledge_num,
    starts_with("Barrier_")
  )

barrier_by_gender <- barrier_demographics %>%
  group_by(Gender) %>%
  summarise(
    n = n(),
    Time = sum(Barrier_Time, na.rm = TRUE) / n() * 100,
    Knowledge = sum(Barrier_Knowledge, na.rm = TRUE) / n() * 100,
    Skepticism = sum(Barrier_Skepticism, na.rm = TRUE) / n() * 100,
    Interest = sum(Barrier_Interest, na.rm = TRUE) / n() * 100,
    Location = sum(Barrier_Location, na.rm = TRUE) / n() * 100,
    Process = sum(Barrier_Process, na.rm = TRUE) / n() * 100,
    Technical = sum(Barrier_Technical, na.rm = TRUE) / n() * 100
  )
write_excel_csv(barrier_by_gender, paste0(model_dir, "barrier_by_gender.csv"))

# ----------------------
# 6. Relationship Between AI Experience and Suggested Improvements
# ----------------------
ai_experience_correlation <- df_ai_improvements %>%
  select(AI_Experience_num, starts_with("Improvement_")) %>%
  cor(use = "pairwise.complete.obs") %>%
  as.data.frame() %>%
  rownames_to_column(var = "variable1") %>%
  filter(variable1 == "AI_Experience_num") %>%
  select(-variable1) %>%
  pivot_longer(
    cols = everything(),
    names_to = "improvement",
    values_to = "correlation"
  ) %>%
  mutate(
    improvement = gsub("Improvement_", "", improvement),
    abs_correlation = abs(correlation)
  ) %>%
  arrange(desc(abs_correlation))
write_excel_csv(ai_experience_correlation, paste0(model_dir, "ai_experience_correlation.csv"))

p3 <- ggplot(ai_experience_correlation, aes(x = reorder(improvement, abs_correlation), y = correlation, fill = correlation > 0)) +
  geom_col(color = "black", alpha = 0.85) +
  coord_flip() +
  scale_fill_manual(values = c("TRUE" = viridis(5)[4], "FALSE" = viridis(5)[2]), 
                    labels = c("TRUE" = "Positive", "FALSE" = "Negative"),
                    name = "Correlation") +
  labs(
    title = "Correlation Between AI Experience and Suggested Improvements",
    x = "Improvement Type",
    y = "Correlation Coefficient"
  ) +
  theme_beautiful() +
  theme(legend.position = "bottom")
ggsave(paste0(fig_dir, "ai_experience_correlation.png"), p3, width = 8, height = 6)

# ----------------------
# 7. Relationship Between Selected Improvements and Barriers
# ----------------------
top_improvements <- names(sort(colSums(df_ai_improvements %>% select(starts_with("Improvement_"))), decreasing = TRUE))[1:3]
top_barriers <- names(sort(colSums(df_barriers %>% select(starts_with("Barrier_"))), decreasing = TRUE))[1:3]

improvement_barrier_tests <- expand.grid(
  improvement = top_improvements,
  barrier = top_barriers,
  stringsAsFactors = FALSE
) %>%
  rowwise() %>%
  mutate(
    chi_test = list({
      tab <- table(
        df_ai_improvements[[improvement]],
        df_barriers[[barrier]]
      )
      if(all(dim(tab) > 0)) {
        chisq.test(tab, correct = FALSE)
      } else {
        NULL
      }
    }),
    p_value = if(!is.null(chi_test)) chi_test$p.value else NA,
    significant = !is.na(p_value) && p_value < 0.05
  ) %>%
  select(-chi_test) %>%
  arrange(p_value)
write_excel_csv(improvement_barrier_tests %>% select(-significant), paste0(model_dir, "improvement_barrier_tests.csv"))

# ----------------------
# 8. End of Script
# ----------------------
cat("Analysis of future suggestions and participation barriers completed.\n")
cat("Generated plots and data files have been saved to", fig_dir, "and", model_dir, "\n")

# Save results
future_suggestion_results <- list(
  improvement_data = improvement_data,
  barrier_data = barrier_data
)
write_excel_csv(improvement_data, paste0(model_dir, "ai_improvement_suggestions.xlsx"))
write_excel_csv(barrier_data, paste0(model_dir, "participation_barriers.xlsx"))