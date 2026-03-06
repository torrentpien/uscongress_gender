# ==================================================
# U.S. House 115th (Gender Project) - Beginner Template
# ==================================================
# Goal:
# 1) Explore table structure and variable distributions
# 2) Clean key variables
# 3) Build gender-related visualizations

# ---- 0. Load packages ----
# Install once if needed:
# install.packages(c("tidyverse", "janitor", "skimr"))

library(tidyverse)
library(janitor)
library(skimr)

# ---- 1. Read data ----
# Put house_115_raw.csv in your working directory.
# You can also replace this path with your full file path.
raw <- read_csv("house_115_raw.csv", show_col_types = FALSE)

# ---- 2. Table exploration ----
# 2-1) Basic structure
names(raw)            # column names
str(raw)              # variable types
glimpse(raw)          # quick overview

dim(raw)              # rows and columns
head(raw, 10)         # first 10 rows

# 2-2) Missing values by column
colSums(is.na(raw))

# 2-3) Quick summary (numeric + character)
skim(raw)

# 2-4) Frequency check for likely categorical variables
# NOTE: Change these names after checking your real column names.
# Example candidates: gender, party, state, district, race_ethnicity
cat_candidates <- c("gender", "party", "state", "district", "incumbent")
cat_candidates <- cat_candidates[cat_candidates %in% names(raw)]

for (v in cat_candidates) {
  cat("\n=====", v, "=====\n")
  print(raw %>% count(.data[[v]], sort = TRUE))
}

# ---- 3. Variable cleaning ----
# Why cleaning is needed (common problems):
# - Upper/lower case inconsistency ("F", "female", "Female")
# - Extra spaces (" Democrat ")
# - Different labels for same concept ("Rep", "Republican")
# - Text columns that should be numeric (e.g., age, district)
# - Missing values represented as "", "NA", "Unknown", "-"

clean <- raw %>%
  clean_names() %>%                                  # make column names snake_case
  mutate(across(where(is.character), str_trim)) %>%  # remove leading/trailing spaces
  mutate(across(where(is.character), ~ na_if(.x, ""))) %>%
  mutate(across(where(is.character), ~ na_if(.x, "Unknown"))) %>%
  mutate(across(where(is.character), ~ na_if(.x, "-")))

# Example standardization (only runs if columns exist)
if ("gender" %in% names(clean)) {
  clean <- clean %>%
    mutate(gender = str_to_title(gender),
           gender = recode(gender,
                           "F" = "Female",
                           "M" = "Male",
                           "Woman" = "Female",
                           "Man" = "Male"))
}

if ("party" %in% names(clean)) {
  clean <- clean %>%
    mutate(party = str_to_title(party),
           party = recode(party,
                          "Dem" = "Democrat",
                          "Democratic" = "Democrat",
                          "Rep" = "Republican",
                          "Gop" = "Republican"))
}

# Optional: convert likely numeric columns if needed
# (edit column names after checking your data)
num_candidates <- c("age", "district", "vote_share")
num_candidates <- num_candidates[num_candidates %in% names(clean)]
clean <- clean %>%
  mutate(across(all_of(num_candidates), readr::parse_number))

# Check cleaned data
glimpse(clean)

# ---- 4. Visualization suggestions + data reshaping ----
# (A) Gender composition overall
if ("gender" %in% names(clean)) {
  p1 <- clean %>%
    count(gender) %>%
    ggplot(aes(x = gender, y = n, fill = gender)) +
    geom_col() +
    labs(title = "Gender Composition in the 115th U.S. House",
         x = "Gender", y = "Number of Members") +
    theme_minimal()
  print(p1)
}

# (B) Gender by party (stacked and fill)
if (all(c("gender", "party") %in% names(clean))) {
  p2 <- clean %>%
    count(party, gender) %>%
    ggplot(aes(x = party, y = n, fill = gender)) +
    geom_col(position = "stack") +
    labs(title = "Gender Counts by Party", x = "Party", y = "Count") +
    theme_minimal()
  print(p2)

  p3 <- clean %>%
    count(party, gender) %>%
    ggplot(aes(x = party, y = n, fill = gender)) +
    geom_col(position = "fill") +
    scale_y_continuous(labels = scales::percent) +
    labs(title = "Gender Share by Party", x = "Party", y = "Share") +
    theme_minimal()
  print(p3)
}

# (C) Gender by state (top 15 states by female members)
if (all(c("gender", "state") %in% names(clean))) {
  p4_data <- clean %>%
    filter(gender == "Female") %>%
    count(state, sort = TRUE) %>%
    slice_head(n = 15)

  p4 <- p4_data %>%
    ggplot(aes(x = reorder(state, n), y = n)) +
    geom_col(fill = "#D95F02") +
    coord_flip() +
    labs(title = "Top 15 States by Number of Female House Members",
         x = "State", y = "Female Members") +
    theme_minimal()
  print(p4)
}

# ---- 5. Example of reshaping for plotting ----
# Wide -> long data is useful for grouped/faceted charts.
# Example: if your data has columns like votes_female, votes_male.
# pivot_longer turns them into: gender_type + votes.
if (all(c("votes_female", "votes_male") %in% names(clean))) {
  votes_long <- clean %>%
    pivot_longer(cols = c(votes_female, votes_male),
                 names_to = "gender_type",
                 values_to = "votes")

  print(head(votes_long))
}

# Another reshape example from grouped summary (already long-friendly):
if (all(c("party", "gender") %in% names(clean))) {
  party_gender_long <- clean %>%
    count(party, gender, name = "members")

  print(party_gender_long)
}

# ---- 6. Save cleaned data ----
write_csv(clean, "house_115_clean.csv")

# End of script
