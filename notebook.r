# Load libraries
library(dplyr)
library(readr)
library(tidytext)
library(sweary)

# Load datasets
sp_lines <- read_csv("datasets/sp_lines.csv")
sp_ratings <- read_csv("datasets/sp_ratings.csv")

# Take a look at the last six observations
tail(sp_lines)
tail(sp_ratings)

# Load english swear words
en_swear_words <- sweary::get_swearwords("en") %>%
  mutate(stem = SnowballC::wordStem(word))

# Load the AFINN lexicon
afinn  <- read_rds("datasets/afinn.rds")

# Join lines with episode ratings
sp <- inner_join(sp_lines, sp_ratings)

# Unnest lines to words, leave out stop words and add a 
# swear_word logical column
sp_words <- sp %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words) %>%
  left_join(afinn) %>%
  mutate(word_stem = SnowballC::wordStem(word),
         swear_word = word %in% en_swear_words$word | word_stem %in% en_swear_words$stem)

# View the last six observations
tail(sp_words)
# Group by and summarize data by episode
by_episode <- sp_words %>%
  group_by(episode_name, rating, episode_order) %>%
  summarize(
    swear_word_ratio = sum(swear_word) / n(),
    sentiment_score = mean(value, na.rm = TRUE)
  ) %>%
  arrange(episode_order)

# Examine the last few rows of by_episode
tail(by_episode)

# What is the naughtiest episode?
( naughtiest <- by_episode[which.max(by_episode$swear_word_ratio), ] )
# Load the ggplot2
library(ggplot2)

# Set a minimal theme for all future plots
theme_set(theme_minimal())

# Plot sentiment score for each episode
ggplot(by_episode, aes(episode_order, sentiment_score)) +
  geom_col() +
  geom_smooth()
# Plot episode ratings
ggplot(by_episode, aes(episode_order, rating)) +
  geom_point() +
  geom_smooth() +
  geom_vline(xintercept = 100, col = "red", lty = "dashed")

# Plot swear word ratio over episode rating
ggplot(by_episode, aes(rating, swear_word_ratio)) +
  geom_point(alpha = 0.6, size = 3) +
  geom_smooth() +
  scale_y_continuous(labels = scales::percent) +
  scale_x_continuous(breaks = seq(6, 10, 0.5)) +
  labs(
    x = "IMDB rating",
    y = "Episode swear word ratio"
  )