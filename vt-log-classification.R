library(tidyverse)
library(tidytext)
library(stringr)
library(caret)
library(tm)
library(readtext)

  
texts <- read_csv("https://github.com/ViralTexts/viral-texts-classification/blob/workshop/data/combinedGenres.csv?raw=true") %>%
  mutate(doc_id = row_number()) %>%
  select(doc_id, text, genre)

texts %>%
  distinct(genre)

tidy_texts <- texts %>%
  unnest_tokens(word, text) %>%
  group_by(word) %>%
  filter(n() > 10) %>%
  ungroup()

tidy_texts %>%
  count(genre, word, sort = TRUE) %>%
  anti_join(get_stopwords()) %>%
  group_by(genre) %>%
  top_n(20) %>%
  ungroup() %>%
  ggplot(aes(reorder_within(word, n, genre), n,
             fill = genre
  )) +
  geom_col(alpha = 0.8, show.legend = FALSE) +
  scale_x_reordered() +
  coord_flip() +
  facet_wrap(~genre, scales = "free") +
  scale_y_continuous(expand = c(0, 0)) +
  labs(
    x = NULL, y = "Word count",
    title = "Most frequent words after removing stop words",
    subtitle = "Words like 'said' occupy similar ranks but other words are quite different"
  )

texts_split <- texts %>%
  select(doc_id) %>%
  initial_split()
train_data <- training(texts_split)
test_data <- testing(texts_split)

sparse_words <- tidy_texts %>%
  count(doc_id, word) %>%
  inner_join(train_data) %>%
  cast_sparse(doc_id, word, n)

word_rownames <- as.integer(rownames(sparse_words))

texts_joined <- data_frame(doc_id = word_rownames) %>%
  left_join(texts %>%
              select(doc_id, genre))

library(glmnet)
library(doMC)
registerDoMC(cores = 8)
is_recipe <- texts_joined$genre == "recipe"
model <- cv.glmnet(sparse_words, is_recipe,
                   family = "binomial",
                   parallel = TRUE, keep = TRUE
)
