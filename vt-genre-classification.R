library(tidyverse)

classified <- read_csv("./data/combinedGenres.csv") %>%
  distinct(doc_id, .keep_all = TRUE)

unclassified <- read_csv("./data/part-00000-b4557c14-339a-4d3b-8422-05d1babf9abc-c000.csv") %>%
  select(cluster, text) %>%
  mutate(cluster = as.character(cluster)) %>%
  rename(doc_id = cluster) %>%
  distinct(doc_id, .keep_all = TRUE) %>%
  mutate(genre = "unknown")

genres <- unique(classified$genre)

#tokenizing

tidy_clusters <- classified %>%
  unnest_tokens(word, text) %>%
  group_by(word) %>%
  filter(n() > 5) %>%
  ungroup()

tidy_clusters %>%
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
    title = "Most frequent words by genre after removing stop words"
  )

library(rsample)


# building the model
# first split classified data into training and testing sets

test_data <- classified %>%
  group_by(genre) %>%
  slice_sample(n = 10, replace = FALSE) %>%
  mutate(genre = "unknown") %>%
  ungroup()
train_data <- anti_join(classified, test_data, by = "doc_id")

sparse_words <- tidy_clusters %>%
  count(doc_id, word) %>%
  inner_join(train_data) %>%
  cast_sparse(doc_id, word, n)
dim(sparse_words)

word_rownames <- rownames(sparse_words)
class_joined <- tibble(doc_id = word_rownames) %>%
  left_join(classified %>%
              select(doc_id, genre))

# class_joined <- rbind(test_data, train_data)

selectGenre <- genres[4]

library(glmnet)
library(doMC)
registerDoMC(cores = 8)
genreClusters <- class_joined$genre == selectGenre
model <- cv.glmnet(sparse_words, genreClusters,
                   family = "binomial",
                   parallel = TRUE, keep = TRUE
)  

plot(model)
plot(model$glmnet.fit)

library(broom)
coefs <- model$glmnet.fit %>%
  tidy() %>%
  filter(lambda == model$lambda.1se)

coefs %>%
  group_by(estimate > 0) %>%
  top_n(10, abs(estimate)) %>%
  ungroup() %>%
  ggplot(aes(fct_reorder(term, estimate), estimate, fill = estimate > 0)) +
  geom_col(alpha = 0.8, show.legend = FALSE) +
  coord_flip() +
  labs(
    x = NULL,
    title = str_to_title(paste0("Coefficients that increase/decrease probability of ", selectGenre, " the most"))
  )

intercept <- coefs %>%
  filter(term == "(Intercept)") %>%
  pull(estimate)
classifications <- tidy_clusters %>%
  inner_join(test_data, by = "doc_id") %>%
  inner_join(coefs, by = c("word" = "term")) %>%
  group_by(doc_id) %>%
  summarize(score = sum(estimate)) %>%
  mutate(probability = plogis(intercept + score)) %>%
  rename(!!paste0(selectGenre, "_score") := score, !!paste0(selectGenre, "_probability") := probability)
View(classifications)

# build a function to create training data in multiple genres
library(broom)
library(stringr)
library(glmnet)
library(doMC)
registerDoMC(cores = 8)

trainGenres <- function(data1, data2){
  f <- unique(data1$genre)
  df <- rbind(data1, data2)
  df_combined <- tibble(df %>%
                          select(doc_id))
  tidy_df <- df %>%
    unnest_tokens(word, text) %>%
    group_by(word) %>%
    # filter(n() > 5) %>%
    ungroup()
  sparse_words <- tidy_df %>%
    count(doc_id, word) %>%
    cast_sparse(doc_id, word, n)
  word_rownames <- rownames(sparse_words)
  class_joined <- tibble(doc_id = word_rownames) %>%
    left_join(df %>%
                select(doc_id, genre))
  for (i in f){
    df1 <- class_joined$genre == i
    model <- cv.glmnet(sparse_words, df1,
                       family = "binomial",
                       parallel = TRUE, keep = TRUE
    )
    coefs <- model$glmnet.fit %>%
      tidy() %>%
      filter(lambda == model$lambda.1se)
    intercept <- coefs %>%
      filter(term == "(Intercept)") %>%
      pull(estimate)
    df2 <- tidy_df %>%
      inner_join(coefs, by = c("word" = "term")) %>%
      group_by(doc_id) %>%
      summarize(score = sum(estimate)) %>%
      mutate(probability = plogis(intercept + score)) %>%
      rename(!!paste0(as.character(i), "_score") := score, !!paste0(as.character(i), "_probability") := probability)
    df_combined <- full_join(df_combined, df2, by="doc_id")
  }
  df_combined <- left_join(df, df_combined, by="doc_id") %>%
    rename(taggedGenre = genre)
  return(df_combined)
}

trainedGenres <- trainGenres(classified, unclassified)

trainedGenres %>%
  filter(taggedGenre == "unknown") %>%
  mutate(probSums = rowSums(.[,c(5,7,9,11,13,15)],na.rm = TRUE)) %>%
  arrange(desc(probSums)) %>%
  View()

         