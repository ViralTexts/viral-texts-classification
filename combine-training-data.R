# Load all the individual datasets

ads <- read_csv("./hand-tagged/ads.csv") %>%
  filter(correct == "yes") %>%
  mutate(doc_id = cluster) %>%
  select(doc_id, text, genre)
info <- read_csv("./hand-tagged/informational.csv") %>%
  filter(correct == "yes") %>%
  rename(doc_id = cluster) %>%
  select(doc_id, text, genre)
lit <- read_csv("./hand-tagged/literary.csv") %>%
  filter(correct == "yes") %>%
  rename(doc_id = cluster) %>%
  select(doc_id, text, genre)
news <- read_csv("./hand-tagged/news.csv") %>%
  rename(doc_id = cluster) %>%
  select(doc_id, text, genre)
poetry <- read_csv("./hand-tagged/poetry.csv") %>%
  rename(doc_id = cluster) %>%
  select(doc_id, text, genre)
recipes <- read_csv("hand-tagged/recipes.csv") %>%
  sample_n(200)
  
trainingData <- rbind(ads, info, lit, news, poetry, recipes)


test_data <- read_csv("./data/part-00000-e9a62088-0439-47d6-af79-3f482c36e0f7-c000.csv") %>%
  select(cluster, text) %>%
  mutate(cluster = as.character(cluster)) %>%
  rename(doc_id = cluster) %>%
  distinct(doc_id, .keep_all = TRUE) %>%
  mutate(genre = "unknown")
train_data <- readtext(files) %>%
  mutate(genre = "recipe")