library(mallet)
library(tidyverse)

# import clusters for reference
classified <- read_csv("./data/combinedGenres.csv") %>%
  distinct(doc_id, .keep_all = TRUE)
unclassified <- read_csv("./data/part-00000-b4557c14-339a-4d3b-8422-05d1babf9abc-c000.csv") %>%
  select(cluster, text) %>%
  mutate(cluster = as.character(cluster)) %>%
  rename(doc_id = cluster) %>%
  distinct(doc_id, .keep_all = TRUE) %>%
  mutate(genre = "unknown")

# Import a dataframe for all topics for clusters with known genres
topics_known <- readRDS("data/known_topics.RData")


# Import a dataframe for all topics for clusters with unknown genres
topics_unknown <- readRDS("data/unknown_topics.RData")


# Create a matrix for all topics for clusters with known genres
modeling_matrix <- topics_known %>% 
  select(-primary_genre, -doc_id)
modeling_matrix <- log(modeling_matrix)
dim(modeling_matrix)

# Create a matrix for all topics for clusters with unknown genres
unknown_matrix <- topics_unknown %>% 
  select(-primary_genre, -doc_id)
unknown_matrix <- log(unknown_matrix)
dim(unknown_matrix)

#Create a training set
should_be_training <- sample(c(TRUE,FALSE),
                             nrow(modeling_matrix),
                             replace=T,
                             prob = c(.75,.25))

#Convert training set into dataframe
training_frame <- modeling_matrix[should_be_training,] %>%
  as_tibble()


# Write a function that will build a model using GLM
build_model <- function(genre,model_function=glm,...) {
  # genre is a string indicating one of the primary_genre fields;
  # model function is something like "glm" or "svm";
  # are further arguments passed to that function.
  training_frame$match=as.numeric(topics_known$primary_genre == genre)[should_be_training]
  # we model against a matrix: the columns are the topics, which we get by dropping out the other four elements
  match_ratio = sum(as.numeric(training_frame$match))/length(training_frame$match)
  model = model_function(match ~ ., training_frame,...,weights = ifelse(match,1/match_ratio,1/(1-match_ratio)))
}

#Visualize relative sizes of genres in data; optional filter
# filter_to_top = 4
topics_known %>% 
  filter(should_be_training) %>% 
  group_by(primary_genre) %>% 
  summarize(doc_id=n()) %>% 
  mutate(rank=rank(-doc_id)) %>% 
  arrange(rank) %>% 
  ggplot() + 
  geom_bar(aes(y=doc_id,x=reorder(primary_genre,doc_id),fill=primary_genre),stat="identity") + 
  coord_flip() + 
  labs(title="Most common genres, by number of clusters in training set")

#Create a value of the genres; optional filter 
genres <- topics_known %>% 
  group_by(primary_genre) %>% 
  summarize(doc_id=n()) %>% 
  mutate(rank=rank(-doc_id)) %>% 
  arrange(rank) %>% 
  # slice(1:filter_to_top) %>% 
  select(primary_genre) %>% 
  unlist

genres

#Create models (models = lapply(top_genres,build_model,glm,family=quasibinomial,maxit = 100))
models <- lapply(genres,
                 build_model,
                 glm,
                 family=quasibinomial,
                 maxit = 100)

# Regularization, look for other packages that might regularize the parameters. https://cran.r-project.org/web/packages/glmnet/glmnet.pdf
# Here's where we predict on out-of-model data. (Saved a model "model_87" which gives really good results. What's that about?)
predictions <- lapply(models,
                      predict,
                      newdata = as_tibble(modeling_matrix[!should_be_training,]),
                      type="response")

# Convert to dataframe with scores for each genre  
predictions_frame <- do.call(cbind,predictions) %>% 
  as_tibble()
names(predictions_frame) <- genres

# Add cluster number and primary genre
predictions_frame <- cbind(topics_known %>% 
                             select(doc_id, primary_genre) %>% 
                             filter(!should_be_training), predictions_frame)

#set.seed(1)
#predictions_frame %>% sample_n(6)

# Tidied data frame
tidied_predict <- predictions_frame %>% 
  gather("classified_genre","probability",-primary_genre,-doc_id)

# Create a data frame with top probability for each cluster
best_guesses <- tidied_predict %>% 
  group_by(doc_id) %>% 
  arrange(-probability) %>% 
  slice(1) %>% # (Only take the top probability for each cluster)
  rename(actual_genre = primary_genre)

confusion <- best_guesses %>% 
  group_by(actual_genre, classified_genre) %>% 
  summarize(`count`=n())
ggplot(confusion) + 
  geom_point(aes(y=classified_genre,x=count)) + 
  facet_wrap(~actual_genre)
# ggplot(confusion) + 
#   geom_bar(stat="identity") + 
#   aes(x=actual_genre,y=count,fill=classified_genre) + 
#   coord_flip() + 
#   theme(plot.title = element_text(family = "Helvetica", color="#666666", face="bold", size=22, hjust=0)) +
#   theme(axis.title = element_text(family = "Helvetica", color="#666666", face="bold", size=18)) + 
#   theme(legend.title = element_text(family = "Helvetica", color="#666666", face="bold", size=18)) +
#   theme(axis.text = element_text(family = "Helvetica", color="#333333", face="bold", size=16))

confusion %>% 
  group_by(actual_genre) %>% 
  summarize(percent_right = 100 * sum(count[actual_genre==classified_genre])/sum(count)) %>% 
  arrange(-percent_right)

confusion %>% 
  group_by(1) %>% 
  summarize(percent_right = 100 * sum(count[actual_genre==classified_genre])/sum(count)) %>% 
  arrange(-percent_right)

#END TEST DATA

# #Create dataframe of averages per genre (first run, remove _column)
# averages_column <- confusion %>% 
#   group_by(actual_genre) %>% 
#   summarize(percent_right = 100 * sum(count[actual_genre==classified_genre])/sum(count)) %>% 
#   arrange(-percent_right)
# 
# #Create dataframe of total averages (first run, remove _column)
# total_averages_column <- confusion %>% 
#   group_by(1) %>% 
#   summarize(percent_right = 100 * sum(count[actual_genre==classified_genre])/sum(count)) %>% 
#   arrange(-percent_right)
# 
# #Merge columns into dataframes
# averages <- merge(averages,
#                   averages_column,
#                   by="actual_genre")
# total_averages <- merge(total_averages,
#                         total_averages_column,
#                         by="1")
# 
# #Name columns
# names(averages)[2:11]<-paste("percent_right_",1:10,sep = "")
# names(total_averages)[2:11]<-paste("percent_right_",1:10,sep = "")
# 
# #Compute averages
# averages <- averages %>%
#   mutate("average" = rowMeans(averages[2:11]))
# total_averages <- total_averages %>% 
#   mutate("average" = rowMeans(total_averages[2:11]))
# 
# #Bar plot on averages
# ggplot(averages) + geom_bar(stat="identity") + aes(x=actual_genre,y=average,fill=actual_genre) + coord_flip()
# 



best_guesses %>% 
  group_by(classified_genre) %>% 
  summarize(doc_id=n()) %>% 
  mutate(rank=rank(-doc_id)) %>% 
  arrange(rank) %>% 
  ggplot() + 
  geom_bar(aes(y=doc_id,x=reorder(classified_genre,doc_id),fill=classified_genre),stat="identity") + 
  coord_flip() + 
  labs(title="Most common guessed genres, by number of clusters")


#How is the classifier performing on topics?

top_predictors <- lapply(1:length(genres),function(n,return_length=50) {
  comedy_model = models[n][[1]]
  using = (rank((comedy_model$coefficients))<=(return_length/2)) | (rank(-comedy_model$coefficients)<=(return_length/2))
  coefficients = data.frame(genre = genres[n],topic=names(comedy_model$coefficients[using]) %>% gsub("modeling_matrix","",.),strength = comedy_model$coefficients[using],row.names = NULL)
  coefficients
}) %>% 
  bind_rows()

ggplot(top_predictors %>% 
         filter(topic!="(Intercept)")) + 
  geom_point(aes(x=strength,y=topic,color=strength>0)) + 
  facet_wrap(~genre,scales="fixed",ncol=2)
ggplot(top_predictors %>% 
         filter(topic!="(Intercept)", strength>0)) + 
  geom_bar(stat="identity") + 
  aes(x=topic,y=strength,fill=genre) + 
  coord_flip() + 
  theme(plot.title = element_text(family = "Helvetica", color="#666666", face="bold", size=12, hjust=0)) +
  theme(axis.title = element_text(family = "Helvetica", color="#666666", face="bold", size=12)) + 
  theme(legend.title = element_text(family = "Helvetica", color="#666666", face="bold", size=12)) +
  theme(axis.text = element_text(family = "Helvetica", color="#333333", face="bold", size=12))



# Here's where we predict on out-of-model data.

out_of_domain_predictions <- lapply(models,
                                    predict,
                                    newdata = as_tibble(unknown_matrix),
                                    type="response")
out_of_domain_predictions_frame <- do.call(cbind,
                                           out_of_domain_predictions) %>% 
  as_tibble()
names(out_of_domain_predictions_frame) <- genres
out_of_domain_predictions_frame <- cbind(topics_unknown %>% 
                                           select(doc_id, 
                                                  primary_genre),
                                         out_of_domain_predictions_frame)
out_of_domain_predictions_tidied <- out_of_domain_predictions_frame %>% 
  gather("classified_genre","probability",-primary_genre,-doc_id)
out_of_domain_predictions_best_guesses <- out_of_domain_predictions_tidied %>% 
  group_by(doc_id) %>% 
  arrange(-probability) %>% 
  slice(1) %>% # (Only take the top probability for each episode)
  mutate(actual_genre=primary_genre)
genreClass <- out_of_domain_predictions_best_guesses %>% 
  left_join(unclassified) %>%
  select(doc_id, actual_genre, classified_genre, text)

# genreClass$genre <- genreClass$classified_genre 
# genreClass = genreClass[,c("doc_id","genre","probability","text")]
# genreClass = genreClass %>% filter(probability>.8)
# genreClass = genreClass %>% filter(classified_genre=="literary")
# literaryClass <- genreClass %>% filter(genre=="literary")

#write.csv(genreClass, file = paste('output/genreClass-8-8-17.csv',sep=""))

# Remove dups and regularize column names
# genreClassDeDup <- genreClass[!duplicated(genreClass[,1]),]
# genreClassDeDup$genre <- genreClassDeDup$classified_genre

#visualize best guesses of unknown

out_of_domain_predictions_best_guesses %>% 
  group_by(classified_genre) %>% 
  summarize(doc_id=n()) %>% 
  mutate(rank=rank(-doc_id)) %>% 
  arrange(rank) %>% 
  ggplot() + 
  geom_bar(aes(y=doc_id,
               x=reorder(classified_genre, doc_id),
               fill=classified_genre),
           stat="identity") + 
  coord_flip() + 
  labs(title="Most commonly guessed genres, by number of clusters") # + 
#  theme(plot.title = element_text(family = "Helvetica", color="#666666", face="bold", size=12, hjust=0)) +
#  theme(axis.title = element_text(family = "Helvetica", color="#666666", face="bold", size=12)) + 
#  theme(legend.title = element_text(family = "Helvetica", color="#666666", face="bold", size=12)) +
#  theme(axis.text = element_text(family = "Helvetica", color="#333333", face="bold", size=12))

out_of_domain_predictions_best_guesses %>% 
  group_by(classified_genre) %>% 
  summarize(doc_id=n()) %>% 
  mutate(percent=(doc_id/nrow(out_of_domain_predictions_best_guesses))*100) %>% 
  arrange(-percent) 



#Rank classified genres in order of probability 
genreRank <- out_of_domain_predictions_tidied %>% 
  group_by(doc_id) %>% 
  arrange(-probability) %>% 
  mutate(genreOrder = paste("genre_",row_number(-probability),sep="")) 
genreRank$probability=paste(round(genreRank$probability*100,digits=2)) %>% as.numeric()


#Histogram for genreRank

sample_n_groups <- function(tbl, size, replace = FALSE, weight = NULL) {
  # regroup when done
  grps = tbl %>% 
    groups %>% 
    lapply(as.character) %>% 
    unlist
  # check length of groups non-zero
  keep = tbl %>% 
    summarise() %>% 
    ungroup() %>% 
    sample_n(size, replace, weight)
  # keep only selected groups, regroup because joins change count.
  # regrouping may be unnecessary but joins do something funky to grouping variable
  tbl %>% 
    right_join(keep, by=grps) %>% 
    group_by(.dots = grps)
}

genreRankTest <- genreRank %>% 
  group_by(doc_id) %>% 
  sample_n_groups(40) %>%
  left_join(genreClass %>%
              select(doc_id, text))

library(viridis)

genreRankTest %>% 
  group_by(doc_id) %>% 
  ggplot() + 
  geom_bar(stat="identity") + 
  aes(x=classified_genre,y=probability,fill=classified_genre) + 
  coord_flip() + 
  facet_wrap(~doc_id,ncol=4) +
  scale_fill_viridis_d()

plot <- genreRankTest %>%
  group_by(doc_id) %>% 
  ggplot() + 
  geom_histogram(stat="identity",position="fill") + 
  aes(x=doc_id,y=probability,fill=classified_genre,text=str_trunc(text, 100)) + 
  coord_flip() +
  scale_fill_viridis_d()

ggplotly(plot, 
         tooltip = c("text", "probability"))




best_guesses %>% 
  group_by(classified_genre) %>% 
  summarize(cluster=n()) %>% 
  mutate(rank=rank(-cluster)) %>% 
  arrange(rank) %>% 
  ggplot() + 
  geom_bar(aes(y=cluster,x=reorder(classified_genre,cluster),fill=classified_genre),stat="identity") + 
  coord_flip() + 
  labs(title="Most common guessed genres, by number of clusters")


