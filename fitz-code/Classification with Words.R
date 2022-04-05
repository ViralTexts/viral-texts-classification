#This assumes topic modelling has been completed in topicmodellingGenres.rmd

#The input for classification is the topic modeled data frame from above.

#Create a dataframe with all topics for clusters that have known genres
topicsDF = td %>% 
  filter(genre!="unknown")

#Create a dataframe for all topics for clusters with unknown genres
topics_unknown = td %>% 
  mutate(primary_genre = genre) %>% 
  filter(primary_genre =="unknown")

#Convert to a matrix
modeling_matrix = topicsDF %>% select(-genre, -cluster)
#modeling_matrix = log(modeling_matrix)

#Create a matrix for all topics for clusters with unknown genres
unclassified_data = td %>% 
  mutate(primary_genre = genre) %>% 
  filter(primary_genre=="unknown") %>% 
  select(-primary_genre,-cluster,-genre)

#Create a training set
should_be_training = sample(c(TRUE,FALSE),nrow(modeling_matrix),replace=T,prob = c(.75,.25))

#Convert training set into dataframe
training_frame = data.frame(modeling_matrix[should_be_training,])
training_frame$match = NA

#Build a model using GLM
build_model = function(genre,model_function=glm,...) {
  # genre is a string indicating one of the primary_genre fields;
  # model function is something like "glm" or "svm";
  # are further arguments passed to that function.
  training_frame$match=as.numeric(topicsDF$genre == genre)[should_be_training]
  # we model against a matrix: the columns are the topics, which we get by dropping out the other four elements
  match_ratio = sum(as.numeric(training_frame$match))/length(training_frame$match)
  model = model_function(match ~ ., training_frame,...,weights = ifelse(match,1/match_ratio,1/(1-match_ratio)))
}

#Visualize top ten (filter_to_top) genres
filter_to_top = 4
topicsDF %>% 
  filter(should_be_training) %>% 
  group_by(genre) %>% 
  summarize(cluster=n()) %>% 
  mutate(rank=rank(-cluster)) %>% 
  arrange(rank) %>% 
  ggplot() + 
  geom_bar(aes(y=cluster,x=reorder(genre,cluster),fill=genre),stat="identity") + 
  coord_flip() + 
  labs(title="Most common genres, by number of clusters in training set")

#Create a value of the top genres. 
top_genres = topicsDF %>% group_by(genre) %>% summarize(cluster=n()) %>% mutate(rank=rank(-cluster)) %>% arrange(rank) %>% slice(1:filter_to_top) %>% select(genre) %>% unlist

top_genres

#Create models
models = lapply(top_genres,build_model,glm,family=quasibinomial,maxit = 100)

# Here's where we predict on out-of-model data.
predictions = lapply(models,predict,newdata = data.frame(modeling_matrix[!should_be_training,]),type="response")

# Convert to dataframe with scores for each genre  
predictions_frame = do.call(cbind,predictions) %>% as.data.frame()
names(predictions_frame) = top_genres

# Add cluster number and primary genre
predictions_frame = cbind(topicsDF %>% select(cluster,genre) %>% filter(!should_be_training),predictions_frame)

# Tidied data frame
tidied = predictions_frame %>% gather("classified_genre","probability",-genre,-cluster)



# Create a data frame with top probability for each cluster
best_guesses = tidied %>% group_by(cluster) %>% 
  arrange(-probability) %>% slice(1) %>% # (Only take the top probability for each cluster)
  mutate(actual_genre=genre)

confusion = best_guesses %>% group_by(actual_genre,classified_genre) %>% summarize(`count`=n())
ggplot(confusion) + geom_point(aes(y=classified_genre,x=count)) + facet_wrap(~actual_genre)
ggplot(confusion) + geom_histogram(stat="identity") + aes(x=actual_genre,y=count,fill=classified_genre) + coord_flip()


confusion %>% 
  group_by(actual_genre) %>% 
  summarize(percent_right = 100 * sum(count[actual_genre==classified_genre])/sum(count)) %>% 
  arrange(-percent_right)

confusion %>% 
  group_by(1) %>% 
  summarize(percent_right = 100 * sum(count[actual_genre==classified_genre])/sum(count)) %>% 
  arrange(-percent_right)


# ---- #

best_guesses %>% 
  group_by(classified_genre) %>% 
  summarize(cluster=n()) %>% 
  mutate(rank=rank(-cluster)) %>% 
  arrange(rank) %>% 
  ggplot() + 
  geom_bar(aes(y=cluster,x=reorder(classified_genre,cluster),fill=classified_genre),stat="identity") + 
  coord_flip() + 
  labs(title="Most common guessed genres, by number of clusters")


#How is the classifier performing on topics?

top_predictors = lapply(1:length(top_genres),function(n,return_length=15) {
  comedy_model = models[n][[1]]
  using = (rank((comedy_model$coefficients))<=(return_length/2)) | (rank(-comedy_model$coefficients)<=(return_length/2))
  coefficients = data.frame(genre = top_genres[n],topic=names(comedy_model$coefficients[using]) %>% gsub("modeling_matrix","",.),strength = comedy_model$coefficients[using],row.names = NULL)
  coefficients
}) %>% rbind_all

ggplot(top_predictors %>% filter(topic!="(Intercept)")) + geom_point(aes(x=strength,y=topic,color=strength>0)) + facet_wrap(~genre,scales="free",ncol=3)


# Here's where we predict on out-of-model data.
# Work on this, still nto working quite right

out_of_domain_predictions = lapply(models,predict,newdata = data.frame(unclassified_data),type="response")


out_of_domain_predictions_frame = do.call(cbind,out_of_domain_predictions) %>% as.data.frame()
names(out_of_domain_predictions_frame) = top_genres

out_of_domain_predictions_frame = cbind(topics_unknown %>% select(cluster,primary_genre),out_of_domain_predictions_frame)

out_of_domain_predictions_tidied = out_of_domain_predictions_frame %>% gather("classified_genre","probability",-primary_genre,-cluster)

out_of_domain_predictions_best_guesses = out_of_domain_predictions_tidied %>% group_by(cluster) %>% 
  arrange(-probability) %>% slice(1) %>% # (Only take the top probability for each episode)
  mutate(actual_genre=primary_genre)

genreClassWords = out_of_domain_predictions_best_guesses %>% mutate(Cluster = cluster) %>% left_join(clustersForTM) 
genreClassWords = genreClassWords[,c("cluster","classified_genre","probability","Text")]
write.csv(genreClassWords, file = paste('output/SentimentalClassWords-12-3-16.csv',sep=""))

#visualize best guesses of unknown

out_of_domain_predictions_best_guesses %>% 
  group_by(classified_genre) %>% 
  summarize(cluster=n()) %>% 
  mutate(rank=rank(-cluster)) %>% 
  arrange(rank) %>% 
  ggplot() + 
  geom_bar(aes(y=cluster,x=reorder(classified_genre,cluster),fill=classified_genre),stat="identity") + 
  coord_flip() + 
  labs(title="Most common guessed genres, by number of clusters")

out_of_domain_predictions_best_guesses %>% 
  group_by(classified_genre) %>% 
  summarize(cluster=n()) %>% 
  mutate(percent=(cluster/nrow(out_of_domain_predictions_best_guesses))*100) %>% 
  arrange(-percent)



confusion %>% group_by(actual_genre) %>% summarize(percent_right = 100 * sum(count[actual_genre==classified_genre])/sum(count)) %>% arrange(-percent_right)

confusion %>% group_by(1) %>% summarize(percent_right = 100 * sum(count[actual_genre==classified_genre])/sum(count)) %>% arrange(-percent_right)




genreClass = genreClass %>% filter(probability>.75)
write.csv(genreClass, file = paste('output/genreClass-5-15-16.csv',sep=""))

write.csv(genreClass, file = paste('output/vignettesVfictionVnews-7-25-16.csv',sep=""))





#just poems
justPoems = genreClass %>% filter(classified_genre=="poetry")
justPoems = justPoems %>% filter(probability>.97)
justPoems = justPoems[,c("cluster","classified_genre")]
justPoems = justPoems %>% inner_join(newDataAll)
write.csv(justPoems, file = paste('output/justPoems-3-21-16.csv',sep=""))


#just political
justPolitical = genreClass %>% filter(classified_genre=="political")
justPolitical = justPolitical %>% filter(probability>.97)
justPolitical = justPolitical[,c("cluster","classified_genre")]
justPolitical = justPolitical %>% inner_join(newDataAll)
write.csv(justPolitical, file = paste('output/justPolitical-3-21-16.csv',sep=""))


#just prose
justProse = genreClass %>% filter(classified_genre=="prose")
justProse = justProse[,c("Cluster","Genre","Text")]
names(justProse)[names(justProse)=="Cluster"] <- "cluster"
names(justProse)[names(justProse)=="Genre"] <- "genre"
names(justProse)[names(justProse)=="Text"] <- "text"


#just ads
justAds = genreClass %>% filter(classified_genre=="advertisement")
justAds = justAds[,c("Cluster","classified_genre","Text")]
names(justAds)[names(justAds)=="Cluster"] <- "cluster"
names(justAds)[names(justAds)=="classified_genre"] <- "genre"
names(justAds)[names(justAds)=="Text"] <- "text"
