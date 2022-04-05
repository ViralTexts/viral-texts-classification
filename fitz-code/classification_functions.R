#Topic Modelling Function
topicModel = function(data.frameTM) {
  dataTM = allDataNoAds
  clustersForTM = dataTM[,c("cluster","text","genre")] %>% filter(nchar(as.character(text), allowNA=TRUE) > 50) 
  clustersForTM = data.frame(Cluster=as.character(dataTM$cluster),Text=as.character(dataTM$text),Genre=as.character(dataTM$genre),stringsAsFactors = F)
  input=clustersForTM
  n.topics=25
  mallet.instances <- mallet.import(input$Cluster, input$Text, stoplist.file="data/stopwords.txt", token.regexp = "\\w+",preserve.case=F)
  topic.model <- MalletLDA(num.topics=n.topics)
  topic.model$loadDocuments(mallet.instances)
  vocabulary <- topic.model$getVocabulary()
  word.freqs <- mallet.word.freqs(topic.model)
  topic.model$setAlphaOptimization(20, 50)
  topic.model$train(300)
  topic.model$maximize(10)
  doc.topics <- mallet.doc.topics(topic.model, smoothed=T, normalized=T)
  topic.docs <- t(doc.topics)
  topic.words <- mallet.topic.words(topic.model, smoothed=T, normalized=T)
  topics.labels <- rep("", n.topics)
  for (topic in 1:n.topics) {
    topics.labels[topic] <- paste(
      mallet.top.words(topic.model, topic.words[topic,], num.top.words=5)$words, collapse=" "
    )}
  rownames(doc.topics) = input$Cluster
  colnames(doc.topics) = topics.labels
  return(doc.topics)
}

#Classification on Topic Models function - Test Data (not working yet)



classification = function(data.frameClass) {
topicsDF = doc.topics %>% 
  as.data.frame() %>% 
  mutate(cluster = input$Cluster, primary_genre = input$Genre) %>% 
  filter(primary_genre!="unknown")
modeling_matrix = topicsDF %>% select(-primary_genre, -cluster)
modeling_matrix = log(modeling_matrix)
should_be_training = sample(c(TRUE,FALSE),nrow(modeling_matrix),replace=T,prob = c(.75,.25))
training_frame = data.frame(modeling_matrix[should_be_training,])
training_frame$match = NA
build_model = function(genre,model_function=glm,...) {
  # genre is a string indicating one of the primary_genre fields;
  # model function is something like "glm" or "svm";
  # are further arguments passed to that function.
  training_frame$match=as.numeric(topicsDF$primary_genre == genre)[should_be_training]
  # we model against a matrix: the columns are the topics, which we get by dropping out the other four elements
  match_ratio = sum(as.numeric(training_frame$match))/length(training_frame$match)
  model = model_function(match ~ ., training_frame,...,weights = ifelse(match,1/match_ratio,1/(1-match_ratio)))
}
filter_to_top = 8
top_genres = topicsDF %>% group_by(primary_genre) %>% summarize(cluster=n()) %>% mutate(rank=rank(-cluster)) %>% arrange(rank) %>% slice(1:filter_to_top) %>% select(primary_genre) %>% unlist
models = lapply(top_genres,build_model,glm,family=binomial)
predictions = lapply(models,predict,newdata = data.frame(modeling_matrix[!should_be_training,]),type="response")
predictions_frame = do.call(cbind,predictions) %>% as.data.frame()
names(predictions_frame) = top_genres
predictions_frame = cbind(topicsDF %>% select(cluster,primary_genre) %>% filter(!should_be_training),predictions_frame)
tidied = predictions_frame %>% gather("classified_genre","probability",-primary_genre,-cluster)
best_guesses = tidied %>% group_by(cluster) %>% 
  arrange(-probability) %>% slice(1) %>% # (Only take the top probability for each cluster)
  mutate(actual_genre=primary_genre)
confusion = best_guesses %>% group_by(actual_genre,classified_genre) %>% summarize(`count`=n())
confusion %>% group_by(actual_genre) %>% summarize(percent_right = 100 * sum(count[actual_genre==classified_genre])/sum(count)) %>% arrange(-percent_right)
confusion %>% group_by(1) %>% summarize(percent_right = 100 * sum(count[actual_genre==classified_genre])/sum(count)) %>% arrange(-percent_right)
return(confusion)
}

#code to run the functions
confusion = classification(doc.topics)
doc.topics = topicModel(allDataNoAds)