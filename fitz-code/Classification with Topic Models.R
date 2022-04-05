  
#This assumes topic modelling has been completed in topicmodellingGenres.rmd

#The input for classification is the topic modeled data frame from above.

#Create a dataframe with all topics for clusters that have known genres
topicsDF = doc.topics %>% 
  as.data.frame() %>% 
  mutate(cluster = input$Cluster, primary_genre = input$Genre) %>% 
  filter(primary_genre!="unknown")

#Create a dataframe for all topics for clusters with unknown genres
topics_unknown = doc.topics %>% 
  as.data.frame() %>% 
  mutate(cluster = input$Cluster, primary_genre = input$Genre) %>% 
  filter(primary_genre =="unknown")

#Convert to a matrix
modeling_matrix = topicsDF %>% select(-primary_genre, -cluster)
modeling_matrix = log(modeling_matrix)
dim(modeling_matrix)

#Create a matrix for all topics for clusters with unknown genres
unclassified_data = doc.topics %>% as.data.frame() %>% mutate(cluster = input$Cluster, primary_genre = input$Genre) %>% filter(primary_genre=="unknown") %>% select(-primary_genre,-cluster) %>% log

#Create a training set
should_be_training = sample(c(TRUE,FALSE),nrow(modeling_matrix),replace=T,prob = c(.75,.25))

#Convert training set into dataframe
training_frame = data.frame(modeling_matrix[should_be_training,])


#Build a model using GLM
build_model = function(genre,model_function=glm,...) {
  # genre is a string indicating one of the primary_genre fields;
  # model function is something like "glm" or "svm";
  # are further arguments passed to that function.
  training_frame$match=as.numeric(topicsDF$primary_genre == genre)[should_be_training]
  # we model against a matrix: the columns are the topics, which we get by dropping out the other four elements
  match_ratio = sum(as.numeric(training_frame$match))/length(training_frame$match)
  model = model_function(match ~ ., training_frame,...,weights = ifelse(match,1/match_ratio,1/(1-match_ratio)))
}

#Visualize top ten (filter_to_top) genres YOU MIGHT NEED TO CHANGE THIS
filter_to_top = 1
topicsDF %>% 
  filter(should_be_training) %>% 
  group_by(primary_genre) %>% 
  summarize(cluster=n()) %>% 
  mutate(rank=rank(-cluster)) %>% 
  arrange(rank) %>% 
  ggplot() + 
  geom_bar(aes(y=cluster,x=reorder(primary_genre,cluster),fill=primary_genre),stat="identity") + 
  coord_flip() + 
  labs(title="Most common genres, by number of clusters in training set")

#Create a value of the top genres. 
top_genres = topicsDF %>% group_by(primary_genre) %>% summarize(cluster=n()) %>% mutate(rank=rank(-cluster)) %>% arrange(rank) %>% slice(1:filter_to_top) %>% select(primary_genre) %>% unlist

top_genres



#Create models (models = lapply(top_genres,build_model,glm,family=quasibinomial,maxit = 100))
models = lapply(top_genres,build_model,glm,family=quasibinomial,maxit = 100)

# Regularization, look for other packages that might regularize the parameters. https://cran.r-project.org/web/packages/glmnet/glmnet.pdf
# Here's where we predict on out-of-model data. (Saved a model "model_87" which gives really good results. What's that about?)
predictions = lapply(models,predict,newdata = data.frame(modeling_matrix[!should_be_training,]),type="response")

# Convert to dataframe with scores for each genre  
predictions_frame = do.call(cbind,predictions) %>% as.data.frame()
names(predictions_frame) = top_genres

# Add cluster number and primary genre
predictions_frame = cbind(topicsDF %>% select(cluster,primary_genre) %>% filter(!should_be_training),predictions_frame)

#set.seed(1)
#predictions_frame %>% sample_n(6)

# Tidied data frame
tidied = predictions_frame %>% gather("classified_genre","probability",-primary_genre,-cluster)

# Create a data frame with top probability for each cluster
best_guesses = tidied %>% group_by(cluster) %>% 
  arrange(-probability) %>% slice(1) %>% # (Only take the top probability for each cluster)
  mutate(actual_genre=primary_genre)

confusion = best_guesses %>% group_by(actual_genre,classified_genre) %>% summarize(`count`=n())
ggplot(confusion) + geom_point(aes(y=classified_genre,x=count)) + facet_wrap(~actual_genre)
ggplot(confusion) + geom_bar(stat="identity") + aes(x=actual_genre,y=count,fill=classified_genre) + coord_flip() + 
  theme(plot.title = element_text(family = "Helvetica", color="#666666", face="bold", size=22, hjust=0)) +
  theme(axis.title = element_text(family = "Helvetica", color="#666666", face="bold", size=18)) + 
  theme(legend.title = element_text(family = "Helvetica", color="#666666", face="bold", size=18)) +
  theme(axis.text = element_text(family = "Helvetica", color="#333333", face="bold", size=16))

confusion %>% 
  group_by(actual_genre) %>% 
  summarize(percent_right = 100 * sum(count[actual_genre==classified_genre])/sum(count)) %>% 
  arrange(-percent_right)

confusion %>% 
  group_by(1) %>% 
  summarize(percent_right = 100 * sum(count[actual_genre==classified_genre])/sum(count)) %>% 
  arrange(-percent_right)

#END TEST DATA


#Create dataframe of averages per genre (first run, remove _column)
averages_column = confusion %>% 
  group_by(actual_genre) %>% 
  summarize(percent_right = 100 * sum(count[actual_genre==classified_genre])/sum(count)) %>% 
  arrange(-percent_right)

#Create dataframe of total averages (first run, remove _column)
total_averages_column = confusion %>% 
  group_by(1) %>% 
  summarize(percent_right = 100 * sum(count[actual_genre==classified_genre])/sum(count)) %>% 
  arrange(-percent_right)

#Merge columns into dataframes
averages = merge(averages,averages_column,by="actual_genre")
total_averages = merge(total_averages,total_averages_column,by="1")

#Name columns
names(averages)[2:11]<-paste("percent_right_",1:10,sep = "")
names(total_averages)[2:11]<-paste("percent_right_",1:10,sep = "")

#Compute averages
averages = averages %>% mutate("average" = rowMeans(averages[2:11]))
total_averages = total_averages %>% mutate("average" = rowMeans(total_averages[2:11]))

#Bar plot on averages
ggplot(averages) + geom_bar(stat="identity") + aes(x=actual_genre,y=average,fill=actual_genre) + coord_flip()




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

top_predictors = lapply(1:length(top_genres),function(n,return_length=50) {
  comedy_model = models[n][[1]]
  using = (rank((comedy_model$coefficients))<=(return_length/2)) | (rank(-comedy_model$coefficients)<=(return_length/2))
  coefficients = data.frame(genre = top_genres[n],topic=names(comedy_model$coefficients[using]) %>% gsub("modeling_matrix","",.),strength = comedy_model$coefficients[using],row.names = NULL)
  coefficients
}) %>% bind_rows

ggplot(top_predictors %>% filter(topic!="(Intercept)")) + geom_point(aes(x=strength,y=topic,color=strength>0)) + facet_wrap(~genre,scales="fixed",ncol=2)
ggplot(top_predictors %>% filter(topic!="(Intercept)", strength>0)) + geom_bar(stat="identity") + aes(x=topic,y=strength,fill=genre) + coord_flip() + 
  theme(plot.title = element_text(family = "Helvetica", color="#666666", face="bold", size=12, hjust=0)) +
  theme(axis.title = element_text(family = "Helvetica", color="#666666", face="bold", size=12)) + 
  theme(legend.title = element_text(family = "Helvetica", color="#666666", face="bold", size=12)) +
  theme(axis.text = element_text(family = "Helvetica", color="#333333", face="bold", size=12))



# Here's where we predict on out-of-model data.


out_of_domain_predictions = lapply(models,predict,newdata = data.frame(unclassified_data),type="response")
out_of_domain_predictions_frame = do.call(cbind,out_of_domain_predictions) %>% as.data.frame()
names(out_of_domain_predictions_frame) = top_genres
out_of_domain_predictions_frame = cbind(topics_unknown %>% select(cluster,primary_genre),out_of_domain_predictions_frame)
out_of_domain_predictions_tidied = out_of_domain_predictions_frame %>% gather("classified_genre","probability",-primary_genre,-cluster)
out_of_domain_predictions_best_guesses = out_of_domain_predictions_tidied %>% group_by(cluster) %>% 
  arrange(-probability) %>% slice(1) %>% # (Only take the top probability for each episode)
  mutate(actual_genre=primary_genre)
genreClass = out_of_domain_predictions_best_guesses %>% left_join(allData) 

genreClass$genre <- genreClass$classified_genre 
genreClass = genreClass[,c("cluster","genre","probability","text")]
#genreClass = genreClass %>% filter(probability>.8)
#genreClass = genreClass %>% filter(classified_genre=="literary")
#literaryClass <- genreClass %>% filter(genre=="literary")

#write.csv(genreClass, file = paste('output/genreClass-8-8-17.csv',sep=""))

# Remove dups and regularize column names
genreClassDeDup <- genreClass[!duplicated(genreClass[,1]),]
genreClassDeDup$genre <- genreClassDeDup$classified_genre

#visualize best guesses of unknown

out_of_domain_predictions_best_guesses %>% 
  group_by(classified_genre) %>% 
  summarize(cluster=n()) %>% 
  mutate(rank=rank(-cluster)) %>% 
  arrange(rank) %>% 
  ggplot() + 
  geom_bar(aes(y=cluster,x=reorder(classified_genre,cluster),fill=classified_genre),stat="identity") + 
  coord_flip() + 
  labs(title="Most commonly guessed genres, by number of clusters") + 
  theme(plot.title = element_text(family = "Helvetica", color="#666666", face="bold", size=22, hjust=0)) +
  theme(axis.title = element_text(family = "Helvetica", color="#666666", face="bold", size=18)) + 
  theme(legend.title = element_text(family = "Helvetica", color="#666666", face="bold", size=18)) +
  theme(axis.text = element_text(family = "Helvetica", color="#333333", face="bold", size=16))

out_of_domain_predictions_best_guesses %>% 
  group_by(classified_genre) %>% 
  summarize(cluster=n()) %>% 
  mutate(percent=(cluster/nrow(out_of_domain_predictions_best_guesses))*100) %>% 
  arrange(-percent) 
  


#Rank classified genres in order of probability 
genreRank = out_of_domain_predictions_tidied %>% group_by(cluster) %>% arrange(-probability) %>% mutate(genreOrder = paste("genre_",row_number(-probability),sep="")) 
genreRank$probability=paste(round(genreRank$probability*100,digits=2)) %>% as.numeric()

#Spread genre order for crowdsource app
genreRankDF2 <- genreRank %>% spread(genreOrder,classified_genre) %>% group_by(cluster) %>%
  summarise_all(funs(first(na.omit(.))))
genreRankDF2 = genreRankDF2 %>% left_join(allData) 
genreRankDF2 = genreRankDF2[,c("cluster","text","genre_1","genre_2","genre_3","genre_4")]
write.csv(genreRankDF2, file = paste('output/genreRank-8-28-17.csv',sep=""))

#Histogram for genreRank

genreRankTest <- genreRank %>% group_by(cluster) %>% sample_n_groups(40)

genreRankTest %>% group_by(cluster) %>% 
  ggplot() + geom_bar(stat="identity") + aes(x=classified_genre,y=probability,fill=classified_genre) + coord_flip() + facet_wrap(~cluster,ncol=4)


genreRankTest %>% group_by(cluster) %>% 
  ggplot() + geom_histogram(stat="identity") + aes(x=cluster,y=probability,fill=classified_genre) + coord_flip() 


#Dataframe for genreRank THE RESHAPE PACKAGE FOR CAST BREAKS THE MODEL FOR SOME REASON
genreRankDF = genreRank %>% cast(cluster~classified_genre, value="probability") 
genreRankDF = genreRankDF %>% left_join(allData) 

# changing the names from poetry and prose to literary and informatinal from original test data
#names(genreRankDF) <- c("cluster", "ads", "news", "literary", "informational", "text", "original_genre")

genreRankDF = genreRankDF[,c("cluster","text","literary","ads","news","informational")]
write.csv(genreRankDF, file = paste('output/genreRank-6-15-18.csv',sep=""))

genreRankDF %>% ggplot() + geom_bar() + aes(x=cluster) + coord_flip() + facet_wrap(~cluster,ncol=4)


#merge justPoems, justProse, NewsPoemsProse, and take the highest probability/genre for each cluster
NewsPoemsProse = read.csv("output/NewsPoemsProse-5-22-16.csv", header=TRUE, fill = TRUE, sep = ",", row.names = NULL, stringsAsFactors = FALSE)
NewsPoemsProse = NewsPoemsProse[,c("cluster","Text","classified_genre","probability")]
justPoems = justPoems[,c("cluster","Text","classified_genre","probability")]
justProse = justProse[,c("cluster","Text","classified_genre","probability")]
testing = rbind(justPoems,justProse,NewsPoemsProse)
testing = testing %>% group_by(cluster) %>% arrange(-probability) %>% slice(1)
write.csv(testing, file = paste('output/topPicks-5-23-16.csv',sep=""))


#just poems
justPoems = genreClass %>% filter(classified_genre=="poetry")
justPoems = justPoems[,c("cluster","classified_genre","probability","Text")]
write.csv(justPoems, file = paste('output/justPoems-11-2-16.csv',sep=""))

#just prose
justProse = genreClass %>% filter(classified_genre=="prose")
justProse = justProse[,c("cluster","classified_genre","probability","Text")]

#just poems and prose
NewsPoemsProse = rbind(justProse,justPoems,justNews)

poemsANDprose = rbind(justProse,justPoems)
poemsANDprose <- poemsANDprose[sample(1:nrow(poemsANDprose), 200,replace=FALSE),]
write.csv(poemsANDprose, file = paste('output/poemsANDprose-11-17-16.csv',sep=""))

NjustProse = justProse %>% left_join(newData)
write.csv(justProse, file = paste('output/justProse-6-29-16.csv',sep=""))

#just prose (from genreRank)
justProse = genreRank %>% filter(prose >= 70)
justProse = justProse[,c("cluster","classified_genre","probability","Text")]
justProse = justProse %>% left_join(newData)
write.csv(justProse, file = paste('output/justProse-5-23-16.csv',sep=""))

#just news
justNews = genreClass %>% filter(classified_genre=="news")
justNews = justNews[,c("cluster","classified_genre","probability","Text")]
write.csv(justNews, file = paste('output/justNews-5-23-16.csv',sep=""))

#just ads
justAds = genreClass %>% filter(classified_genre=="advertisement")
justAds = justAds[,c("cluster","classified_genre","probability","Text")]
write.csv(justAds, file = paste('output/justAds-5-23-16.csv',sep=""))


#merge the just__ dataframes and get best probabilities
justEverything = rbind(justPoems,justProse,justNews)
justEverything = justEverything %>% group_by(cluster) %>% arrange(-probability) %>% slice(1)
justEverything = justEverything %>% left_join(newData)
justEverything = justEverything[,c("cluster","date","classified_genre","Text","url","corpus")]
write.csv(justEverything, file = paste('output/justEverything-5-23-16.csv',sep=","),row.names=FALSE)
write.xlsx(justEverything, file = paste('output/justEverything-5-23-16.xlsx'),row.names=FALSE)

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


