#Prepare data for Topic Modelling

#Get the Cluster Number and text column from Cluster frame 

#put the dataframe for topic modelling here, and don't change code below

#I removed numbers from stopwords.txt

dataTM = train_data

clustersForTM = dataTM[,c("doc_id","text","genre")] %>% filter(nchar(as.character(text), allowNA=TRUE) > 50) 
clustersForTM = data.frame(Cluster=as.character(dataTM$doc_id),Text=as.character(dataTM$text),Genre=as.character(dataTM$genre),stringsAsFactors = F)

input=clustersForTM

n.topics=12 
SEED = 1789

#Don't change below...

mallet.instances <- mallet.import(input$Cluster, 
                                  input$Text, 
                                  stoplist.file="data/stopwords.txt", 
                                  token.regexp = "\\w+",preserve.case=F)


topic.model <- MalletLDA(num.topics=n.topics)
#topic.model$model$setRandomSeed(as.integer(SEED))
topic.model$loadDocuments(mallet.instances)

#Look at the word frequencies sorted in order.
vocabulary <- topic.model$getVocabulary()
word.freqs <- mallet.word.freqs(topic.model)
head(word.freqs)

#Some preferences. Inside baseball: see Wallach and Mimno for what's going on.
topic.model$setAlphaOptimization(20, 50)
topic.model$train(300)
#Increase the fit without changing the topic distribution; optional
topic.model$maximize(10)

#Gets a list of the documents and topics
doc.topics <- mallet.doc.topics(topic.model, smoothed=T, normalized=T)
#Changes the orientation of that matrix to be horizontal:
topic.docs <- t(doc.topics)

#Gets a list of the top words.
topic.words <- mallet.topic.words(topic.model, smoothed=T, normalized=T)


#Assign some labels to the topics
topics.labels <- rep("", n.topics)
for (topic in 1:n.topics) {
  topics.labels[topic] <- paste(
    mallet.top.words(topic.model, topic.words[topic,], num.top.words=5)$words, collapse=" "
  )}
topics.labels
#to look at the labels, type "topics.labels"

rownames(doc.topics) = input$Cluster
colnames(doc.topics) = topics.labels


