# install.packages("topicmodels")
# install.packages("umap")
# install.packages("Rtsne")

library(tidyverse)
## text & topics
library(tidytext)
library(topicmodels)
## projections
library(Rtsne)
library(umap)
## visuals
library(plotly)

## LDA: a minimal example. 

# We have a collection of documents (only three for now). These are the beginnings of Wikipedia articles on:  
# [Space Exploration](https://en.wikipedia.org/wiki/Space_exploration);
# [Space Adventures (comics)](https://en.wikipedia.org/wiki/Space_Adventures_(comics)); 
# [Superhero Comics](https://en.wikipedia.org/wiki/Superhero_comics).

text1 <- "Space exploration is the process of utilizing astronomy and space technology to investigate outer space. While the exploration of space is currently carried out mainly by astronomers with telescopes, its physical exploration is conducted both by uncrewed robotic space probes and human spaceflight. Space exploration, like its classical form astronomy, is one of the main sources for space science. "

text2 <- "Space Adventures is an American science-fiction anthology comic book series that was published sporadically by Charlton Comics from 1952 to 1979. Its initial iteration included some of the earliest work of industry notables Steve Ditko, Dick Giordano, and Tony Tallarico, and at least one story by EC Comics mainstay Bernard Krigstein. In 1960, a second iteration introduced the superhero Captain Atom by writer Joe Gill and artist Ditko, shortly prior to Ditko's co-creation of Spider-Man for Marvel Comics."

text3 <- "Superhero comics is one of the most common genres of American comic books. The genre rose to prominence in the 1930s and became extremely popular in the 1940s and has remained the dominant form of comic book in North America since the 1960s. Superhero comics feature stories about superheroes and the universes these characters inhabit. Beginning with the introduction of Superman in 1938 in Action Comics #1 (an anthology of adventure features) comic books devoted to superheroes (heroic people with extraordinary or superhuman abilities and skills, or god-like powers and attributes) ballooned into a widespread genre, coincident with the beginnings of World War II and the end of the Great Depression. "

## make a table 

df <- tibble(doc=c("Space Exploration", "Space Comics", "Superheroes"),
             text=c(text1, text2, text3))

tokenized <- df %>%
  unnest_tokens(output=word, input=text, token="words") %>%
  anti_join(stop_words)

## Sidenote: stopwords removal effect
t2_after <- tokenized %>% 
  filter(doc=="Space Exploration") %>%
  pull(word) %>% 
  paste(collapse = " ")

cat(df$text[1],"\n\n", t2_after)

## now , to counting and 'wide' Document-Term-Matrix representation
tokenized %>% count(doc,word)

dtm <- tokenized %>% 
  count(doc, word) %>% #count words within each "document"
  cast_dtm(document = doc, term = word, value=n) #define matrix with your columns

## it's a special DTM object...
dtm

## but it's really just a package for a matrix!
as.matrix(dtm[1:3,1:10])

## you need just one function to fit an LDA model... but be mindful of parameters

n_topics = 2 # number of topics to fit
r_seed = 1989 # to initialize the model from the same state
alpha = 1 # topic density in a document
delta = 0.01 # word density in a topic 

toy_lda = LDA(dtm, k=n_topics, method="Gibbs", control=list(seed=r_seed,
                                                            alpha=alpha,
                                                            delta=delta))



## 'beta' is an estimate of word probabilities in topics
topics <- tidy(toy_lda, matrix="beta")


top_words = 4

top_terms <- topics %>% 
  group_by(topic) %>% 
  top_n(top_words, beta) %>% #top 4 probabilities by each topic
  arrange(topic,-beta)
top_terms

## often words in topics are visualized as a bar plot
my_palette <- c("darkblue", "gold")

top_terms %>% 
  mutate(topic=factor(topic)) %>% 
  ggplot(aes(beta, reorder(term, beta), fill=topic)) + #reorder terms by probability
  geom_col() + 
  facet_wrap(~topic, scales = "free") + 
  theme_minimal(base_size = 14) + 
  scale_fill_manual(values=my_palette) + 
  labs(x="terms",y=NULL)

## now, time for fun: documents as topics! 
## "gamma" is a distribution of topic probabilities over documents 
tidy(toy_lda, matrix="gamma")


doc_probs <- tidy(toy_lda, matrix="gamma") %>%
  mutate(topic=factor(topic),
         document=factor(document, levels = c("Space Exploration",
                                              "Space Comics", 
                                              "Superheroes"))) 
## visualize topic distribution over documents
doc_probs %>%
  ggplot(aes(topic, gamma, fill=topic)) + 
  ## geometries
  geom_col(width = 1) + 
  facet_wrap(~document) + 
  ## appearance 
  theme_minimal(base_size = 14) + 
  scale_fill_manual(values=my_palette) + 
  labs(y="Probability", x="Topic")

## 2D visualization
doc_probs %>% 
  pivot_wider(names_from = topic,values_from = gamma) %>% 
  ggplot(aes(x=`1`,y=`2`)) + 
  ## geometries
  geom_point(size=5,color="darkblue") +
  geom_text(aes(label=document),nudge_y = 0.05) + 
  ## appearance 
  theme_minimal(base_size = 14) + 
  labs(x="Topic 1: space, exploration, science",
       y="Topic 2: comics, superhero, genre")

## predicting topics in unseen documents

new_text <- "Superhero film/movie is a film genre categorized by the presence of superhero characters, individuals with extraordinary abilities who are dedicated to fighting crime, saving the world, or helping the innocent. It is sometimes considered a sub-genre of the action film genre and has evolved into one of the most financially successful film genres worldwide. These films focus on superhuman abilities, advanced technology, mystical phenomena, or exceptional physical and mental skills that enable these heroes to fight for the common good or defeat a supervillain antagonist. Superhero films typically include genre elements of romance, comedy, fantasy, and science fiction, with large instances of the superhero genre predominantly occupied and produced by American media franchises DC and Marvel, originally adaptations of their existing works of superhero comic books. Individual superhero films frequently contain a character's origin story."

dtm_new <- tibble(doc="Superhero film",text=new_text) %>%
  unnest_tokens(input=text,output=word) %>%
  filter(word %in% tokenized$word) %>%  # to set up the same vocabulary as a model we use
  bind_rows(tokenized) %>% # we need the original data to set our new document in the same feature space
  count(doc,word) %>% 
  cast_dtm(document = doc,term=word,value=n)

as.matrix(dtm_new[1:4,1:5])
v<-posterior(toy_lda, newdata = dtm_new[3,],)


v$topics # our new document is 82% comic books and superheroes. that's nice! 

### YOUR TURN

# play with LDA parameters to build up intuitions around it! 
# what will happen if you build a model with three topics? 
# what will happen if you remove words that appear only once?

## END OF YOUR TURN


## Let's LDA larger corpus with English genre fiction! 

#corpus_url <- "https://drive.google.com/uc?export=view&id=1Y8toRCeDR5iPlLJ12S1ILoNyqCT5g1_J"

# Download the ZIP
#download.file(corpus_url, "Genre_fiction.zip")

# Unzip the file
#unzip("Genre_fiction.zip")


# list files
files <- list.files("corpus_genre/")
paths <- list.files("corpus_genre/",full.names = T)

genre_df <- tibble(doc=str_remove(files,"\\.txt$"),
       text=lapply(paths, read_file)) %>%
  unnest(text)


## initialize some settings! 

top_words = 10000 # how many most frequent words to use in a model
chunk_size = 500 # how large will be text segments (chunking long texts is important!)
doc_proportion = 0.5 # in how many documents (novels) a word should appear to be included

# for LDA
n_topics = 15 # how many topics
r_seed = 1989
alpha=0.1
delta=0.1

genre_tokens <- genre_df %>%
  unnest_tokens(input=text,output=word) %>% 
  anti_join(stop_words)

## count all words to filter later by frequency
frequency_rank <- genre_tokens %>% count(word,sort=T)

## filter by MFW
genre_tokens_filt <- genre_tokens %>% 
  filter(word %in% frequency_rank$word[1:top_words])

### START: Determine words shared across documents
n_docs <- nrow(genre_df)
counts_novel <- genre_tokens_filt %>% count(doc,word)

shared_words <- counts_novel %>%
  count(word) %>% # this count determines in how many novels a word appears
  mutate(p=n/n_docs) %>%  # proportion from all novels
  filter(p >= doc_proportion) # filter by proportion 
### END

## now to chunking
genre_tokens_chunked <- genre_tokens_filt %>% 
  group_by(doc) %>% 
  mutate(chunk= ceiling( row_number() / chunk_size )) # can you tell what this lines does?

genre_tokens_chunked
genre_tokens_chunked$chunk[1:1000] # 'chunk' labels for the first 1k words


## count words within chunks, add chunk number to doc name!
counts <- genre_tokens_chunked %>% 
  group_by(doc,chunk) %>% # important grouping
  count(word) %>%  # count within group
  filter(word %in% shared_words$word) %>%  # filter words that are novel-specific
  unite(doc_id,c(doc,chunk),sep="_") # unite two columns into one called "doc_id"


## now, to construct DTM 
dtm <- counts %>% cast_dtm(document = doc_id,
                           value = n,
                           term = word)


## finally, train an LDA model 
lda_genre = LDA(dtm, k=n_topics, method="Gibbs",control = list(seed=r_seed,
                                                               alpha=alpha,
                                                               delta=delta))


## exploring the model

## 1. topics
top_words = 10

topics_genre <- tidy(lda_genre, matrix="beta")

top_terms <- topics_genre %>% 
  group_by(topic) %>% 
  filter(!term %in% c("time","people")) %>% 
  top_n(top_words, beta) %>% #top 4 probabilities by each topic
  arrange(topic,-beta)

top_terms %>% 
  mutate(topic=factor(topic)) %>% 
  ggplot(aes(beta, reorder(term, beta), fill=topic)) + #reorder terms by probability
  geom_col() + 
  facet_wrap(~topic, scales = "free",ncol = 5) + 
  theme_minimal(base_size = 10) + 
  labs(x="terms",y=NULL) + guides(fill="none")


## 2. documents

## first, bird-eye view .
## we will represent documents by topic probability and project them on a 2D space for plotting

doc_probs <- tidy(lda_genre, matrix="gamma") %>%
  mutate(topic=factor(topic),
         genre=str_extract(document, "^.."),
         author=str_extract(document,"_[a-zA-Z].*?_"),
         author=str_remove_all(author,"_"))

dtm_docs <- doc_probs %>%
  pivot_wider(names_from = topic,values_from = gamma)

meta <- dtm_docs[,1:3]
dtm_probs <- dtm_docs[,-c(1:3)] %>% as.matrix()

# tSNE
set.seed(1989) # fix random seed for reproducibility
tsne <- Rtsne(dtm_probs, dims = 2, perplexity = 30)

# UMAP
set.seed(1989) 
umap_proj <- umap(dtm_probs,n_neighbors=25,preserve.seed = T)

tsne_df <- tibble(
  X = tsne$Y[, 1],
  Y = tsne$Y[, 2],
) %>% bind_cols(meta)

umap_df <- tibble(
  X = umap_proj$layout[,1],
  Y = umap_proj$layout[,2]
) %>% bind_cols(meta)



# Interactive plot with `plotly` package
plot_ly(tsne_df, # data frame
        x = ~X,  # which column to X-axis
        y = ~Y,  # which to Y
        type = 'scatter', # visualization type
        color= ~genre,  # color by a variable 
        mode = 'markers', 
        text = ~document, # define  a text string associated with with each point 
        hoverinfo = 'text') # hover from 'text' parameter


# make topic labels
topic_labels <- top_terms %>% group_by(topic) %>% top_n(8) %>% summarize(label=paste(term,collapse="_"))

## a convenience function for plotting
topics_across_novels <- function(x=doc_probs,l=topic_labels,t=1) {
  
  ## choose a label
  lbl <- topic_labels %>% 
    filter(topic==t) %>%
    pull(label)
  
  ## tinker with data and plot
  p<- x %>% mutate(book=str_remove(document, "_[0-9]*$"),
                       chunk=as.integer(str_extract(document,"[0-9]*$"))) %>% 
    filter(topic==t) %>% 
    ggplot(aes(chunk,book)) + 
    theme_minimal() +
    facet_wrap(~genre,ncol = 1,scale="free_y") +
    geom_tile(aes(fill=gamma)) + 
    labs(title=paste0("Topic ", t,": ", lbl)) +
    scale_fill_gradient(low="gold",high="red")
  
  return(p)
  
} 

topics_across_novels(doc_probs,t=1)




## novel as a probability distribution over topics
topic_labels <- top_terms %>% group_by(topic) %>% top_n(3) %>% summarize(label=paste(term,collapse="_"))


novel_means <- doc_probs %>% mutate(book=str_remove(document, "_[0-9]*$")) %>%
  group_by(book,topic,genre) %>% 
  summarize(p=mean(gamma)) # here we average each topic expression across the whole novel
novel_means

novel_means %>% 
  ggplot(aes(topic, book)) + geom_tile(aes(fill=p)) +scale_fill_gradient(low="white",high="red") + facet_wrap(~genre,scales = "free_y",ncol=1) + labs(title="Documents over topics") + scale_x_discrete(labels=topic_labels$label) + theme(axis.text.x = element_text(angle=45,vjust = 0.75,hjust = 1)) + labs(y=NULL)


### YOUR TURN

## Play with model parameters, evaluate the output (note that it will take some time for LDA to train); change chunk size, word filtering principles etc.
## - What happens if you train LDA with 2 topics? 
## - Can you wrap the data preparation / LDA code into function or two?
## If you have extra time, you can:
## - build a classifier that tells apart SF and Fantasy based on topics. Use `dtm_docs` table!

### END OF YOUR TURN



# a quick intro to top2vec


# we need to download a "pretrained model", more info here: https://github.com/maxoodf/word2vec#basic-usage
library(word2vec)
library(doc2vec)
library(dbscan)

options(timeout = 900) # set download timeout to 15 minutes (default is 1 minute) 
download.file(url = "https://owncloud.gwdg.de/index.php/s/zG7Ty3XZGbewAaP/download", destfile = "cb_ns_500_10.w2v")
# just consider that it was trained on 11.8GB English texts corpus!

# load the model
model <- read.word2vec(file = "cb_ns_500_10.w2v", normalize = TRUE)
# check the model
predict(model, newdata = c("sherlock", "man", "woman"), type = "nearest", top_n = 5)

## rechunk to smaller contexts
genre_tokens <- genre_df %>%
  unnest_tokens(input=text,output=word) %>% 
  anti_join(stop_words)

## count all words to filter later by frequency
frequency_rank <- genre_tokens %>% count(word,sort=T)

## filter by MFW
genre_tokens_filt <- genre_tokens %>% 
  filter(word %in% frequency_rank$word[1:10000])

### START: Determine words shared across documents
n_docs <- nrow(genre_df)
counts_novel <- genre_tokens_filt %>% count(doc,word)

shared_words <- counts_novel %>%
  count(word) %>% # this count determines in how many novels a word appears
  mutate(p=n/n_docs) %>%  # proportion from all novels
  filter(p >= doc_proportion) # filter by proportion 
### END

## now to chunking
genre_tokens_chunked <- genre_tokens_filt %>% 
  filter(word %in% shared_words$word) %>% 
  group_by(doc) %>% 
  mutate(chunk= ceiling( row_number() / 200 )) 

## make a doc_id + text table
doc_df <- genre_tokens_chunked %>%
  group_by(doc,chunk) %>% 
  summarise(text=paste(word,collapse=" ")) %>% 
  unite(doc_id, c(doc,chunk), sep="_")



paragraph_vectors = paragraph2vec(doc_df,
                                  type="PV-DBOW",
                                  iter=20,
                                  dim=500,
                                  threads=4,
                                  embeddings=as.matrix(model))

# get docs & words embeddings
emb_docs = as.matrix(paragraph_vectors, which='docs')
emb_words = as.matrix(paragraph_vectors, which='words')

# tSNE
set.seed(1989) # fix random seed for reproducibility
tsne <- Rtsne(emb_docs, dims = 2, perplexity = 30)

# UMAP
set.seed(1989) 
umap_proj <- umap(emb_docs,n_neighbors=20,preserve.seed = T)

tsne_df <- tibble(
  X = tsne$Y[, 1],
  Y = tsne$Y[, 2],
  document = rownames(emb_docs)
) %>%
  mutate(genre=str_extract(document, "^.."),
         author=str_extract(document,"_[a-zA-Z].*?_"),
         author=str_remove_all(author,"_"))

umap_df <- tibble(
  X = umap_proj$layout[,1],
  Y = umap_proj$layout[,2],
  document = rownames(emb_docs)
) %>%
  mutate(genre=str_extract(document, "^.."),
         author=str_extract(document,"_[a-zA-Z].*?_"),
         author=str_remove_all(author,"_"))

set.seed(1989)
hdb <- hdbscan(tsne$Y, minPts = 5)
## Add cluster info to main table
tsne_df <- tsne_df %>% mutate(cluster=hdb$cluster)
#umap_df <- umap_df %>% mutate(cluster=hdb$cluster)

# Interactive plot with `plotly` package
plot_ly(tsne_df, # data frame
        x = ~X,  # which column to X-axis
        y = ~Y,  # which to Y
        type = 'scatter', # visualization type
        color= ~cluster,  # color by a variable 
        mode = 'markers', 
        text = ~paste(document, "<br>cluster:", cluster), # define  a text string associated with with each point 
        hoverinfo = 'text') # hover from 'text' parameter


get_topics <- function(x,hdbscan=hdb,top_n=10) {
  
  ## first get all chunks associated with cluster
  docs_in_cluster <- which(hdbscan$cluster == x)
  ## cluster size
  n_docs <- length(docs_in_cluster)
  ## calculate cluster centroid: average over dimensions
  cluster_center <- emb_docs[docs_in_cluster,] %>% colMeans()
  ## get neighbor
  cl_df <- predict(model, newdata = cluster_center, type = "nearest", top_n = top_n)
  ## combine in a dataframe
  cl_df <- as_tibble(cl_df) %>% mutate(cluster=x,
                                       center=list(cluster_center),
                                       size=n_docs)
  return(cl_df)
}

cl <- hdb$cluster %>% table() %>% names() %>% as.numeric()

topics_df <- lapply(cl[-1],get_topics)

top_clusters <- topics_df %>% bind_rows() %>% arrange(-size)#  %>%  pull(cluster) %>% unique()
top_clusters
topics_df %>% bind_rows() %>% filter(cluster==3)
