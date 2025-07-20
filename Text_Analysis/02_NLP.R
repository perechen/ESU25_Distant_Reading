### Natural Language Processing with R

# This is an R script file, adjusted by Artjoms from Simone
# Everything written after an hashtag is a comment
# Everything else is R code
# To activate the code, place the cursor on the corresponding line
# (or highlight multiple lines/pieces of code) 
# ...and press Ctrl+Enter (or Cmd+Enter for Mac)
# (the command will be automatically copy/pasted into the console)

# before everything starts: check the working directory!
# you can do it by running `getwd()` command
# (it should be YOUR_COMPUTER/*/Distant_Reading_in_R/)

# required packages, part I

# install.packages("udpipe")
# install.packages("tidyverse")
# install.packages("word2vec")

#  required packages, part II

# install.packages("Rtsne")
# install.packages("dbscan")
# install.packages("plotly")

# you can also install them by using the yellow warning above
# (...if it appears)

### 1. 
### Let's use an NLP library
### (examples adapted from: https://bnosac.github.io/udpipe/en/)

# load libraries
library(udpipe)
library(tidyverse)

### Simplest example

# choose a text
my_text <- "To be or not to be. That is the question, my dear Watson."

# annotate it with udpipe!
x <- udpipe(x = my_text, object = "english")
View(x)

# of course, you can also work on different languages
my_text <- "Nel mezzo del cammin della mia vita, mi ritrovai con il mio caro Watson."

# annotate it with udpipe!
x <- udpipe(x = my_text, object = "italian")
View(x)

### let's work on one entire novel

# read the novel (read_file() from tidyverse)
my_text <- read_file("corpus/Doyle_Study_1887.txt")

# check the first 1000 characters
substr(my_text,start = 1,stop = 1000)

# analyze with Udpipe
doyle <- udpipe(x = my_text, object = "english")

### Dispersion plots (example taken from Matthew Jockers, Text analysis with R for students of literature, 2014; adapted for tidyverse & ggplot)

# first, let's find the appearances of a certain word in the text
sherlock_v1 <- doyle %>% 
  as_tibble() %>%  # 'upgrade' to tibble
  mutate(key=ifelse(lemma=="Sherlock", yes=1, no=0)) %>% # check the lemma column for key, mark it 1 or 0
  select(key) # select the new 'key' column

sherlock_v2 <- sherlock_v1 %>%
  mutate(position=row_number())  %>% # numerate the rows == word positions in the story
  filter(key==1) # filter only matches

sherlock_v2

ggplot(sherlock_v2,aes(position, key)) +  # set mapping
  geom_col(width = 150,position = "identity") + # column geometry
  theme_minimal() + # just theme
  xlim(0,nrow(doyle)) + # limit x axis to full size of the novel
  theme(axis.text.y=element_blank()) +  # remove Y-axis text for aesthetic pleasure
  labs(x="Novel time (tokens)", y=NULL, title="Dispersion Plot of 'Sherlock' in A study in Scarlet")


### YOUR TURN

# let's practice plotting and getting information from texts
# 1. get occurrences of another keyword, "Lucy" 
# 2. plot it 
# 
# 2* plot it together with "Sherlock"?
# 3. color the bars by the keyword?


### END OF YOUR TURN


### Keyword in context
match <- sherlock_v2$position[1] # take the first time the word "Sherlock" occurs 
window <- 5 # how many tokens in the left and right context

kwic <- doyle$token[(match-window):(match+window)] # takes only tokens in a window from the position of a match
kwic

cat(match, "\t", kwic)

### put it in a loop

for(match in sherlock_v2$position){
  
  cat(match, "\t", doyle$token[(match-window):(match+window)], "\n")
  
}

### Better keyword in context (per sentence)

# find sentence id

my_sent_id <- doyle$sentence_id[sherlock_v2$position]
my_sent_id


# print just the first
first_sentence <- doyle %>%
  filter(sentence_id == my_sent_id[1]) %>%  # filter the first sentence
  pull(token) # pull command "pulls" a column from a data frame and takes it as a vector


cat(first_sentence) 

# put all in a loop
for(match in my_sent_id){
  
  sentence <- doyle %>% filter(sentence_id==match) %>% pull(token)
  
  cat(match, "\t", sentence, "\n")
  
}

### Overall stats per part of speech

# calculate frequencies of "upos"
stats <- txt_freq(doyle$upos)


# plot result using ggplot
ggplot(stats, mapping = aes(x = key, y = freq)) +
  geom_col(fill = "cadetblue") +
  labs(title = "UPOS (Universal Parts of Speech)\nfrequency of occurrence")

## not very nice looking , isn't it? 
## few ways to rearrange 

# 1. convert to factor (factors are a special way to deal with categorical data, e.g. strings)
stats <- stats %>% mutate(key2=factor(key,levels=key))

ggplot(stats, mapping = aes(x = key2, y = freq)) +
  geom_col(fill = "cadetblue") +
  labs(title = "UPOS (Universal Parts of Speech)\nfrequency of occurrence", x=NULL)


# 2. use reorder() by frequency

ggplot(stats, mapping = aes(x = reorder(key, -freq), y = freq)) +
  geom_col(fill = "cadetblue") +
  labs(title = "UPOS (Universal Parts of Speech)\nfrequency of occurrence",x=NULL)


### NOUNS
# same procedure as above, but preselecting just nouns
noun_df <- doyle %>% filter(upos=="NOUN")

stats <- txt_freq(noun_df$token)

ggplot(data = stats[1:20,], mapping = aes(x = reorder(key, -freq), y = freq)) +
  geom_bar(stat = "identity", fill = "cadetblue") +
  theme(axis.text.x = element_text(angle = 45,vjust = 0.6)) +
  labs(title = "Most occurring nouns",x=NULL)


### Your Turn (1) - start

# try to do the same as above, but for adjectives
# suggestion: just copy/paste the code above
# modify the code and run it!
# (if you feel confident, you can also try to repeat all operations on a different text)



### Your Turn (1) - end


### 2.
### Word embeddings
library(word2vec)

set.seed(1234) # this is necessary for reproducibility

### We can create our own embeddings based on our corpus

# list all text files
all_text_files <- list.files("corpus", full.names = T)
all_text_files

# read them all
all_texts <- lapply(all_text_files, readLines)

# unlist and convert to lowercase
all_texts <- all_texts %>% unlist() %>% tolower()
all_texts <- tolower(unlist(all_texts))
head(all_texts)

# now we can train our model (it will take a bit)
model <- word2vec(x = all_texts, type = "cbow", dim = 50, iter = 20)

# we can see the embeddings
embedding <- as.matrix(model)
View(embedding)

# find closest words
lookslike <- predict(model, "forest", type = "nearest", top_n = 10)
lookslike

### Your Turn (2) - start

# experiment a bit with the model!
# try with other words
# or train the model with different setups



### Your Turn (2) - end


### Let's visualize the whole embedding space! 

# Example dimensionality reduction with t-SNE
library(Rtsne)
library(dbscan)
library(plotly)

set.seed(1989) # fix random seed for reproducibility
tsne <- Rtsne(embedding, dims = 2, perplexity = 30)

# Create a data frame for plotting
tsne_df <- tibble(
  X = tsne$Y[, 1],
  Y = tsne$Y[, 2],
  label = rownames(embedding)
)

## HDBscan to map "dense" regions 
hdb <- hdbscan(tsne$Y, minPts = 10)
## Add cluster info to main table
tsne_df <- tsne_df %>% mutate(cluster=hdb$cluster)


# Interactive plot with `plotly` package
plot_ly(tsne_df, # data frame
        x = ~X,  # which column to X-axis
        y = ~Y,  # which to Y
        type = 'scatter', # visualization type
        color=~cluster,  # color by `cluster` column
        mode = 'markers', 
        text = ~paste(label, "<br>cluster:", cluster), # define  a text string associated with with each point 
        hoverinfo = 'text') # hover from 'text' parameter



### Appendix. Pretrained models

# we need to download a "pretrained model", more info here: https://github.com/maxoodf/word2vec#basic-usage
options(timeout = 300) # set download timeout to 5 minutes (default is 1 minute) 
download.file(url = "https://owncloud.gwdg.de/index.php/s/zG7Ty3XZGbewAaP/download", destfile = "cb_ns_500_10.w2v")
# just consider that it was trained on 11.8GB English texts corpus!

# we read the model into R
model <- read.word2vec(file = "cb_ns_500_10.w2v", normalize = TRUE)

# then we can predict, the words that are closest to...
predict(model, newdata = c("sherlock", "man"), type = "nearest", top_n = 5)

# we can also make operations: actor - man + woman = ?
# first, we extract the vectors
wv <- predict(model, newdata = c("actor", "man", "woman"), type = "embedding")
View(wv)
# then we make our "operation" 
wv_new <- wv["actor", ] - wv["man", ] + wv["woman", ]
# and we predict the closest word to the result
predict(model, newdata = wv_new, type = "nearest", top_n = 1)

# also:
wv <- predict(model, newdata = c("white", "racism", "person"), type = "embedding")
wv_new <- wv["white", ] - wv["person", ] + wv["racism", ] 
predict(model, newdata = wv_new, type = "nearest", top_n = 10)
