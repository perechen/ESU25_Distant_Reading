# install.packages("caret")
# install.packages("e1071")


#### Method 1: stylo ####

####  NB! Function `classify()` looks in "primary_set" folder for training set and "secondary_set" for test set. It's kind of tedious. We are now hopefully confident enough to feed it data ourselves.

library(stylo)
library(tidyverse)

## for stats
library(caret)

set.seed(1981)
res_sampled <- stylo(gui=F,
                     corpus.dir = "corpus/",
                     sample.size=2000,
                     number.of.samples=5,
                     sampling="random.sampling",
                     mfw.min=20,
                     mfw.max=20)

z_scores <- as.matrix(res_sampled$table.with.all.zscores)

#### now, let's quickly take out a third of the available samples to serve as our "test set"
n_test <- ceiling(0.33*nrow(z_scores)) ## determine how many samples to take

set.seed(1989)
test_ids <- sample(1:nrow(z_scores),size = n_test,replace = F) # just sample rows

## subset samples into sets
test_set <- z_scores[test_ids,] # only test rows
train_set <- z_scores[-test_ids,] # without test rows


mfw=10 ## how many features we want to subset
first_classification <- classify(gui = F,
                                 distance.measure="dist.wurzburg",
                                 method="delta", # change to "svm", "knn", or "nsc"
                                 test.frequencies = test_set[,1:mfw], # set data manually
                                 training.frequencies = train_set[,1:mfw], # set data manually
                                 cost=1)
# check the results object
summary(first_classification) 

# check the overall success rate (accuracy)
first_classification$success.rate

predicted <- first_classification$predicted
expected <- first_classification$expected

# tabulate predictions vs. expectations
# note some imbalance in samples per class! this is ok, we just took rows randomly. for a truly stratified test/train split you would need to control for number of samples for each class
table(expected,predicted)

# stylo also offers quick cross-validation function
# running it may take some time on larger datasets...
cv_res <- crossv(training.set = z_scores[,1:10],# taking the whole table because of leave-one-out CV 
                 cv.mode = "leaveoneout",
                 classification.method = "delta",
                 distance="dist.wurzburg")

cv_res$y # results for all 'leave-out' samples, recording whether the classification was correct (1) or wrong (0)

cm <- cv_res$confusion_matrix
cm

accuracy <- sum(diag(cm)) / sum(cm)

accuracy

cv_res$confusion_matrix
### let's manually calculate precision, recall & F1 

# set variables
num_classes <- nrow(cm)
precision <- recall <- f1 <- numeric(num_classes)

## precision and recall should be calculated for each class separately
for (i in 1:num_classes) {
  # true positives (diagonal)
  tp <- cm[i, i]
  # false positives (columns)
  fp <- sum(cm[, i]) - tp
  # false negatives (rows)
  fn <- sum(cm[i, ]) - tp
  
  # precision: how many true positives among all positives
  precision[i] <- ifelse((tp + fp) == 0, 0, tp / (tp + fp))
  # recall: what is the proportion of false negatives 
  recall[i] <- ifelse((tp + fn) == 0, 0, tp / (tp + fn))
  
  ## F1 is a "harmonic mean" between recall and precision 
  f1[i] <- ifelse((precision[i] + recall[i]) == 0, 0, 
                  (2 * precision[i] * recall[i]) / ((precision[i]) + recall[i]))
}
## add names of classes to F1 variable
names(f1) <- colnames(cm)

f1
precision
recall
## total accuracy and average F1 of the experiment
cat("Accuracy: ", accuracy, "\t", "mean F1 score: ", mean(f1))


## luckily, there are many libraries that would make evaluation calculations much easier

# confusionMatrix expects factors as input
preds <- factor(cv_res$predicted)
expects <- factor(cv_res$expected)

# call a function
eval_summary<-caret::confusionMatrix(preds,expects,mode = "prec_recall")

eval_summary

f1_caret <- eval_summary$byClass[,"F1"]

f1==f1_caret # should be the same 


### YOUR TURN ###

# Use stylo()/classify()/crossv() to run classification on a different (larger) corpus, with different settings
# Play with sampling, classification method, etc.
# Possible candidate: corpus_FR_drama (comedy / tragedy classification)
# Develop a feel for the workflow! 
# You can wrap F1 calculation into a function, if you like it.

### END OF YOUR TURN ### 


##########################################################
##### Method 2. SVM from e1071 - DYI machine learning ####
##########################################################

library(tidytext)
library(e1071)


## list files in fr18 corpus

#unzip files if you need

files = list.files("corpus_FR18_drama/", full.names = T)


## read whole texts as character strings and put them into one tibble together with document names

## applies "read_file" to each file
fr_texts <- sapply(files, read_file)

corpus = tibble(title = files,
                text = fr_texts) %>% 
  mutate(title = str_replace(title, ".*/(.*?).txt", "\\1"), ## some cleaning of titles
         text = str_replace_all(text,"'|’", " "))  # some cleaning of texts


corpus

## words arranged by their frequency

rank = corpus %>%
  #tokenize by word -> new column "word" out of column "text"
  unnest_tokens(input = text, output = word, token = "words") %>%
  #count & sort
  count(word, sort = T) %>% 
  select(-n) %>% 
  head(5000) # cut wordlist to 5000 MFWs

rank

# calculate word frequencies for each text in corpus

freqs = corpus %>%
  unnest_tokens(input = text, output = word, token = "words") %>% # tokenization
  right_join(rank,by="word") %>% # we are leaving 5000 MFWs that we cut-off earlier 
  count(title, word) %>% # count words within each text
  group_by(title) %>%
  mutate(n = n/sum(n)) %>% # because of group_by() will sum word counts only within titles -> we get relative frequencies
  rename(text_title = title) %>%
  mutate(word = factor(word, levels = rank$word)) %>% #reorder words by their rank
  spread(key = "word", value="n",fill = 0) %>% # make the table wider
  ungroup()


freqs[1:10,1:15]

## scale desired amount of MFWs
z = freqs[,2:101] %>%
  scale() %>% 
  as_tibble() # transform back to tibble

## combine it back with titles
z_fin <- bind_cols(freqs[,1], z) %>% 
  mutate(text_genre = str_extract(text_title, "^."))


z_fin[1:10,1:10]

### Now when we have represented texts by feature vectors, we can split our data to test and train

## train / test split

## First, check genre labels

z_fin$text_genre %>% table()

ratio = 0.33
n_classes = unique(z_fin$text_genre) %>% length()
samples_per_class = ceiling(nrow(z_fin) * ratio) / n_classes

set.seed(1989)
test_set <- z_fin %>% 
  group_by(text_genre) %>% # group table by text genre 
  sample_n(samples_per_class) # sample across groups! easy!

# for train set, exclude test set with anti_join by titles!
train_set = z_fin %>%
  anti_join(test_set,by="text_title")

# remember it still has metadata columns! 

### fit SVM model

svm_model <-svm(as.factor(text_genre)~.,  # we try to predict genre with all words (GENRE ~ WORDS), so we take all columns as predictor variables, we use . to select all other columns
                data=train_set %>% select(-text_title), # fit on train data, remove titles column, 
                method="C-classification", 
                kernel="linear", 
                cost=1,
                scale=F)

### check summary
summary(svm_model)

### now predict classes from unseen test set with the model we have

prediction <- predict(svm_model, test_set %>% select(-text_title))

# final confusion matrix
table(test_set$text_genre, prediction)

### compute important features using slopes of vectors and Support Vector alignment to features

w = t(svm_model$coefs) %*% svm_model$SV

## prepare data frame
feature_df <- 
  tibble(weight=w[1,],  # weights
         word=colnames(w)) %>%  # feature names
  mutate(genre = case_when(weight > 0 ~ "Comedie", # label weights, comedie is a reference level , so positive values contribute to it
                           weight < 0 ~ "Tragedie"),
         genre=factor(genre,levels=c("Tragedie", "Comedie"))) %>% # reorder! 
  group_by(genre) %>% 
  top_n(20,abs(weight)) # select top N from each group (using abs() to select top negative too)


my_pallette <- c("purple", "gold")

# plot
feature_df %>% 
  ggplot(aes(reorder(word,abs(weight)),weight,fill=genre)) + geom_col() +
  coord_flip() + 
  scale_fill_manual(values=my_pallette) +
  theme_minimal() + 
  facet_wrap(~genre,scales="free") +
  theme(axis.text.y = element_text(size=14)) +
  labs(x=NULL, y="Feature contribution")

### in the era of the expanding need for "explainable AI" features, weights, visualizations, reasoning steps (for LLMs) are increasingly important! Blackboxes are not that black after all.

### if you want to learn more, see, for example, SHAP values that associate features to classes and are model-agnostic approach to describing model's decisions
### e.g.: https://christophm.github.io/interpretable-ml-book/shap.html

### BONUS: Rolling classification (with stylo)

url <- "https://github.com/computationalstylistics/RdlR_for_rolling_classify/archive/refs/heads/master.zip"

# Download the ZIP
download.file(url, "RomanDeLaRose.zip")

# Unzip the file
unzip("RomanDeLaRose.zip")

library(stylo)
v<-rolling.classify(training.corpus.dir = "RdlR_for_rolling_classify-master/reference_set/",
                 test.corpus.dir = "RdlR_for_rolling_classify-master/test_set/",
                 classification.method="delta",
                 slice.size = 2000,
                 slice.overlap = 500,mfw = 50,corpus.lang="Other")
