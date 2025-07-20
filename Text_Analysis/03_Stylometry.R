### Stylometry with R

# This is an R script file, adjusted by Artjoms from Simone
# Everything written after an hashtag is a comment
# Everything else is R code
# To activate the code, place the cursor on the corresponding line
# (or highlight multiple lines/pieces of code) 
# ...and press Ctrl+Enter (or Cmd+Enter for Mac)
# (the command will be automatically copy/pasted into the console)

# before everything starts: check the working directory!
# (it should be YOUR_COMPUTER/*/Distant_Reading_in_R/)

# required packages:
# install.packages("stylo")

# for Mac users: you would need to install XQuarts: just google it and install

# you can also install them by using the yellow warning above
# (...if it appears)

### 1. Delta Analysis

# call the "stylo" library
library(stylo)

# Stylo works by default on the files in the "corpus" folder inside the working directory
# if you are not there, it will ask you to reach that folder
# HOWEVER, it is always good practice to define the working directory from the beginning
# it can be done via the menu "Session" -> "Set Working Directory" -> "Choose Working Directory"
# in the menu, you will have to browse to the folder that contains the "corpus" folder (NOT to the "corpus" folder itself!)

# to use stylo, write this very simple command:
stylo()
# it has a user interface, so (for simple experiments) it does not require coding

# in the first panel (Input & Language), select: "plain text", "English (ALL)", and "Native encoding"
# Try different experiments:
# 100MFW min and max, 0 increment (features), CA with Classic and Cosine Delta (statistics)
# 2000MFW min and max, 0 increment (features), CA with Classic and Cosine Delta (statistics)
# 200-2000MFW, 200 increment (features), BCT with Classic and Cosine Delta (statistics)

# try also sampling, to see to which is the minimum amount of text for Delta to work 
# sampling: random; sample size: 5000
# sampling: random; sample size: 2000
# sampling: random; sample size: 1000
# sampling: random; sample size: 500

# also, note that each analysis has generated a .csv file that can be opened with Gephi (for network analysis)

# It can be useful to save the texts into R variables, so we can run multiple analyses on them without having to read them all the times


# tokenizes the corpus using lower level `stylo` function
my_corpus <- stylo::load.corpus.and.parse(corpus.dir = "corpus/",
                             corpus.lang = "English",
                             features = "w", # w = words
                             ngram.size = 1) # single words


# first 20 words of the first document
my_corpus[[1]][1:20]

# names of the files
names(my_corpus)

# Now everything is ready to run stylo on the texts that we have saved in the R list
# We can  call stylo by deactivating the GUI, and setting all the features via R code

results_stylo <- stylo(gui = FALSE, 
                       corpus.lang="English", 
                       analysis.type="CA", 
                       mfw.min=200, 
                       mfw.max=200,
                       distance.measure="dist.wurzburg",
                       parsed.corpus = my_corpus)
# Note: the results of the analysis have been saved in a variable called "stylo_results"

# Explore
View(results_stylo$distance.table)
# Note: the "$" simbol is used to see the sub-section in a structured variable

# see the name of the texts in the distance table
rownames(results_stylo$distance.table)

# see a portion of the distance table
# for example the one of the first text in our selection
results_stylo$distance.table[1,]

# which one is the "closest" text?
sort(results_stylo$distance.table[1,])

# see a table with the frequency of all words
View(results_stylo$frequencies.0.culling)
# rows are the texts, columns the words

# produce a list of the most frequent words
colnames(results_stylo$frequencies.0.culling)[1:200]

# which is the position in the table of the word "lights"
lights_position <- which(colnames(results_stylo$frequencies.0.culling) == "lights")

# which author uses "lights" more frequently?
sort(results_stylo$frequencies.0.culling[,lights_position], decreasing = T)


## some simple datasets are coming with `stylo` as well!
data("galbraith")

galbraith[,1:5]
stylo(gui=F,frequencies = galbraith)

## obviously, when you have a prepared table with frequencies, running any analysis becomes simple! 

### Let's try taking stylo output for your own analysis ### 

## we can recreate the TSNE visualization having frequencies on our own! 

## run stylo with consequtive samples (each novel will be chunked, each chunk would become a separate row of frequencies)

res_sampled <- stylo(gui=F,
                     corpus.dir = "corpus/",
                     sampling="normal.sampling",
                     sample.size=5000,
                     mfw.min=200,
                     mfw.max=200)
                     
z_scores <- as.matrix(res_sampled$table.with.all.zscores)
z_scores[1:15,1:5]

# load libraries for tSNE and visualizing

# install.packages("Rtsne")
# install.packages("dbscan")
# install.packages("plotly")

library(tidyverse)
library(Rtsne)
library(dbscan)
library(plotly)

set.seed(1989) # fix random seed for reproducibility

tsne <- Rtsne(z_scores, dims = 2, perplexity = 30) # fit tSNE model

# Create a data frame for plotting
tsne_df <- tibble(
  X = tsne$Y[, 1],
  Y = tsne$Y[, 2],
  label = rownames(z_scores)) %>%
  mutate(author=str_extract(label,"^\\w*?_")) # get only author's name (string before the first underscore)

# Interactive plot with `plotly` package
plot_ly(tsne_df, # data frame
        x = ~X,  # which column to X-axis
        y = ~Y,  # which to Y
        type = 'scatter', # visualization type
        color=~author,  # color by `author` column
        mode = 'markers', 
        text = ~label, # define  a text string associated with with each point 
        hoverinfo = 'text') # hover from 'text' parameter




### Your Turn (1) - start

# Run the same analyses on a different corpus (e.g. in a different language)
# see for example this one for German: https://github.com/computationalstylistics/68_german_novels
# or this one for French: https://github.com/COST-ELTeC/ELTeC-fra/tree/master (for easy analysis, use the files in the "plain1" folder)
# or this one for Italian: https://github.com/COST-ELTeC/ELTeC-ita/tree/master (for easy analysis, use the files in the "orig" folder)

# remember that once downloaded the files, you need to change the working directory
# to a directory that contains "corpus" in itself (or specify a new corpus directory explicitly)

### Your Turn (1) - end


### 2. Zeta Analysis
library(tidyverse)
# find the texts written by one author (e.g. Woolf)
Chosen_texts <- which(str_detect(names(my_corpus), "Woolf"))
Chosen_texts

# We use the "oppose" function, still in the "stylo" package,
# that looks for the most distinctive words.
# The method it uses is known as "Zeta Analysis"
# The corpus should be divided in two parts:
# A "primary set" where we have the texts of interest;
# A "secondary set" to be compared with

# Our primary set are the texts by Woolf
primary_set <- my_corpus[Chosen_texts]
# Our secondary set are the texts by all the others
secondary_set <- my_corpus[-Chosen_texts]

# now everything is ready to run an "oppose" analysis
oppose(primary.corpus = primary_set, secondary.corpus = secondary_set)

# of course you can save everything 
z_res <- oppose(primary.corpus = primary_set, 
                     secondary.corpus = secondary_set,gui = F)

z_res$words.preferred[1:20]
z_res$words.preferred.scores[1:20]

# let's redraw the visualization! 


## make a data frame with preferred words (Woolf)
pref_df <- tibble(word=z_res$words.preferred[1:20],
                  score=z_res$words.preferred.scores[1:20],
                  corpus="Woolf") %>% mutate(rank=row_number())

pref_df

## make a data frame with avoided words (Others)
avoid_df <- tibble(word=z_res$words.avoided[1:20],
                  score=z_res$words.avoided.scores[1:20],
                  corpus="Others") %>% mutate(rank=row_number())

df_combined <- bind_rows(pref_df,avoid_df)

my_palette <- c("gold","purple") 
ggplot(df_combined, aes(score, reorder(rank,-rank))) + # NB reordered rank 
  # geometries (columns + points + text)
  geom_col(width=0.2,aes(fill=corpus)) + 
  geom_point(size=3,aes(color=corpus)) +
  geom_text(aes(label=word),nudge_y = 0.5) +
  facet_wrap(~corpus,scales = "free_x") +
  # appearance tweaks
  theme_minimal() +
  scale_color_manual(values=my_palette) +
  scale_fill_manual(values=my_palette) +
  labs(y="Word rank",x="Zeta scores") +
  guides(fill="none")

# in the graphical interface, you can leave things as they are
# please choose the "Words" visualization

### Your Turn (2) - start

# Run the same analyses on a different corpus
# or with different target author(s)


### Your Turn (2) - end




###### Appendix. `seetrees` and making sense of `stylo` output

#install.packages("devtools")
#devtools::install_github("perechen/seetrees")

library(stylo)
library(seetrees)

data(lee) 
lee[,1:5]

## run basic stylo on Harper Lee dataset
stylo_res <- stylo(gui = F,frequencies = lee)


## cut the tree, and associate features with clusters
view_tree(stylo_res,k = 2,right_margin = 12,color_leaves = T)


## view_scores()
## see the highest z-score distribution in an author or a class
view_scores(stylo_res, target_text = "Faulkner_Absalom_1936",top=15)
view_scores(stylo_res, target_class = "Faulkner",top=15)

## compare_scores()
## compare profiles of two documents
compare_scores(stylo_res,
               source_text = "Capote_Blood_1966",
               target_text = "HarperLee_Mockingbird_1960")

compare_scores(stylo_res,
               source_text = "Capote_Blood_1966",
               target_text = "Capote_Breakfast_1958")

compare_scores(stylo_res,
               source_text = "Capote_Blood_1966",
               target_text = "HarperLee_Mockingbird_1960",
               type = "diff")

compare_scores(stylo_res,
               source_text = "Faulkner_Absalom_1936",
               target_text = "Faulkner_Sound_1929",
               type = "diff",top_diff = 15)

## view distance distribution! 
view_distances(stylo_res,group = F)
view_distances(stylo_res,group = T)

## what is going on with that long right tail? let's check within-author distances
view_distances(stylo_res,group = T,author = "Capote")
view_distances(stylo_res,group = T,author = "HarperLee")
view_distances(stylo_res,group = T,author = "McCullers")
view_distances(stylo_res,group = T,author = "OConnor")
view_distances(stylo_res,group = T,author = "Welty")
view_distances(stylo_res,group = T,author = "Glasgow")
view_distances(stylo_res,group = T,author = "Styron")
view_distances(stylo_res,group = T,author = "Faulkner")


### YOUR TURN ### 

## analyse another dataset to compare distance distribution
data("galbraith")
View(galbraith)

gal = stylo()

view_distances()

### END OF YOUR TURN ###