## here we will use Lana Del Rey's lyrics for quick keyword analysis
## and hopefully learn how to wrangle / read texts + and handle metadata

## install.packages("tidyverse")
## install.packages("tidytext")
## install.packages("tidylo")
library(tidyverse)
library(tidytext)
library(tidylo)

## read the metadata file
ldr <- read_csv("corpus_LDR/ldr_main.csv") %>% 
  filter(!str_detect(title,"Remix")) # filter remixes

ldr

## let's write a function that we can reuse later
## general syntax: my_function <- function(x) {here we do thing with 'x' and it returns results}

process_ldr <- function(path,trim_chorus=F) {
  ## read lines
  t <- readLines(path)
  
  ## determine where are different sections of the song
  br <- c(which(str_detect(t,"^\\[")),length(t))
  
  ## if decided, remove duplicated choruses
  if(trim_chorus) {
    
    ## chorus
    chorus <- which(str_detect(t, "\\[Chorus\\]"))
    
    dup_chorus <- which(br %in% chorus[-1]) # detect which are choruses, but leave one out
    
    lines_to_remove <- NULL # a variable that will hold line indices
    
    for(d in dup_chorus) {
      # take range of lines from first mention of chorus to the next song section
      ch <- br[d]:(br[d+1]-1) 
      lines_to_remove <- c(lines_to_remove,ch) # save these line numbers
    }
    
    # remove from main body
    t <- t[-c(br, lines_to_remove)] 
    
  } else {
    
    t <- t[-br]
    
  } # end of if/else condition
  
  t <- paste(t,collapse="\n")
  return(t)
} # end of function definition


## an example of usage
process_ldr(path = ldr$path[2],trim_chorus = T) %>% cat()

## apply is like a loop - applies a function over a set of something (like a vector that holds paths)
ldr_texts <- lapply(ldr$path, process_ldr, trim_chorus=T)

## now we can add texts to our table, having 1 song = 1 row
ldr_df <- ldr %>% mutate(lyrics=ldr_texts %>% unlist())


ldr_df
##
## we will use weighted log-odds approach to keywords (Monroe et al 2008), implemented in R by Julia Silge and others https://juliasilge.github.io/tidylo/
##

# first, tokenize with 'unnest_tokens'
# we will get a 'long' tokenized table
ldr_tidy <- ldr_df %>%
  unnest_tokens(word,lyrics,token="words") %>%
  filter(!is.na(word))

ldr_tidy %>% select(id,title, word)

## now count words in each album. it's easy when all info is in the table!
ldr_counts <- ldr_tidy %>% 
  count(album, word, sort=T)

ldr_counts

## use 'bind_log_odds' function
ldr_odds <- ldr_counts %>%
  bind_log_odds(album,word,n)

ldr_odds %>%
  arrange(-log_odds_weighted)

album_order <- ldr$album %>% unique() # already arranged by year in our main table


## do some data preparation / filtering
ldr_filt <- ldr_odds %>%
  group_by(album) %>%
  slice_max(log_odds_weighted, n = 20) %>% # takes top 10 values per group (album)
  ungroup() %>%
  mutate(word = reorder(word, log_odds_weighted), # reorder bigrams to plot nicely
         album=factor(album,levels=album_order)) #%>% # reorder albums so they would follow a chronological order

## plot with ggplot
ldr_filt %>% 
  ggplot(aes(log_odds_weighted, word, fill = album)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(vars(album), scales = "free") +
  labs(y = NULL)

## the same but word cloud 
library(ggwordcloud)

ldr_filt %>%
  ggplot(
    aes(label = word, size = log_odds_weighted)) +
  geom_text_wordcloud_area(aes(color=album)) +
  scale_size_area(max_size = 8) +
  theme_minimal() +
  facet_wrap(~album)

## wordcloud with all words from LDR
ldr_tidy %>% 
  count(word,sort=T) %>%  # count again, not grouped by album now
  anti_join(stop_words,by="word") %>% # remove default stop word list (be mindful)
  top_n(200) %>% 
  ggplot(
    aes(label = word, size = n,color=n)) +
  geom_text_wordcloud_area() +
  scale_size_area(max_size = 20) +
  theme_minimal() + 
  scale_color_gradient(low = "pink", high = "red")


### Contrastive analysis (with quanteda)

## Here we will compare LDR's song lyrics with her poetry collection

# install.packages("quanteda")
# install.packages("textstats")
library(quanteda)
library(quanteda.textstats)

## Lana's lyrics
ldr_lyrics <- ldr_df$lyrics %>% 
  str_remove_all("\n") %>%  
  paste(collapse = " ")

## Lana's poetry
ldr_poetry <- read_lines("corpus_LDR/VBBOTG.txt") %>% 
  str_remove_all("\n") %>% 
  paste(collapse = " ") # Violet Bent Backwards Over the Grass (2020)

# make a "corpus" object from quanteda
ldr_corp <- corpus(c(groupA=ldr_poetry,
                     groupB=ldr_lyrics),
                   docnames = c("LDR Poetry", "LDR Lyrics"))

ldr_corp

## tokenization
ldr_tokens <- tokens(ldr_corp,remove_punct = T,remove_symbols = T,remove_numbers = T)
## function to make Document-Feature Matrix
ldr_dfm <- dfm(ldr_tokens)
ldr_dfm

## calculate keyness
ldr_keys <- textstat_keyness(ldr_dfm,target = "LDR Poetry",measure="chi2")
ldr_keys

## subset for plotting ,annotate with groups
top_w <- bind_rows(ldr_keys %>% head(20) %>% mutate(corpus="LDR Poetry",
                                                    rank=row_number()), 
                   ldr_keys %>% tail(20) %>% mutate(corpus="LDR Lyrics",
                                                    rank=rev(row_number())))  # here , i am taking 20 values from the tail of the dataframe: because they are keywords for the "reference" corpus (i.e. not distinctive for the "target"). all is relative

as_tibble(top_w)


my_palette <- c("#E6C28B","#3B7A57") 

ggplot(top_w, aes(chi2,reorder(rank,-rank))) + # NB reordered rank 
  # geometries (columns + points + text)
  geom_col(width=0.2,aes(fill=corpus)) + 
  geom_point(size=3,aes(color=corpus)) +
  geom_text(aes(label=feature),nudge_y = 0.5) +
  facet_wrap(~corpus,scales = "free_x") +
  # appearance tweaks
  theme_minimal() +
  scale_color_manual(values=my_palette) +
  scale_fill_manual(values=my_palette) +
  labs(y=NULL,x="Chi-squared")

### YOUR TURN ###

# Explore quanteda keyness measures, try different ones
# Can you calculate weighted log-odds for 






