library(httr)
library(jsonlite)
library(tidyverse)
library(rvest)
library(xml2)

search_artist("Del Rey")
token <- "AkMJ8vBuxPnJDqwcvTF4qMP-Ja_E9uWPm42osPbwYnyWBVYPLLhRflSwhvRvZjhK"

# Replace with actual artist ID (Lana Del Rey's ID is 166058)
ids=NULL
titles=NULL

page=1
for(p in 1:1000000) {

res <- GET(
  url = "https://api.genius.com/artists/15740/songs",
  add_headers(Authorization = paste("Bearer", token)),
  query = list(per_page=50, page=page)
)

if(is.null(res$content)) {
 break 
}

page = page+1
content <- content(res)


for(i in 1:length(content$response$songs)) {

  id <- content$response$songs[[i]]$id
  title <- content$response$songs[[i]]$title
  
  ids <- c(ids,id)
  titles <- c(titles,title)

}
}

ldr_df <- tibble(id=ids,title=titles)

write_csv(ldr_df,"ldr_meta.csv",quote = "all")

alb <- vector(length=nrow(ldr_df))
date <- vector(length=nrow(ldr_df))

for(i in 1:nrow(ldr_df)) {
  
  res <- GET(
    url = paste0("https://api.genius.com/songs/",ldr_df$id[i]),
    add_headers(Authorization = paste("Bearer", token))
  )
  
  content <- content(res)
  if(is.null(content$response$song$album$name)) {
    a <- NA
  } else {
    a <- content$response$song$album$name
  }
  alb[i] <- a
  
  if(is.null(content$response$song$album$release_date_for_display)) {
    d <- NA
  } else {
    d <- content$response$song$album$release_date_for_display
  }
  
  date[i] <- d
  cat(content$response$song$title, "\t", d, "\t",  a,"\n")
  
  
  
  
  
}

ldr_df <- ldr_df %>% mutate(album = alb,release=date) 

write_csv(ldr_df,"ldr_meta.csv",quote = "all")

ldr_df <- read_csv("ldr_meta.csv") %>% mutate(year=as.integer(str_extract(release, "[0-9]{4}")))
ldr_df
albums <- ldr_df %>% filter(!is.na(album)) %>% count(album) 

a <- c("Born to Die - The Paradise Edition (Box Set)",
       "Ultraviolence (Japanese Deluxe Version)",
       "Honeymoon",
       "Lust for Life", 
       "Norman Fucking Rockwell!",
       "Chemtrails Over the Country Club",
       "Blue Banisters",
       "Did you know that there’s a tunnel under Ocean Blvd")

ldr_main <- ldr_df %>% filter(album %in% a) %>% arrange(year,album)


urls <- vector(length = nrow(ldr_main))

for(i in 1:nrow(ldr_main)) {

res <- GET(
  url = paste0("https://api.genius.com/songs/",ldr_main$id[i]),
  add_headers(Authorization = paste("Bearer", token))
)
content <- content(res)
urls[i] <- content$response$song$url

cat(ldr_main$id[i], "\t", ldr_main$title[i], "\n")  

}

ldr_main<-ldr_main %>% 
  mutate(url=urls)# %>% 
  
write_csv(ldr_main,"ldr_main.csv",quote = "all")

## for albums
content$response$song$album$name
content$response$song$album$release_date_for_display
s <- content$response$songs

# Function to fetch lyrics from a Genius URL
get_genius_lyrics <- function(url) {
  page <- read_html(url)
  
  # CSS selector targeting the lyrics container segments
  lyrics <- page %>%
    html_nodes("div[class^='Lyrics__Container']") %>%
    html_text2(preserve_nbsp = T) #%>%            # Get clean text
    paste(collapse = "\n\n")    # Join separate <div> blocks with blank lines
  
  return(lyrics)
}

get_genius_lyrics2 <- function(url) {
  page <- read_html(url)
  
  # Target the lyrics containers
  lyrics_nodes <- html_nodes(page, xpath = "//div[contains(@class,'Lyrics__Container')]")
  
  # Insert newline paragraphs before each <br>, then remove <br>
  xml_find_all(lyrics_nodes, ".//br") %>% xml_add_sibling("p", "\n")
  xml_find_all(lyrics_nodes, ".//br") %>% xml_remove()
  
  # Extract text now with proper newlines
  lyrics <- html_text(lyrics_nodes, trim = TRUE)
  lyrics <- paste(lyrics, collapse = "\n\n")
  return(lyrics)
}

paths <- vector(length = nrow(ldr_main))

for(i in 1:nrow(ldr_main)) {
  url <- ldr_main$url[i]
  lyrics_text <- get_genius_lyrics2(url)
  t <- lyrics_text %>% str_split("\n") %>% unlist()
  no <- which(str_detect(t, "Read More"))
  if(1 %in% no) {
    no <- no[-1]
  }
  t <- t[-c(1,no)]
  
  paths[i] <- paste0("corpus_LDR/texts/", ldr_main$year[i],"_", str_replace_all(ldr_main$title[i], " ", "_"), ".txt")
  write_lines(t,file = paths[i])
}

ldr_main %>% mutate(path=paths) %>% write_csv("corpus_LDR/ldr_main.csv",quote = "all")


##### poetry book to text
library(epubr)


## read epub
epub <- epubr::epub("corpus_LDR/Lana Del Rey - Violet Bent Backwards Over the Grass.epub")
## nice dataframe
vbb_df <- epub$data[[1]]

## some quick trimming of front/end matter 
vbb_df <- vbb_df[-c(1:4,77:87),]

## write it as a plain text
lapply(vbb_df$text, function(x) {write_lines(x,"corpus_LDR/VBBOTG.txt",sep="\n\n",append = T)})
