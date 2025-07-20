### Welcome!

# A yellow warning might appear above
# No worries: you can close it for the moment

# This is an R script file, adjusted by Artjoms (based on Simone's work)
# Everything written after an hashtag is a comment
# Everything else is R code
# To activate the code, place the cursor on the corresponding line
# (or highlight multiple lines/pieces of code) 
# ...and press Ctrl+Enter (or Cmd+Enter for Mac)
# (the command will be automatically copy/pasted & executed in the console)

### 1. Basic functions

# print something to the screen
print("Hello world!")

# get info on a function
?print # or help(print)

# the "cat" function
cat("Hello world!")
help(cat)

# concatenate and print
cat("The cat is", "on the table")
cat("The cat is", "on the table", "sleeping")

# add arguments
cat("The cat is", "on the table", "sleeping", sep = "\n")
cat("The cat is", "on the table", "sleeping", sep = " happy ")

### Your Turn (1) - start

# use the cat function to print this message:
# "The cat is with Simone on the table with Simone sleeping"
# Separate by newline to make it look like a verse!  
# you can complete the code here below:
cat("The cat is", "on the table", "sleeping", sep = "")

### Your Turn (1) - end


### 2. Creating variables

# numbers
my_number <- 1
my_number

# strings of text
my_string <- "to be or not to be"
my_string

## PS: print(my_string) is the same as just entering the variable name and running it

# logical
that_is_true <- TRUE # T for short
that_is_false <- FALSE # F for short

# == means "is equal"?
TRUE == FALSE
T == T
T != T # != 'is not equal?'
F == F

1 == 2
my_number == my_string

!T # ! = "NOT this -> "
!T == F # Is a Not True equal False? 

# logicals map to 0 / 1
as.numeric(FALSE)
as.numeric(TRUE)

TRUE == 1

# vectors
my_first_vector <- c(1,2,3,4,5)
my_first_vector

# PS! usually there are many ways to do the same thing
v1 <- 1:5 # colon sets a "range" of integers between 1 and 5
v2 <- seq(from=1,to=5,by=1) # explicit sequence function

v1
v2

### YOUR TURN! ###

# make a sequence of numbers from 0 to 100 that increase by 13 and save it to a variable "v3"
# or make a sequence of your choice! 


### END OF YOUR TURN ###

# you can also plot a vector in the "plots" panel
plot(my_first_vector)

# vectors (of strings)
my_second_vector <- c("to", "be", "or", "not", "to", "be")
my_second_vector



# lists
my_list <- list(1:5, c("to", "be", "or", "not", "to", "be"))
my_list

# tip: you can get the same by writing
my_list <- list(my_first_vector, my_second_vector)
my_list


# dataframes
my_df <- data.frame(author = c("Shakespeare", "Dante", "Cervantes", "Pynchon"), 
                    nationality = c("English", "Italian", "Spanish", "American"))
View(my_df)

# or view in the console
my_df

### Your Turn (2) - start

# create a new dataframe with other authors
# If you're feeling adventurous, add another column that would be a vector of TRUE/FALSE values! what data can it describe?  
# you can complete the code here below:
my_new_df <- data.frame(author = c(),
                        nationality = c())
View(my_new_df) # view opens new tab and renders the object in RStudio, alternatively you can just print my_new_df in the console

### Your Turn (2) - end


### 3. Accessing variables

# vector subsets
# 1. access values
my_first_vector[1]
my_second_vector[1]
my_second_vector[4]

# 2. access values based on other vectors
my_second_vector[1:4]
my_second_vector[c(1,4)]
my_first_vector[c(T,F,F,F,T)] # check out how logicals behave!

# remove values
my_first_vector[-1]
my_first_vector[-c(1,4)]

# subset lists
my_list[[1]] # [[]] access list items, 
my_list[[1]][4] # [] - vector elements
my_list[[2]][4]
my_list[[2]][1:3]

# dataframes[column_number,row_number]
# dataframes$column_name
my_df[,1] 
my_df$author # the same, but more meaningful 
my_df[,2]
my_df$nationality # the same!!
my_df[1:3,1] 
my_df$author[1:3] # the same!!

# rows
my_df[1,]
my_df[3,]

# NB
# sometimes vectors can have names! a single row of a column is typically a "named" vector
named_vector<-1:5
names(named_vector) <- c("first", "second","third","fourth", "fifth")
named_vector

# accessing variables in a meaningful way
my_df$author == "Dante"

is_dante <- my_df$author == "Dante"

my_df$nationality[is_dante]

# you can ask which value equals to "Dante" explicitly with which()
# note how the logic of the subsetting did slightly change
which(my_df$author == "Dante")

which_is_dante <- which(my_df$author == "Dante")

my_df$nationality[which_is_dante]

### Your Turn (3) - start

# find the author who has "Spanish" nationality
is_spanish <- # ...
my_df$author[] # ... 

### Your Turn (3) - end


### 4. Manipulating variables
my_first_vector+1
my_first_vector[2]+1
my_second_vector+1 # this produces an error!!

# manipulating strings
paste(my_string, "?",sep = "") # paste strings together

strsplit(my_string,split =  " ") # split by a character, returns a list
strsplit(my_string, " ")[[1]]

table(strsplit(my_string, " ")) # tabulate / tally values

sort(table(strsplit(my_string, " "))) # sort by frequency
sort(table(strsplit(my_string, " ")),decreasing = T) # sort by frequency (most frequent first) 

### Your Turn (4) - start

# use the "strsplit" function to split another string by another character
another_string <- "i_am_another_string_of_text"
strsplit()

### Your Turn (4) - end


### 6. Making loops

# basic loop
for(my_first_iterator in 1:10){
  
  print(my_first_iterator)
  
}

# if conditions
for(i in 1:10){
  
  if(i >= 5){ # IF statements return *logical* TRUE/FALSE
    print(i)
  }
  
}

# if/else conditions
for(i in 1:10){
  
  if(i == 1){
    
    print(i)
    
  } else {
    
    print("more than one")
  }
}

## iterator can be anything, not just numbers
names <- c("artemis","anna","zooey", "michelle", "yuki")

for(the_name in names) {
  
  print(the_name)
  Sys.sleep(0.5) # adds a delay for coolness 
  
}

## .. but often you will see a numeric representation to be able to access variables in parallel

for(i in 1:length(names)) {
  
  cat(names[i], "\t", my_first_vector[i],"\n")
  Sys.sleep(0.5)
  
}

### Your Turn (6) - start

# write a loop that prints numbers until 4, then prints "more than four"
# tip: use as a model the two loops above


### Your Turn (6) - end


## Pipes and tidyverse 

# install (this should be done just once)
# install.packages("tidyverse")

# load (this should be done every time you start R!)
library(tidyverse)


# this syntax is kind of madness! it's called "function nesting" and is very hard to read
sort(table(strsplit(my_string, " ")[[1]]),decreasing = T)

# instead, tidyverse philosophy uses pipes (%>%) that "pipe" the output down the code
# you can read them left to right, or top to bottom (instead of from inside to outside)
# Shortcut to put a pipe: Ctrl + Shift + M

str_split(my_string, " ") %>% table() %>% sort(decreasing = T)

# Or even more elemental (first row passes down data)
my_string %>% 
  str_split(" ") %>% 
  table() %>% 
  sort()

## Tibbles

## tidyverse also significantly modernizes data.frames (into 'tibbles') 

as_tibble(my_df)

## .. and simplifies syntax for working with them

# for example: find the Italian author in our dataframe of authors
# with base R, you can do like that
my_df[which(my_df$nationality == "Italian"),]

# with tidyverse, you do like
my_df %>% filter(nationality == "Italian")

# it's easier to manipulate data frames as well:
my_df2 <- my_df %>% mutate(genre=c("drama", "poetry", "novel", "novel"))

# do something based on data available 
my_df2 %>% mutate(is_english=ifelse(nationality=="English", TRUE, FALSE))

## Plotting! 
## GG in ggplot stands for Grammar of Graphics
## it's really neat language of step-by-step construction of plots 

# let's use our simple DF with authors and update it with some data
pop <- c(2.7, 0.198,  0.294, 0.092831) # ratings of the most-read work on Goodreads in millions!
birth <- c(1564, 1265,1547,1937) # years of birth

my_df3 <- my_df %>% mutate(popularity=pop, birth_year=birth)
my_df3

# to understand ggplot most easier is to imagine it as layers of instruction that control various elements of the plot: mapping of data to axis, groups to data, graphical elements, labels etc. 

# For most bare-bones plot you need to specify what is going to be on X axis and what is Y axis

ggplot(data = my_df3, mapping = aes(x=birth_year, y=popularity))

# we didn't provide it with any instruction for the graphical elements yet! let's add it with '+"

# note the shorthand use of arguments in ggplot() function
ggplot(my_df3, aes(birth_year,popularity)) + geom_point(size=3)

# want to annotate? 

ggplot(my_df3, aes(birth_year,popularity)) + 
  geom_point(size=3,color="gold") + 
  geom_text(aes(label=author),nudge_y = 0.1)

# want to change labels? 

ggplot(my_df3, aes(birth_year,popularity)) + 
  geom_point(size=3,color="gold") + 
  geom_text(aes(label=author),nudge_y = 0.1) +
  labs(title="Popularity vs. birth year", x="Year of birth", y="Popularity (Goodreads)")

# logarithmic scale for Y-axis?

ggplot(my_df3, aes(birth_year,popularity)) + 
  geom_point(size=3,color="gold") + 
  geom_text(aes(label=author),nudge_y = 0.1) +
  scale_y_log10() +
  labs(title="Popularity vs. birth year", x="Year of birth", y="Popularity (Goodreads)")

# various appearance elements of the plot are controlled by theme() function that is added to the plot the same way as all other layers. There are already pre-defined themes to quickly use

ggplot(my_df3, aes(birth_year,popularity)) + 
  geom_point(size=3,color="gold") + 
  geom_text(aes(label=author),nudge_y = 0.1) +
  scale_y_log10() +
  labs(title="Popularity vs. birth year", x="Year of birth", y="Popularity (Goodreads)") + 
  theme_light(base_size = 14)

# you can still adjust the theme manually! 

ggplot(my_df3, aes(birth_year,popularity)) + 
  geom_point(size=3,color="gold") + 
  geom_text(aes(label=author),nudge_y = 0.1) +
  scale_y_log10() +
  labs(title="Popularity vs. birth year", x="Year of birth", y="Popularity (Goodreads)") + 
  theme_light() + 
  theme(axis.text.x = element_text(size=12))

# want to do the coloring / size of points programmatically based on variables? change mapping in aes()! 
ggplot(my_df3, aes(birth_year,popularity,color=nationality,size=popularity)) + 
  geom_point() + 
  geom_text(aes(label=author),nudge_y = 0.1) +
  scale_y_log10() +
  labs(title="Popularity vs. birth year", x="Year of birth", y="Popularity (Goodreads)") + 
  theme_light() + 
  theme(axis.text.x = element_text(size=12))

# see how the global data mapping applies to all levels of graphics. You can control individual geom_ instead.
ggplot(my_df3, aes(birth_year,popularity)) + 
  geom_point(aes(color=nationality, size=popularity)) + 
  geom_text(aes(label=author),nudge_y = 0.1) +
  scale_y_log10() +
  labs(title="Popularity vs. birth year", x="Year of birth", y="Popularity (Goodreads)") + 
  theme_light() + 
  theme(axis.text.x = element_text(size=12)) +
  xlim(1200,2000)

## tidyverse generally and ggplot in particular expects 'tidy' approach to data. One observation = one row!  
# it is also called "long" tables, as opposite to "wide", where a row can describe several observations.
# document-term matrix is an example of a "long" table.


### YOUR TURN ### 

# Design a toy 'long' table that would hold data on the topic of your interest
# Make a prototype data frame in R 

### END OF YOUR TURN ### 


# PS .base R plots are OK for quick plotting
set.seed(189)
my_norm <- rnorm(100,mean=5,sd=1)
hist(my_norm)


### 8. Cheat sheets
# good practice when you start coding with R is to use cheat sheets
# you can download some from here (or just Google them!)
# https://iqss.github.io/dss-workshops/R/Rintro/base-r-cheat-sheet.pdf
# https://posit.co/resources/cheatsheets/


### 9. ChatGPT
# Large Language Models are very good in writing code!
# You just need to provide clear instructions
# However, never trust them 100% (especially when task is complex): always test the script!
# Example: https://chat.openai.com/share/ef0e31ba-9136-40ab-9e78-6c046de48b78


### ADDENDUM. Reading/writing text files

# printing working directory
getwd()
# setting working directory
setwd("path/to/my_folder")

# read text line by line
my_text <- readLines("corpus/Cbronte_Jane_1847.txt")
head(my_text)

# note that if you change the working dir, you will get an error
setwd("../") # two dots mean "go one folder above the current"
my_text <- readLines("corpus/Cbronte_Jane_1847.txt")


# collapse all text in a single line (separated by the "newline" character)
my_text_collapsed <- paste(my_text, collapse = "\n")
# show its head (by using the "substr" function)
substr(my_text_collapsed, 1, 100)
# elegantly print it
cat(substr(my_text_collapsed, 1, 100))

# write file
cat("The cat is on the table", file = "Cat.txt")

### Your Turn (5) - start

# read another .txt file in the "corpus" folder
# and split it into single words 
my_text <- readLines("corpus/Cbronte_Jane_1847.txt")
strsplit()

### Your Turn (5) - end

