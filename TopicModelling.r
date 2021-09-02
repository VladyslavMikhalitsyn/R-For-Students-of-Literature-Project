#topic modelling

#Wells

library(tidyverse)
library(gutenbergr)
library(tidytext)
library(topicmodels)
library(quanteda) # used to create the structure
library(stm) # structured topic modeling
library(dplyr)
library(readr)

install.packages(c("stm", "quanteda", "topicmodels"))

wells_total_tm <- wells_total %>%
  mutate(line = row_number()) %>% # keep track of which line each word is coming from
  unnest_tokens(word, text) %>% # one word per row instead of one line per row
  anti_join(stop_words) # remove stop words


wells_tf_idf <- wells_total_tm %>%
  count(title, word, sort = TRUE) %>% # how many times each word was used in each novel
  bind_tf_idf(word, title, n) %>% # column, the book, counts
  arrange(-tf_idf) %>% # arrange in a descending order
  group_by(title) %>% # group by the book
  top_n(10) %>% #we take the top 
  ungroup


wells_tf_idf %>%
  mutate(word = reorder_within(word, tf_idf, title)) %>% # show words in the same order as tf_idf
  ggplot(aes(word, tf_idf, fill = title)) + #word x axis, tf_idf y axis, color tied to book
  geom_col(alpha = 0.8, show.legend = FALSE) + #barplot, transparency 0.8, no legend
  facet_wrap(~ title, scales = "free", ncol = 5) + #facet according to title, free scales, 3 cols
  scale_x_reordered() +
  coord_flip() + #flip plots so that they are horizontal
  theme(strip.text=element_text(size=11)) + # indicating theme for better aesthetics
  labs(x = NULL, y = "tf-idf", # labels
       title = "Highest tf-idf words in selected Wells' novels")


wells_dfm <- wells_total_tm %>%
  count(title, word, sort = TRUE) %>% # word frequency per book, descending order
  cast_dfm(title, word, n) # create document frequency matrix (quanteda)


wells_sparse <- wells_total_tm %>%
  count(title, word, sort = TRUE) %>%
  cast_sparse(title, word, n)

topic_model_wells <- stm(wells_dfm, K = 9, 
                   verbose = FALSE, init.type = "Spectral")

td_beta_wells <- tidy(topic_model_wells) # tidy function of broom package


td_beta_wells %>%
  group_by(topic) %>% 
  top_n(10, beta) %>% # top words per topic
  ungroup() %>% 
  mutate(topic = paste0("Topic ", topic),
         term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(term, beta, fill = as.factor(topic))) +
  geom_col(alpha = 0.8, show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free_y") +
  coord_flip() +
  scale_x_reordered() +
  labs(x = NULL, y = expression(beta),
       title = "Highest word probabilities for each topic",
       subtitle = "Different words are associated with different topics")



# In which documents (novels) can the topics be found?

td_gamma_wells <- tidy(topic_model_wells, matrix = "gamma",                     
                 document_names = rownames(wells_dfm))

td_gamma_wells %>% 
  ggplot(aes(gamma, fill = as.factor(topic))) + # histogram so only one variable needed
  geom_histogram(alpha = 0.8, show.legend = FALSE) + 
  facet_wrap(~ topic, ncol = 3) + # facet wrap according to topic
  labs(title = "Distribution of document probabilities for each topic",
       subtitle = "Each topic is associated with 1-3 novels",
       y = "Number of novels", x = expression(gamma))




#### Tidymodels package####


foundation <- read_tsv("Asimov.txt")
foundation <- foundation %>%
  mutate(text = gsub("\u2019", "'", text)) %>%
  mutate(id = row_number()) %>%
  mutate(title = "Foundation")

foundation <- foundation %>% #pick "Foundation" the most popular novel, do the same for other authors
  filter(id <= 5860)

odyssey <- read_tsv("Clarke.txt")
odyssey <- odyssey %>%
  mutate(text = gsub("\u2019", "'", text)) %>%
  mutate(id = row_number()) %>%
  mutate(title = "2001: A Space Odyssey")

odyssey <- odyssey %>% 
  filter(between(id, 9035, 12880))

stranger <- read_tsv("Heinlein.txt")
stranger <- stranger %>%
  mutate(text = gsub("\u2019", "'", text)) %>%
  mutate(id = row_number()) %>%
  mutate(title = "A Stranger in a Strange Land")

stranger <- stranger %>% 
  filter(between(id, 3085, 8976))

timemachine <- wells %>%
  filter(ID <= 3074) %>%
  mutate(id = row_number())

timemachine <- within(timemachine, rm(gutenberg_id, ID))


#### Tidymodels package####
library(topicmodels)

titles <- c("Dracula", 
            "The War of the Worlds",
            "Tess of the d'Urbervilles: A Pure Woman", 
            "Elizabeth and Her German Garden")

books <- gutenberg_works(title %in% titles) %>%
  gutenberg_download(meta_fields = "title")

??`%in%`
# divide into documents, each representing one chapter

tidym <- rbind(timemachine, odyssey, stranger, foundation)

# split into words
tidym <- tidym %>%
  unnest_tokens(word, text)

# find document-word counts
word_counts_tidym <- tidym %>%
  anti_join(stop_words) %>%
  count(title, word, sort = TRUE) %>%
  ungroup()

word_counts

# document term matrix of all chapters
tidym_dtm <- word_counts_tidym %>%
  cast_dtm(title, word, n)

tidym_dtm


# running the topic model
chapters_lda_tidym <- LDA(tidym_dtm, k = 4, control = list(seed = 1234)) # data, num. of topics, seed
chapters_lda_tidym

chapter_topics_tidym <- tidy(chapters_lda_tidym, matrix = "beta")
chapter_topics_tidym

top_terms_tm <- chapter_topics_tidym %>%
  group_by(topic) %>%
  slice_max(beta, n = 10) %>%
  ungroup() %>%
  arrange(topic, -beta)

top_terms_tm
str(top_terms_tm)
top_terms_tm %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(beta, term, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  scale_y_reordered()
#2001: A Space Odyssey and The Time Machine are clearly prevalent in different topics


###General Topic Modelling Across Three Authors
#Asimov

a <- read_tsv("PreludeToFoundationIsaacAsimov_Prelude-to-Foundation-Isaac-Asimov.txt") %>%
  mutate(id = row_number()) %>%
  mutate(title = "Prelude to Foundation")

  names(a)<-str_replace_all(names(a), c(" " = "." , "," = "" ))

  names(a)[1] <- 'text'
  
    mutate(
      title = case_when(
        id <= 2458 ~ "Foundation",
        id <= 4467 & id >= 2458 ~ "Foundation and Empire",
        id >= 4468 ~ "Second Foundation"
      )
    ) 
  

#````````````````````````````

b <- read_tsv("AsimovTheFoundation_201705_Asimov_the_foundation.txt")

b <- b %>%
  mutate(id = row_number())
#since this file is a trilogy, we should specify the titles in the dataframe

names(b)<-str_replace_all(names(b), c(" " = "." , "," = "" ))

names(b)[1] <- 'text'


b <- b %>% #dplyr version of If > then #NOT IN USE, JUST FOR REFERENCE
  mutate(
    title = case_when(
      id <= 2458 ~ "Foundation",
      id <= 4467 & id >= 2458 ~ "Foundation and Empire",
      id >= 4468 ~ "Second Foundation"
    )
  ) 

names(b)<-str_replace_all(names(b), c(" " = "." , "," = "" ))

names(b)[1] <- 'text'


c <- read_tsv("AsimovIsaacFoundation6FoundationAndEarth_Asimov_-Isaac-Foundation-6-Foundation-and-Earth.txt") %>%
  mutate(id = row_number()) %>%
  mutate(title = "Foundation and Earth")

names(c)<-str_replace_all(names(c), c(" " = "." , "," = "" ))

names(c)[1] <- 'text'

d <- read_tsv("FoundationsEdgeIsaacAsimov_Foundation_s-Edge-Isaac-Asimov.txt") %>%
  mutate(id = row_number()) %>%
  mutate(title = "Foundation's Edge")


names(d)<-str_replace_all(names(d), c(" " = "." , "," = "" ))

names(d)[1] <- 'text'


e <- read_tsv("Ayzek_Azimov__The_Gods_Themselves.txt") %>%
  mutate(id = row_number()) %>%
  mutate(title = "The Gods Themselves")

names(e)<-str_replace_all(names(e), c(" " = "." , "," = "" ))

names(e)[1] <- 'text'

f <- read_tsv("I_-Robot-by-Isaac-Asimov.txt") %>%
  mutate(id = row_number()) %>%
  mutate(title = "I, Robot")

names(f)<-str_replace_all(names(f), c(" " = "." , "," = "" ))

names(f)[1] <- 'text'

h <- read_tsv("The_End_of_Eternity_-_Isaac_Asimov.txt") %>%
  mutate(id = row_number()) %>%
  mutate(title = "The End of Eternity")

names(h)<-str_replace_all(names(h), c(" " = "." , "," = "" ))

names(h)[1] <- 'text'

i <- read_tsv("The-Caves-of-Steel-Isaac-Asimov.txt") %>%
  mutate(id = row_number()) %>%
  mutate(title = "The Caves of Steel")

names(i)<-str_replace_all(names(i), c(" " = "." , "," = "" ))

names(i)[1] <- 'text'

asimov_total <- rbind(a, b, c, d, e, f, h, i)

asimov_total <- asimov_total %>% #remove text columns with *page* numbers to be more clear, credit https://stackoverflow.com/questions/51107901/how-do-i-filter-a-range-of-numbers-in-r/51108210
  filter(!asimov_total$text %in% c(1:9999999)) %>% #because of it messing up with ids, we should do it again(titles should be okay)
  mutate(id = row_number())



#Heinlein


heinlein_total <- read_tsv("Heinlein.txt") %>%
  mutate(id = row_number())

heinlein_total <- heinlein_total %>% #dplyr version of If > then #NOT IN USE, JUST FOR REFERENCE
  mutate(
    title = case_when(
      id <= 1210 ~ "Door Into Summer",
      id <=  2308 & id >= 1211 ~ "Double Star",
      id <=  8259 & id >= 2309 ~ "Stranger in a Strange Land",
      id <=  11065 & id >= 8260 ~ "Have Space Suit?Will Travel",
      id <=  13303 & id >= 11066 ~ "Starship Troopers",
      id <=  17015 & id >= 13304 ~ "Friday",
      id <=  23556 & id >= 17016 ~ "Time Enough for Love",
      id <=  27279 & id >= 23557 ~ "The Cat Who Walked Through Walls",
      id <=  30603 & id >= 27280 ~ "Luna is a harsh mistress",
      id >= 30604 ~ "The Puppet Masters "
    )
  ) 

#Clarke

clarke_total <- read_tsv("ArthurC.Clarke.txt") %>%
  mutate(id = row_number())

clarke_total <- clarke_total %>%
  mutate(
    title = case_when(
      id <=  1487 ~ "2001: A Space Odyssey",
      id <=  3914 & id >= 1488 ~ "2010: Odyssey Two",
      id <=  4874 & id >= 3915 ~ "2061: Odyssey Three",
      id <=  5857 & id >= 4875 ~ "3001 The Final Odyssey",      
      id <=  7167 & id >= 5858 ~ "The City and the Stars", 
      id <=  9246 & id >= 7168 ~ "The Fountains of Paradise",
      id <=  11085 & id >= 9247 ~ "Songs of Distant Earth",     
      id <=  12955 & id >= 11086 ~ "A Fall of Moondust",      
      id <=  14493 & id >= 12956 ~ "Rendezvous with Rama",      
      id >= 14494 ~ "Childhood's End"
    )
  )

wells_total <- wells_total %>% 
  select(-gutenberg_id, -ID) %>%
  mutate(id = row)

wells_total <- wells_total[, c(1,3,2)] #reorder columns #credit http://www.sthda.com/english/wiki/reordering-data-frame-columns-in-r
  

books_total <- rbind(wells_total, clarke_total, asimov_total, heinlein_total) %>%
  mutate(id = row_number())


list2_table <- books_byword %>%
  group_by(author) %>%
  filter(word == list2) %>%
  count(word)

list1 <- list("space", "spaceship", "ship", "beam", "engine", "mechanism")

list2 <- list("human", "nature", "water", "tree", "plant", "creature")

clarke - 367
wells - 72
Asimov - 267
Heinlein - 182

2
clarke - 169
wells - 109
asimov - 209
heinlein - 240








