###Topic Modelling General

library(tidyverse)
library(gutenbergr)
library(tidytext)
library(topicmodels)
library(quanteda) # used to create the structure
library(stm) # structured topic modeling
library(dplyr)
library(readr)
library(forcats)
library(ggpubr)
library(scales)

asimov_byword <- asimov_total %>%
  unnest_tokens(word, text) %>%
  mutate(author = "IsaacAsimov") %>%
  mutate(word = gsub("\u2019", "'", word)) %>%  #sometimes apostrophe is substituted with a right quotation mark in the data, which "stop_words" does not include, 
  #resulting it words like "I'm" appearing, this fixes that
  
  anti_join(stop_words)

clarke_byword <- clarke_total %>%
  mutate(author = "ArthurCClark") %>%
  unnest_tokens(word, text) %>%
  mutate(word = gsub("\u2019", "'", word)) %>% 
  anti_join(stop_words)

heinlein_byword <- heinlein_total %>%
  unnest_tokens(word, text) %>%
  mutate(author = "RobertAHeinlein") %>%
  mutate(word = gsub("\u2019", "'", word)) %>% 
  anti_join(stop_words)


wells_byword <- wells_total %>%
  mutate(author = "HGWells") %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words)

books_byword <- rbind(wells_byword, clarke_byword, asimov_byword, heinlein_byword)

books_byword_tf_idf_byauthor <- books_byword %>% 
  count(author, word, sort = TRUE) %>% # how many times each word was used by each author?
  bind_tf_idf(word, author, n) %>% # column, the book, counts
  arrange(-tf_idf) %>% # arrange in a descending order
  group_by(author) %>% # group by the author
  summarize(Mean = mean(tf_idf, na.rm=TRUE)) %>%
  ungroup


books_byword_tf_idf_bytitle <- books_byword %>% 
  count(title, word, sort = TRUE) %>% # how many times each word was used in each novel
  bind_tf_idf(word, title, n) %>% # column, the book, counts
  arrange(-tf_idf) %>% # arrange in a descending order
  group_by(title) %>% # group by the title
  summarize(Mean = mean(tf_idf)) %>%
  ungroup

#graph

books_byword_tf_idf_byauthor_graph <- books_byword_tf_idf_byauthor %>%
  mutate(author = fct_reorder(author, Mean)) %>%
  ggplot( aes(x=author, y=Mean)) +
  geom_bar(stat="identity", fill="#151BCE", alpha=.6, width=.4) +
  coord_flip() +
  xlab("") +
  theme_bw()
books_byword_tf_idf_byauthor_graph

books_byword_tf_idf_bytitle_graph <- books_byword_tf_idf_bytitle %>%
  top_n(10) %>%
  mutate(title = fct_reorder(title, Mean)) %>%
  ggplot( aes(x=title, y=Mean)) +
  geom_bar(stat="identity", fill="#f68060", alpha=.6, width=.4) +
  coord_flip() +
  xlab("") +
  ylab("Mean tf-idf") +
  theme_bw()
books_byword_tf_idf_bytitle_graph

ggarrange(books_byword_tf_idf_byauthor_graph, books_byword_tf_idf_bytitle_graph)

#Surprisingly, Heinlein is a clear outsider according to tf_idf, how do works themselves compare?
#Out of the ten works with most tf_idf, six are H.G. Wells' works, and three of the short stories are in the top four, nevertheless, Wells as a whole is behind second, this indicates 
#that some of the examined works are clear outliers in comparison to the average, as opposed to Heinlein, for whom the opposite is the case - out of ten least tf_idf works
#he has six as well, which is more in accordance to what is to be expected given his average tf_idf






books_dfm <- books_byword %>%
  count(author, word, sort = TRUE) %>% # word frequency per book, descending order
  cast_dfm(author, word, n) # create document frequency matrix (quanteda)

books_sparse <- books_byword %>%
  count(author, word, sort = TRUE) %>%
  cast_sparse(author, word, n)

topic_model_general <- stm(books_dfm, K = 4, 
                   verbose = FALSE, init.type = "Spectral")

td_beta_general <- tidy(topic_model_general)

td_beta_general %>%
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

td_gamma_general <- tidy(topic_model_general, matrix = "gamma",                     
                 document_names = rownames(books_dfm))

td_gamma_general %>% 
  ggplot(aes(gamma, fill = as.factor(topic))) + # histogram so only one variable needed
  geom_histogram(alpha = 0.8, show.legend = FALSE) + 
  facet_wrap(~ topic, ncol = 3) + # facet wrap according to topic
  labs(title = "Distribution of document probabilities for each topic",
       subtitle = "Each topic is associated with 1-3 novels",
       y = "Number of novels", x = expression(gamma))

word_counts <- books_byword %>%
  count(author, word, sort = TRUE) %>%
  ungroup()

books_dtm <- word_counts %>%
  cast_dtm(author, word, n)

books_dtm


books_lda <- LDA(books_dtm, k = 5, control = list(seed = 1234)) # data, num. of topics, seed
books_lda

books_topics <- tidy(books_lda, matrix = "beta")
books_topics

top_terms_books <- books_topics %>%
  group_by(topic) %>%
  slice_max(beta, n = 10) %>% 
  ungroup() %>%
  arrange(topic, -beta)

top_terms_books
str(top_terms_books)
top_terms_books %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(beta, term, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  scale_y_reordered() ################

books_gamma <- tidy(books_lda, matrix = "gamma")
books_gamma


books_gamma %>%
  mutate(author = reorder(author, gamma * topic)) %>%
  ggplot(aes(factor(topic), gamma)) +
  geom_boxplot() +
  facet_wrap(~ author) +
  labs(x = "topic", y = expression(gamma))

  colnames(books_gamma)[1] <- "author"
  
books_classifications <- books_gamma %>%
    group_by(author) %>%
    slice_max(gamma) %>%
    ungroup()
books_classifications



books_topics <- books_classifications %>%
  count(author, topic) %>%
  group_by(author) %>%
  slice_max(n, n = 1) %>% 
  ungroup() %>%
  transmute(consensus = author, topic)

books_classifications <- books_classifications %>%
  inner_join(books_topics, by = "topic") %>%
  filter(author != consensus)

assignments <- augment(books_lda, data = books_dtm)
assignments

assignments <- assignments %>%
  separate(document, c("document"), 
           sep = "_", convert = TRUE) %>%
  inner_join(books_topics, by = c(".topic" = "topic"))

assignments

assignments %>%
  count(document, consensus, wt = count) %>%
  mutate(across(c(document, consensus), ~str_wrap(., 20))) %>%
  group_by(document) %>%
  mutate(percent = n / sum(n)) %>%
  ggplot(aes(consensus, document, fill = percent)) +
  geom_tile() +
  scale_fill_gradient2(high = "darkred", label = percent_format()) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1),
        panel.grid = element_blank()) +
  labs(x = "Book words were assigned to",
       y = "Book words came from",
       fill = "% of assignments")

#
wrong_words <- assignments %>%
  filter(document != consensus)

wrong_words

wrong_words %>%
  count(document, consensus, term, wt = count) %>%
  ungroup() %>%
  arrange(desc(n))



Sys.setenv(JAVA_HOME = "C:/Program Files (x86)/Common Files/Oracle/Java/javapath")
library(rJava)
library(mallet)

# create a vector with one string per chapter
collapsed <- books_byword %>%
  mutate(word = str_replace(word, "'", "")) %>%
  group_by(author) %>%
  summarize(text = paste(word, collapse = " "))

# create an empty file of "stopwords"
file.create(empty_file <- tempfile())
docs <- mallet.import(collapsed$title, collapsed$text, empty_file)

mallet_model <- MalletLDA(num.topics = 6)
mallet_model$loadDocuments(docs)
mallet_model$train(1000)


# word-topic pairs
tidy_mallet_a <- tidy(mallet_model)

top_terms_mallet <- tidy_mallet_a %>%
  group_by(topic) %>%
  slice_max(beta, n = 5) %>% 
  ungroup() %>%
  arrange(topic, -beta)

top_terms_mallet %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(beta, term, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  scale_y_reordered()

?MalletLDA

# document-topic pairs
tidy_mallet_b <- tidy(mallet_model, matrix = "gamma")
tidy_mallet_b <- tidy_mallet_b %>% 
  separate(document, c("title", "chapter"), sep = "_", convert = TRUE)

tidy_mallet_b %>%
  mutate(title = reorder(title, gamma * topic)) %>%
  ggplot(aes(factor(topic), gamma)) +
  geom_boxplot() +
  facet_wrap(~ title) +
  labs(x = "topic", y = expression(gamma))



