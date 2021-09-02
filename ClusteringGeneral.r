###Clustering General

library(tidyverse)
library(gutenbergr)
library(tidytext)
library(topicmodels)
library(quanteda) # used to create the structure
library(stm) # structured topic modeling
library(dplyr)
library(readr)
library(FactoMineR)
library(factoextra)
library(sentimentr)
library(magrittr)
library(knitr)
library(tinytex)
library(koRpus)
library(koRpus.lang.en)
library(rvest)
library(gutenbergr)
library(tidyverse)
library(tidytext)
library(tm)
library(dplyr)
library(broom)
library(ggrepel)

library(FactoMineR)
library(factoextra)

# Making a list of the texts according to the title column
split_books <- split(books_total, books_total$title)

# Selecting only the text column in each element of the list
books_strings <- lapply(split_books, function(x) x%>% select(text))

# Converting these columns to strings (creating a character vector)
books_strings <- sapply(books_strings, toString)

names(books_strings) <- c("2001 A Space Odyssey", "2010 Odyssey Two", "2061 Odyssey Three", "3001 The Final Odyssey", "A Fall of Moondust", "Door Into Summer", "Double Star", "Foundation","Foundation's Edge", 
                            "Foundation and Earth", "Foundation and Empire", "Friday", "Have Space Suit—Will Travel", "Childhood's End",  "I, Robot", "Luna is a harsh mistress", "Prelude to Foundation",
                            "Rendezvous with Rama", "Second Foundation",  "Songs of Distant Earth", "Starship Troopers", "Stranger in a Strange Land", "The Cat Who Walked Through Walls", 
                            "The Caves of Steel", "The City and the Stars", "The Country of the Blind", "The Door in the Wall", "The End of Eternity", "The First Men in the Moon", "The Food of the Gods and How It Came to Earth",
                            "The Fountains of Paradise", "The Gods Themselves", "The Invisible Man: A Grotesque Romance", "The Island of Doctor Moreau", "The Magic Shop", "The Puppet Masters",
                            "The Time Machine", "The War of the Worlds",  "Time Enough for Love",  "When the Sleeper Wakes")
  
  for (i in 1:length(books_strings)) {
    write.csv(books_strings[i], file=paste0(names(books_strings)[i], ".txt"))
}

write.csv(books_strings, file="name.txt")
#for whatever reason, The Invisible Man, and Space Odyssey 2001, 2010 and 2061 are created as empty files, so here I'm manually creating them and moving the text from a whole string into them

# The files were manually moved to the "Project/Txts/" subfolder

filenames <- list.files(path="Project/Txts/", pattern="*.txt", full.names=T, recursive=FALSE)

text.tagged <- lapply(filenames, function(x) koRpus::tokenize(x, lang="en"))

raw_MATTR_values_books <- lapply(text.tagged, MATTR, window = 25)

final_MATTR_values_books <- sapply(lapply(raw_MATTR_values_books,slot,'MATTR'),'[',c('MATTR'))

final_MATTR_values_books <- unlist(final_MATTR_values_books)

names(final_MATTR_values_books) <- c("2001 A Space Odyssey", "2010 Odyssey Two", "2061 Odyssey Three", "3001 The Final Odyssey", "A Fall of Moondust", "Door Into Summer", "Double Star", "Foundation","Foundation's Edge", 
                                     "Foundation and Earth", "Foundation and Empire", "Friday", "Have Space Suit—Will Travel", "Childhood's End",  "I, Robot", "Luna is a harsh mistress", "Prelude to Foundation",
                                     "Rendezvous with Rama", "Second Foundation",  "Songs of Distant Earth", "Starship Troopers", "Stranger in a Strange Land", "The Cat Who Walked Through Walls", 
                                     "The Caves of Steel", "The City and the Stars", "The Country of the Blind", "The Door in the Wall", "The End of Eternity", "The First Men in the Moon", "The Food of the Gods and How It Came to Earth",
                                     "The Fountains of Paradise", "The Gods Themselves", "The Invisible Man: A Grotesque Romance", "The Island of Doctor Moreau", "The Magic Shop", "The Puppet Masters",
                                     "The Time Machine", "The War of the Worlds",  "Time Enough for Love",  "When the Sleeper Wakes")
final_MATTR_values_books <- data.frame(final_MATTR_values_books)
library(data.table)
# We use the setDT() function from the data.table package to convert the rownames to values of a
# separate column

final_MATTR_values_books<- setDT(final_MATTR_values_books, keep.rownames = TRUE)[]
colnames(final_MATTR_values_books)[1] <- "Title"



books_sentiment <- books_total %>% 
  mutate(sentences = get_sentences(text)) %$% # exposition pipe, exposing specific variables to function
  sentiment_by(sentences, title)

clustering_dataset <- cbind(final_MATTR_values_books, books_sentiment) %>% 
  select(-3) %>% 
  rename(title=Title)

ggplot(clustering_dataset, aes(final_MATTR_values_books, ave_sentiment)) +
  geom_point()


km_fit <- clustering_dataset %>% 
  select(final_MATTR_values_books, ave_sentiment) %>% 
  kmeans(centers = 3, nstart = 15)
clustering_plot <- km_fit %>% 
  augment(clustering_dataset) %>% 
  ggplot() +
  aes(x = final_MATTR_values_books, y = ave_sentiment) +
  geom_point(
    aes(color = .cluster)
  ) +
  geom_point(
    data = tidy(km_fit),
    aes(fill = cluster),
    shape = 21, color = "black", size = 4
  ) +
  guides(color = "none")

clustering_plot <- clustering_plot +
  geom_label_repel((aes(label = title)),
                   box.padding   = 0.35, 
                   point.padding = 0.5,
                   segment.color = 'grey50') +
  theme_classic() +
  ggtitle("K means clustering of selected works (avg. sentiment and MATTR)") +
  xlab("MATTR values") +
  ylab("avg. sentiment")
clustering_plot


calc_withinss <- function(data, centers) {
  km_fit <- select(data, where(is.numeric)) %>%
    kmeans(centers = centers, nstart = 10)
  km_fit$tot.withinss
}
tibble(centers = 1:10) %>%
  mutate(
    within_sum_squares = map_dbl(
      centers, ~calc_withinss(clustering_dataset, .x)
    )
  ) %>%
  ggplot() +
  aes(centers, within_sum_squares) +
  geom_point() +
  geom_line()

hc <- hclust(dist)

# Plot the result
plot(hc,
     labels = clustering_dataset$title, 
     main = "Dendrogram of selected works",
     xlab = "Distance", 
     ylab = "Height")
rect.hclust(hc, k = 5, border = 2:4)


df1 <- clustering_dataset[,c(2,5)] #
# Computing k-means
set.seed(123)
km.res <- kmeans(df1, 5, nstart = 10)
# Visualization
str(clustering_dataset)
part_cluster_plot <- fviz_cluster(km.res, data = df1,
                                  geom = "point",
                                  palette = c("#00AFBB", "#E7B800", "#FC4E07", "#D303FC", "#FC8403"),
                                  main = "Partitioning Clustering Plot"
)

part_cluster_plot <- part_cluster_plot +
  geom_label_repel((aes(label = clustering_dataset$title)),
                   box.padding   = 0.35, 
                   point.padding = 0.5,
                   segment.color = 'grey50') +
  theme_classic()
part_cluster_plot