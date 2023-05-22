setwd("C:\\Users\\35387\\OneDrive\\Desktop\\RStudio")
package_list <- c(
  "dplyr",
  "ggplot2",
  "igraph",
  "irr",
  "LDAvis",
  "LiblineaR",
  "Matrix",
  "NLP",
  "openNLP",
  "openNLPdata",
  "pals",
  "png",
  "quanteda",
  "readtext",
  "reshape2",
  "rvest",
  "topicmodels",
  "tsne",
  "webdriver",
  "wordcloud",
  "wordcloud2"
)
lapply(package_list, require, character.only = TRUE)
install.packages()
install.packages("tm") 
install.packages("topicmodels")
install.packages("dplyr" )
install.packages("stringr")
install.packages("tidytext")
install.packages("reshape2")
install.packages("LDAvis")
install.packages("ggplot2")
install.packages("wordcloud")
install.packages("countrycode")
install.packages("maps")
install.packages("mapproj")
install.packages("leaflet")
install.packages("sp")
install.packages("plotly")
library(tm)
library(topicmodels)
library(dplyr)
library(stringr)
library(tidytext)
library(reshape2)
library(LDAvis)
library(ggplot2)
library(wordcloud)
library(countrycode)
library(maps)
library(mapproj)
library(leaflet)
library(sp)
library(plotly)
stopwords <- stopwords("en")


data <- read.csv("C:/Users/35387/OneDrive/Desktop/RStudio/tweets.csv"
                 , header = TRUE, encoding = "UTF-8", stringsAsFactors = FALSE)

#TOPICMODELING
   # Create a document-term matrix
dtm <- DocumentTermMatrix(Corpus(VectorSource(data$text)),
                          control = list(stopwords = TRUE, minWordLength = 3))
      
      # Create the topic model

lda_model <- LDA(dtm, k = 5, method = "Gibbs", control = list(seed = 1234))
num_topics <- topics(lda_model)

# Update the value of k to match the actual number of topics
k <- num_topics

View(lda_model)

# Extract topics from the LDA model
topics <- tidy(lda_model, matrix = "beta")
# View the top words for each topic
top_words <- topics %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup()

# Interpret the topics
topic_interpretation <- top_words %>%
  arrange(topic, -beta) %>%
  mutate(rank = row_number()) %>%
  select(topic, term, rank)
# Print the topic interpretation
print(topic_interpretation)



# Creating a bar plot of the top terms for each topic
ggplot(topic_interpretation, aes(x = rank, y = term, fill = factor(topic))) +
  geom_bar(stat = "identity") +
  facet_wrap(~ topic, ncol = 2, scales = "free") +
  labs(x = "Rank", y = "Term", fill = "Topic") +
  theme_minimal()

# Check the structure of topic_interpretation
str(topic_interpretation)

# Creating a word cloud using the topic_interpretation data
wordcloud(topic_interpretation$term, topic_interpretation$topic, scale=c(2, 0.4)
          ,random.order=FALSE, rot.per=0.3, colors=brewer.pal(8, "Dark2"))




# Extract country names from user_location
country_names <- str_extract(data$user_location, "\\b[A-Za-z]+\\b")

# Count occurrences of country names
country_counts <- table(country_names)

# Sort the counts in descending order
sorted_counts <- sort(country_counts, decreasing = TRUE)

# Display the top N countries and their counts
N <- 10 # Specify the number of top countries to display
top_countries <- head(sorted_counts, N)

# Print the top countries and their counts
print(top_countries)


# Creating a data frame from top_countries
library(ggplot2)

# Creating a data frame from top_countries
top_countries_df <- data.frame(country = names(top_countries),
                               count = as.numeric(top_countries))

# Creating a bar plot
bar_plot <- ggplot(top_countries_df, aes(x = country, y = count)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  xlab("Country") +
  ylab("Count") +
  ggtitle("Top Countries by Count") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Displaying the bar plot
print(bar_plot)

# Creating a word cloud
> wordcloud(words = names(top_countries), freq = top_countries,
            +           scale = c(7, 3), random.order = FALSE,
            +           colors = brewer.pal(8, "Dark2"))

#GEOVISUALISATION
# Identifying the most frequent countries
top_countries <- head(sort(table(data$user_location), decreasing = TRUE), 50)

# Creating a data frame with country names and frequencies
country_data <- data.frame(country = names(top_countries), frequency = 
                             as.numeric(top_countries))

#  Loading country map data
map_data <- map_data("world")

# Merging country data with map data
merged_data <- merge(map_data, country_data, by.x = "region", by.y = "country",
                     all.x = TRUE)

#  Creating a choropleth map
ggplot(merged_data, aes(x = long, y = lat, group = group, fill = frequency)) +
  geom_polygon(color = "white", size = 0.1) +
  scale_fill_gradient(low = "lightblue", high = "darkblue", na.value = "grey50")
+
  theme_void()











