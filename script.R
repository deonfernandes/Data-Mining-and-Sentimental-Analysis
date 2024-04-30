source("require_packages.R")
require_packages(c(
  "xml2",
  "httr",
  "dplyr",
  "readr",
  "tidytext",
  "textdata"
))

# get the RSS Feed using GET request
response <- GET("http://rss.cnn.com/rss/cnn_latest.rss")
content <- content(response, "text")

#Parse the RSS Feed
rss_feed <- read_xml(content)

# Extract news titles and descriptions from the RSS feed
news_items <- xml_find_all(rss_feed, ".//item")
titles <- xml_text(xml_find_all(news_items, ".//title"))
descriptions <- xml_text(xml_find_all(news_items, ".//description"))

# Create a dataframe of item, title, and description
news_data <- data.frame(id = seq_along(titles), title = titles, description = descriptions)

# Tokenize descriptions into words
words <- news_data %>%
  mutate(text = paste(title, description)) %>%  # Combine title and description 
  unnest_tokens(word, text) %>%
  select(word)

#to perform sentiment analysis using NRC lexicon
nrc <- read_csv("nrc.csv")

#to join words with NRC lexicon and keep word along with the count of sentiments
sentiment_words <- words %>%
  inner_join(nrc, by = "word", relationship = 'many-to-many') %>%
  group_by(word, sentiment) %>%
  summarise(count = n(), .groups = 'drop')  # Count occurrences of each sentiment for each word
print(sentiment_words)

# Total occurrences of each sentiment
total_sentiments <- sentiment_words %>%
  group_by(sentiment) %>%
  summarise(total = sum(count), .groups = 'drop')

#Total count of sentiments
total_counts <- sum(total_sentiments$total)

# Calculate the proportion of each sentiment and convert it to a percentage format
sentiment_proportions <- total_sentiments %>%
  mutate(proportion = (total / total_counts) * 100) %>%  # Convert proportion to percentage
  mutate(proportion = sprintf("%.2f%%", proportion))  # Format the proportion as a percentage string with two decimal places

# Print the sentiment proportions in percentage
print(sentiment_proportions) 

sentiment_proportions |> write_csv(format(Sys.time(), "%Y-%m-%d-%H-%M.csv"))

