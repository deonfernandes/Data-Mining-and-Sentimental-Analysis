source("require_packages.R")
require_packages(c(
  "xml2",
  "httr",
  "dplyr",
  "readr",
  "tidytext",
  "textdata"
))




# Step 1: Fetch the RSS Feed using GET
response <- GET("http://rss.cnn.com/rss/cnn_latest.rss")
content <- content(response, "text")

# Step 2: Parse the RSS Feed
rss_feed <- read_xml(content)

# Step 3: Extract news titles and descriptions
news_items <- xml_find_all(rss_feed, ".//item")
titles <- xml_text(xml_find_all(news_items, ".//title"))
descriptions <- xml_text(xml_find_all(news_items, ".//description"))

# Include an id for each news item
news_data <- data.frame(id = seq_along(titles), title = titles, description = descriptions)

# Tokenize descriptions into words, retaining the id for each news item
words <- news_data %>%
  mutate(text = paste(title, description)) %>%  # Combine title and description for comprehensive sentiment analysis
  unnest_tokens(word, text) %>%
  select(word)  # Keep the id alongside the word




# Step 5: Perform sentiment analysis using NRC lexicon
nrc <- read_csv("nrc.csv")


# Join words with NRC lexicon and keep word data in the output
emotion_words <- words %>%
  inner_join(nrc, by = "word", relationship = 'many-to-many') %>%
  group_by(word, sentiment) %>%
  summarise(count = n(), .groups = 'drop')  # Count occurrences of each sentiment for each word

print(emotion_words)
#################################################
#################################################
# Total occurrences of each sentiment
total_sentiments <- emotion_words %>%
  group_by(sentiment) %>%
  summarise(total = sum(count), .groups = 'drop')

# Assuming total_sentiments is already calculated and includes each sentiment's total count
total_counts <- sum(total_sentiments$total)

# Calculate the proportion of each sentiment and convert it to a percentage format
sentiment_proportions <- total_sentiments %>%
  mutate(proportion = (total / total_counts) * 100) %>%  # Convert proportion to percentage
  mutate(proportion = sprintf("%.2f%%", proportion))  # Format the proportion as a percentage string with two decimal places

# Print the sentiment proportions in percentage
print(sentiment_proportions)


filename <- sprintf("%s-%s.csv", 
                    format(Sys.Date(), "%m-%d"),  # Format for month and day
                    format(Sys.time(), "%H%M"))   # Format for hour and minute

# Specify the path to the directory where the file should be saved
# Adjust "deonfernandes/data_mining" to the actual path where your GitHub repository's relevant folder is cloned locally
path <- file.path("deonfernandes/data_mining", filename)

# Use write_csv to save the sentiment_proportions dataframe to the specified path
write_csv(sentiment_proportions, path)
