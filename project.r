library(rvest)
library(dplyr)
library(stringr)
library(ggplot2)

setwd("C:/Users/adhit/Documents/NJIT/Data Analytics With R/Project")

url <- 'https://humgenomics.biomedcentral.com/articles'
webpage <- read_html(url)

titles <- webpage %>%
  html_nodes('.c-listing__title a') %>%
  html_text()

authors <- webpage %>%
  html_nodes('.c-listing__authors-list') %>%
  html_text()

abstracts <- webpage %>%
  html_nodes('.c-listing__content p') %>%
  html_text()

publish_dates <- webpage %>%
  html_nodes("[data-test='published-on'] span[itemprop='datePublished']") %>%
  html_text()

cat("Titles: ", length(titles), "\n")
cat("Authors: ", length(authors), "\n")
cat("Abstracts: ", length(abstracts), "\n")
cat("Publish Dates: ", length(publish_dates), "\n")

max_length <- max(length(titles), length(authors), length(abstracts), length(publish_dates))

titles <- c(titles, rep(NA, max_length - length(titles)))
authors <- c(authors, rep(NA, max_length - length(authors)))
abstracts <- c(abstracts, rep(NA, max_length - length(abstracts)))
publish_dates <- c(publish_dates, rep(NA, max_length - length(publish_dates)))

articles_df <- data.frame(
  Title = titles,
  Authors = authors,
  Abstract = abstracts,
  Publish_Date = publish_dates,
  stringsAsFactors = FALSE
)

write.csv(articles_df, "articles_data.csv", row.names = FALSE)

print(head(articles_df))

author_list <- str_split(articles_df$Authors, ",\\s*")
all_authors <- unlist(author_list)
author_df <- data.frame(Author = all_authors, stringsAsFactors = FALSE)
author_df$Author <- str_trim(author_df$Author)

author_freq <- author_df %>%
  group_by(Author) %>%
  summarise(Frequency = n()) %>%
  arrange(desc(Frequency))

top_authors <- author_freq %>% top_n(10, wt = Frequency)

ggplot(top_authors, aes(x = reorder(Author, Frequency), y = Frequency)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  coord_flip() +
  labs(title = "Top 10 Most Frequent Authors in Human Genomics Articles",
       x = "Author",    
       y = "Number of Articles") +
  theme_minimal()

ggsave("author_frequency_chart.png", width = 8, height = 6)

cat("Process completed! Outputs saved in the directory: C:/Users/adhit/Documents/NJIT/Data Analytics With R/Project")
