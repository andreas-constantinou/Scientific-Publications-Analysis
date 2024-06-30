library(rvest)
library(httr)
library(xml2)
library(stringr)
library(dplyr)
library(ggplot2)
library(tidyr)

extract_article <- function(year) {
  url <- paste0('https://neuraldevelopment.biomedcentral.com/articles?query=', gsub(" ", "", as.character(year)))
  ND <- read_html(url)
  
  # Extract titles
  titles <- ND %>% 
    html_nodes('.c-listing__title') %>% 
    html_text() %>% 
    str_trim()
  
  # Extract authors
  authors <- ND %>% 
    html_nodes('.c-listing__authors-list') %>% 
    html_text() %>% 
    str_trim()
  
  # Extract article links
  articles_links <- ND %>% 
    html_nodes('.c-listing__title a') %>% 
    html_attr('href') %>% 
    paste0('https://neuraldevelopment.biomedcentral.com', .)
  
  # Extract publication dates
  dates <- ND %>% 
    html_nodes(".c-listing__metadata span span") %>% 
    html_text()
  
  published_date <- dates[grepl("\\d+ \\w+ \\d{4}", dates)]
  
  key_words <- vector("list", length(articles_links))
  abstracts <- vector("list", length(articles_links))
  corresponding_author_email <- vector("list", length(articles_links))
  corresponding_author_name <- vector("list", length(articles_links))
  
  for (i in seq_along(articles_links)) {
    link <- read_html(articles_links[i])
    
    # Extract keywords
    keys <- link %>% 
      html_nodes('.c-article-subject-list__subject') %>% 
      html_text() %>% 
      gsub("\n", ", ", .)
    key_words[[i]] <- ifelse(length(keys) > 0, keys, NA)
    
    # Extract abstracts
    abstract <- link %>% 
      html_nodes('#Abs1-content p') %>% 
      html_text()
    abstracts[[i]] <- ifelse(length(abstract) > 0, abstract, NA)
    
    # Extract corresponding author name and email
    author_name <- link %>% 
      html_nodes(xpath = '//*[@id="corresp-c1"]') %>% 
      html_text()
    corresponding_author_name[[i]] <- ifelse(length(author_name) > 0, author_name, NA)
    
    email_address <- link %>% 
      html_node(xpath = '//*[@id="corresp-c1"]') %>% 
      html_attr("href") %>% 
      gsub("mailto:", "", .)
    corresponding_author_email[[i]] <- ifelse(length(email_address) > 0, email_address, NA)
  }
  
  df <- data.frame(
    Title = titles,
    Authors = authors,
    Corresponding_author = unlist(corresponding_author_name),
    Corresponding_mail = unlist(corresponding_author_email),
    Published_Date = published_date,
    Abstract = unlist(abstracts),
    Key_Words = unlist(key_words)
  )
  
  return(df)
}

all_data <- list()
start_year <- 2006
end_year <- 2024

for (year in start_year:end_year) {
  data <- extract_article(year)
  all_data[[as.character(year)]] <- data
}

combined_data <- bind_rows(all_data) %>% unique()
write.csv(combined_data, "neural_development_data.csv", row.names = FALSE)

# Data Cleaning and Handling Missing Values

# Load the gathered combined data into unclean_data
unclean_data <- read.csv("neural_development_data.csv")

# Replace missing values in all columns with "Unknown"
unclean_data[is.na(unclean_data)] <- "Unknown"

# Convert Published_Date column to Date format
unclean_data$Published_Date <- as.Date(unclean_data$Published_Date, format = "%d %B %Y")
unclean_data$Authors <- as.factor(unclean_data$Authors)
print(unclean_data)

# Save the cleaned data into a new CSV file named cleaned_neural_development_data.csv
write.csv(unclean_data, "cleaned_neural_development_data.csv", row.names = FALSE)

# Load the cleaned data for analysis of the number of publications per year
cleaned_data <- read.csv("cleaned_neural_development_data.csv")

# Convert Published_Date column to Year format
cleaned_data$Year <- format(as.Date(cleaned_data$Published_Date), "%Y")

# Create a data frame with publication counts per year
publication_count <- cleaned_data %>%
  count(Year, name = "Count")

# Bar chart showing the number of publications per year
ggplot(publication_count, aes(x = Year, y = Count)) +
  geom_bar(stat = "identity", fill = "blue") +
  labs(title = "Number of Publications by Year",
       x = "Year",
       y = "Publication Count") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Contribution of total number of authors over the years
total_authors <- cleaned_data %>%
  group_by(Year) %>%
  summarise(Total_Authors = n_distinct(Authors))

# Bar chart showing the contribution of total number of authors over the years
ggplot(total_authors, aes(x = Year, y = Total_Authors)) +
  geom_bar(stat = "identity", fill = "red") +
  labs(title = "Contribution of Total Number of Authors Over Years",
       x = "Year",
       y = "Total Number of Authors") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Combine the data of publications by year and authors by year
combined_data <- left_join(publication_count, total_authors, by = "Year")

# Plot the relationship between publications and authors by year as a grouped bar chart
ggplot(combined_data, aes(x = Year)) +
  geom_bar(aes(y = Count, fill = "Publications"), stat = "identity", position = "dodge") +
  geom_bar(aes(y = Total_Authors, fill = "Authors"), stat = "identity", position = "dodge") +
  labs(title = "Publications and Authors by Year",
       x = "Year",
       y = "Count") +
  scale_fill_manual(values = c("Publications" = "blue", "Authors" = "red")) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

library(dplyr)
library(tidyr)
library(ggplot2)

# Read the CSV file
data <- read.csv("cleaned_neural_development_data.csv")

# Replace "Unknown" values with NA
data[data == "Unknown"] <- NA

# Bar Chart of Authors
authors_counts <- data %>%
  filter(!is.na(Authors)) %>%
  separate_rows(Authors, sep = ",") %>%
  count(Authors, sort = TRUE)

ggplot(head(authors_counts, 10), aes(x = reorder(Authors, -n), y = n)) +
  geom_bar(stat = "identity", fill = "lightgreen") +
  labs(title = "Top 10 Authors by Frequency",
       x = "Author",
       y = "Frequency") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Boxplot of Abstract Lengths
data %>%
  filter(!is.na(Abstract)) %>%
  mutate(Abstract_Length = nchar(Abstract)) %>%
  ggplot(aes(x = "", y = Abstract_Length)) +
  geom_boxplot(fill = "skyblue") +
  labs(title = "Distribution of Abstract Lengths",
       y = "Abstract Length",
       x = NULL) +
  theme_minimal()
