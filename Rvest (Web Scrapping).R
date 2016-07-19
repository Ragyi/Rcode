# Using rvest to extract data from html sites 

install.packages("rvest", dependencies = TRUE)
library(rvest)
lego_movie <- read_html("http://www.imdb.com/title/tt1490017/")

#Extract Ratings
lego_movie %>% 
  html_node("strong span") %>%
  html_text() %>%
  as.numeric()

#Extract Cast
Cast <- lego_movie %>%
  html_nodes("#titleCast .itemprop span") %>%
  html_text()

Blurb <- lego_movie %>%
  html_nodes("div#titleStoryLine.article p") %>%
  html_text()

Review <- lego_movie %>%
  html_nodes("div#titleUserReviewsTeaser.article p") %>%
  html_text()

#Extract Comments
lego_movie %>%
  html_nodes("table") %>%
  .[[3]] %>%
  html_table()