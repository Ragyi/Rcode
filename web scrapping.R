#Web scraping

# Loading required libraries:

# General-purpose data wrangling
#install.packages("tidyverse")
library(tidyverse)  

# Parsing of HTML/XML files  
#install.packages("rvest")
library(rvest)    

# String manipulation
#install.packages("stringr")
library(stringr)   

# Verbose regular expressions
#install.packages("rebus")
library(rebus)     

# Eases DateTime manipulation
#install.packages("lubridate")
library(lubridate)

#url <- "https://boqspecialist.com.au"
#boqs_site_xml <- read_html(url)

url_2 <- 'http://www.trustpilot.com/review/www.amazon.com'
test_site_xml <- read_html(url_2)

get_last_page <- function(html){
  
  pages_data <- html %>% 
    # The '.' indicates the class
    html_nodes('.pagination-page') %>% 
    # Extract the raw text as a list
    html_text()                   
  
  # The second to last of the buttons is the one
  pages_data[(length(pages_data)-1)] %>%            
    # Take the raw string
    unname() %>%                                     
    # Convert to number
    as.numeric()                                     
}

first_page <- read_html(url_2)
latest_page_number <- get_last_page(first_page)

get_reviews <- function(html){
  html %>% 
    # The relevant tag
    html_nodes('.review-info__body__text') %>%      
    html_text() %>% 
    # Trim additional white space
    str_trim() %>%                       
    # Convert the list into a vector
    unlist()
}

get_reviewer_names <- function(html){
  html %>% 
    html_nodes('.user-review-name-link') %>% 
    html_text() %>% 
    str_trim() %>% 
    unlist()
}

get_review_dates <- function(html){
  
  status <- html %>% 
    html_nodes('time') %>% 
    # The status information is this time a tag attribute
    html_attrs() %>%             
    # Extract the second element
    map(2) %>%                    
    unlist() 
  
  dates <- html %>% 
    html_nodes('time') %>% 
    html_attrs() %>% 
    map(1) %>% 
    # Parse the string into a datetime object with lubridate
    ymd_hms() %>%                 
    unlist()
  
  # Combine the status and the date information to filter one via the other
  return_dates <- tibble(status = status, dates = dates) %>%   
    # Only these are actual reviews
    filter(status == 'ndate') %>%              
    # Select and convert to vector
    pull(dates) %>%                            
    # Convert DateTimes to POSIX objects
    as.POSIXct(origin = '1970-01-01 00:00:00') 
  
  # The lengths still occasionally do not lign up. You then arbitrarily crop the dates to fit
  # This can cause data imperfections, however reviews on one page are generally close in time)
  
  length_reviews <- length(get_reviews(html))
  
  return_reviews <- if (length(return_dates)> length_reviews){
    return_dates[1:length_reviews]
  } else{
    return_dates
  }
  return_reviews
}

get_star_rating <- function(html){
  
  # The pattern you look for: the first digit after `count-`
  pattern = 'count-'%R% capture(DIGIT)    
  
  ratings <-  html %>% 
    html_nodes('.rating.summary-rating') %>% 
    html_attrs() %>% 
    # Apply the pattern match to all attributes
    map(str_match, pattern = pattern) %>%
    # str_match[1] is the fully matched string, the second entry
    # is the part you extract with the capture in your pattern  
    map(2) %>%                             
    
    unlist()
  
  # Leave out the first instance, as it is not part of a review
  ratings[2:length(ratings)]               
}

for(i in 1:60){
  date_time[i] <- review_dates[i+3] %>%
  data.frame(date_time)
}

print(date_time[[3]])