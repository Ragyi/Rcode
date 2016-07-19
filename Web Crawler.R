# LOAD LIBRARIES REQUIRED

library(plyr)
library(XML)
library(RCurl)
library(httr)



# FIGURE OUT PATTERN OF URL FOR EACH SEASON

url.b1 = 'http://ca.sports.yahoo.com/nhl/stats/byposition?pos=C,RW,LW,D';

url.b2 = '&sort=14&conference=NHL&year=season_';

url.b3 = 'y';

url    = paste(url.b1, url.b2, sep = '')

tabs <- GET(url)
tabs <- readHTMLTable(rawToChar(tabs$content), stringsAsFactors = F)

url.ragy <- getURL(url)
url.ragy.table <- readHTMLTable(url.ragy, stringsAsFactors = FALSE)

# WRITE FUNCTION TO EXTRACT DATA FOR A SEASON

extract_data <- function(y){

  url    = paste(url.b1, url.b2, as.character(y), sep = '');

  tab    = readHTMLTable(url, stringsAsFactors = FALSE)[[4]];

  tab    = tab[,-c(2*(2:16))] # remove empty columns

  names(tab) = tab[1,];

  tab		   = tab[-1,];

  tab$year   = y;

  tab

}

# APPLY FUNCTION TO EXTRACT DATA FOR ALL SEASONS

skaters <- ldply (2005:2010, extract_data, .progress = 'text')


# CLEAN DATA FRAME AS REQUIRED

skaters[,-c(1, 2)] = sapply(skaters[, -c(1, 2)], as.numeric);

skaters[, c(1, 2)] = sapply(skaters[, c(1, 2)], as.factor);

names(skaters) = tolower(names(skaters));

names(skaters)[7] = 'pm'


write.csv(skaters, 'skaters.csv', row.names = F)
