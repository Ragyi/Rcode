## Text analytics

####################################################################################
################################ Tidy Text package #################################
####################################################################################

# load required packages
required_packages <- list(y = c("broom", "tidytext", 
                                "stringr", "tm", "ggplot2", "janeaustenr", 
                                "SnowballC", "wordcloud", "RColorBrewer", 
                                "RCurl", "XML", "gutenbergr", "tidyr", "scales"))

for( i in 1:length(required_packages$y))
  if(required_packages$y[i] %in% rownames(installed.packages()) == FALSE) {
    install.packages(required_packages$y[i])
    lapply(required_packages$y[i], require, character.only = TRUE)
  } else{
    if (required_packages$y[i] %in% (.packages())){
      print(paste0("Package ", required_packages$y[i],  " is loaded"))
    } else {
      lapply(required_packages$y[i], require, character.only = TRUE)
    }
  }

# create a dummy text object
text <- c("Because I could not stop for Death -",
          "He kindly stopped for me -",
          "The Carriage held but just Ourselves -",
          "and Immortality")

# Using Dplyr library transform into DF
text_df <- data_frame(line = 1:4, text = text)

# Tokenisation of text DF
text_df %>% unnest_tokens(word, text)

#Adding chpater and line numbers (dont really see the need for this)
original_books <- austen_books() %>%
  group_by(book) %>%
  mutate(linenumber = row_number(),
         chapter = cumsum(str_detect(text, regex("^chapter [\\divxlc]",
                                                 ignore_case = TRUE)))) %>%
  ungroup()
original_books

#Tokenize books
tidy_books <- original_books %>%
  unnest_tokens(word, text, token = "sentence") 
tidy_books

#Remove stop-words
data(stop_words)
tidy_books <- tidy_books %>%
  anti_join(stop_words, by = "word")
tidy_books

#Group and sort words
tidy_books %>% 
  count(word, sort = T)

#Visualise words
tidy_books %>%
  count(word, sort = T) %>%
  filter(n > 600) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n)) + geom_col() + coord_flip() + xlab("Words") + ylab("Frequency")

#Create word cloud corpus
corpus_words <- tidy_books %>%
  count(word, sort = T)

#Create word cloud
set.seed(1234)
wordcloud(words = corpus_words$word, freq = corpus_words$n , min.freq = 100,
          max.words=200, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))


####################################################################################
################################ Gutenbergr package ################################
####################################################################################

#Getting the required text
hgwells <- gutenberg_download(c(35, 36, 5230, 159))

# Create tidy version of HG Wells books
tidy_hgwells <- hgwells %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words, by = "word") %>%
  count(word, sort = T)

bronte <- gutenberg_download(c(1260, 768, 969, 9182, 767))

tidy_bronte <- bronte %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words, by = "word") %>%
  count(word, sort = T)

## Combine data frame

frequency <- bind_rows(mutate(tidy_bronte, author = "Brontë Sisters"),
                       mutate(tidy_hgwells, author = "H.G. Wells"), 
                       mutate(tidy_books, author = "Jane Austen")) %>% 
  mutate(word = str_extract(word, "[a-z']+")) %>%
  count(author, word) %>%
  group_by(author) %>%
  mutate(proportion = n / sum(n)) %>% 
  select(-n) %>% 
  spread(author, proportion) %>% 
  gather(author, proportion, `Brontë Sisters`, `H.G. Wells`)

##We use str_extract() here because the UTF-8 encoded texts from Project Gutenberg have some examples of words with underscores around them to indicate emphasis (like italics). The tokenizer treated these as words, but we don’t want to count “_any_” separately from “any” as we saw in our initial data exploration before choosing to use str_extract().

#Plotting the word frequencies of Jane Austen, the Brontë sisters, and H.G. Wells
  
  ggplot(frequency, aes(x = proportion, y = `Jane Austen`, color = abs(`Jane Austen` - proportion))) +
    geom_abline(color = "gray40", lty = 2) +
    geom_jitter(alpha = 0.1, size = 2.5, width = 0.3, height = 0.3) +
    geom_text(aes(label = word), check_overlap = TRUE, vjust = 1.5) +
    scale_x_log10(labels = percent_format()) +
    scale_y_log10(labels = percent_format()) +
    scale_color_gradient(limits = c(0, 0.001), low = "darkslategray4", high = "gray75") +
    facet_wrap(~author, ncol = 2) +
    theme(legend.position="none") +
    labs(y = "Jane Austen", x = NULL)
