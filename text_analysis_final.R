#text analysis

library(tidyverse)
library(tidytext)
library(fuzzyjoin)
library(colorspace)

#### Data Import ####

guardian_raw <- read_rds("./data/guardian_articles_from_01-11-2016_to_31-03-2017_topics_media_usnews.rds.bz2")
breitbart_raw <- read_rds("./data/breitbart_articles_from_01-11-2016_to_31-03-2017_topics_politics-the-media.rds.bz2")
dictionary <- read_csv("custom_dictionary.csv")

#join articles, removing not shared columns
articles = bind_rows(guardian = select(guardian_raw, -topic),
                     breitbart = select(breitbart_raw, -last_modified, -topic),
                     .id = "source") 

#filter by:
media_articles <- articles %>%
  #only unique urls
  distinct(url, .keep_all = TRUE) %>%
  #only non-empty text (the web-scraping scripts aren't perfect)
  filter(!is.na(text)) %>%
  #only articles that contain "fake news". (case insesntitive, as whole phrase)
  filter(str_detect(text, "(?i)fake news"))

#print n of articles
media_articles %>%
  count(source)

#### Data Preparation ####

#add certain word groups
grouped_articles <- media_articles %>%
  mutate(text = str_replace_all(text, c("fake news" = "fake_news",
                                        "alternative fact" = "alternative_fact",
                                        "conspiracy theory" = "conspiracy_theory",
                                        "filter bubble" = "filter_bubble")))

#split by sentence
by_sentence <- grouped_articles %>%
  unnest_tokens(sentence, text, token = "sentences") 

#remove sentences where they quote the other side
saying <- c("said", "say", 
           "claim", 
           "deny",
           "write", "wrote",
           "call", 
           "term",
           "“", "”") #quotation marks

saying_regex <- str_c("(?i)(", str_c(saying, collapse = "|"), ")")
no_saying <- by_sentence %>%
  filter(!str_detect(sentence, saying_regex))

#tokenise by word, removing stopwords
by_word <- no_saying %>%
  unnest_tokens(word, sentence, token = "words") %>%
  anti_join(get_stopwords('en'), by = "word")

#create regex to find all words in the dictionary
#word has to be at start of the string,
#so e.g "prov" matches "proving" and "provable" but not "improve"
dict_regex <- str_c("(", str_c(dictionary$word, collapse = "|^"), ")")


#### Analysis ####

#get counts of dictionary words occuring in the articles
occurring_dict_words <- by_word %>%
  #dictionary words only
  filter(str_detect(word, dict_regex)) %>%
  #add in associations
  regex_left_join(dictionary, by = "word") %>%
  #sum the different text words (word.x) that match the same dictionary word (word.y)
  #'bring along' the columns: source, association
  group_by(source, association, word.y) %>%
  summarise(n = n()) %>%
  ungroup() %>%
  rename(word = word.y)

#include non-occurring words 
all_dict_words <- dictionary %>%
  #exclude words that do not occur at all
  filter(word %in% occurring_dict_words$word) %>%
  #create the grid of all word-source combinations
  expand_grid(source = c("guardian", "breitbart")) %>%
  #add this back into tbl of occurring words
  full_join(occurring_dict_words, by = c("source", "word", "association")) %>%
  #replace generated NAs
  replace(., is.na(.), 0)

#get total words, by news source
n_words = by_word %>%
  count(source, name = "total")

#get log ratios.
log_ratios <- all_dict_words %>%
  left_join(n_words, by = "source") %>%
  #calculate frequency adding one to both sides,
  #so that we get a non-0 value if the word only occurs in 1 text
  mutate(freq = (n + 1) / (total + 1)) %>%
  #get two-column format
  select(-c(n, total)) %>%
  pivot_wider(names_from = source,
              values_from = freq) %>%
  #calculate ratio and mean grequency
  mutate(log_ratio = log2(breitbart / guardian)) %>%
  rowwise() %>%
  mutate(mean_freq = mean(c(breitbart, guardian))) %>%
  ungroup()


#### Visualise ####

horiz_bar_plot <- function(data) {
  data %>%
    #clean regex markers
    mutate(word = str_replace_all(word, "[\\(\\[].*[\\)\\]]", "") %>% #between () or []
             str_replace_all("[\\^\\$]", ""), #any ^ or $
           #order words
           word = fct_reorder(word, log_ratio)) %>%
    #plot
    ggplot(aes(x = word, y = log_ratio, fill = log10(mean_freq))) +
      geom_col() +
      facet_wrap(~association, scales = "free") +
      labs(title = "Log Ratios of Keyword Frequency",
           y = "Log2 of Frequency Ratio: Breitbart / Guardian",
           x = "") +
      theme_minimal() +
      theme(strip.text = element_text(size = 12)) +
      scale_fill_continuous_sequential(palette = "ag_Sunset",
                                       name = "Log10 of mean\nfrequency") +
      coord_flip()
}

#order associations for plotting
to_plot <- log_ratios %>%
  mutate(association = fct_relevel(association, "sourcing", "correction", "honesty",
                                   "facts", "truth", "purpose", "manipulation"))

#plot
to_plot %>%
  filter(association %in% c("sourcing", "correction")) %>%
  horiz_bar_plot()
ggsave("graphs/sourcing_sunset.png")

to_plot %>%
  filter(association %in% c("honesty", "truth", "purpose", "facts")) %>%
  horiz_bar_plot()
ggsave("graphs/truth_sunset.png")

log_ratios %>%
  filter(association %in% c("manipulation")) %>%
  horiz_bar_plot()
ggsave("graphs/manip_sunset.png")

#####
