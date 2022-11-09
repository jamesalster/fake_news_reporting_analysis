#scrape script heavily based on guardian_scrape_2.R because the sites are very similarly organised

#To use, set the global variables at the head of the script
#Setting a large date range takes AGES because it fetches ALL the published titles, then searches within them
#Search is a case-insensitive string match in title of article
#Because of breitbart site organisation, you have to select topics to search within.
#Date-times are gmt
#Decent error handling for e.g url not found, no xml matches found in page.

library(tidyverse)
library(httr)
library(xml2)
library(lubridate)


#### Global Variables, to set ####

start_date = "01-11-2016" #dmy format
end_date = "31-03-2017"
#will match any of the search terms (edit variable search_regexp in script to change that)
search_query = c("fake news", "trump", "inauguration")
#set to FALSE to turn off search
search_on = TRUE
#set to FALSE to only search in title
search_description = FALSE
#domain name base
url_base = "https://www.breitbart.com"
#set this vector to search within topics. supports searching multiple topics
topic_strings = c("politics/", "the-media/")

#what strings to use?
#the breitbart website is arranged first by topic, then date
#so you need to provide a topic date string
#best found by just browsing the site
#but some examples given here
example_strings = c("politics/",
                    "entertainment/",
                    "the-media",
                    "clips/",
                    "radio/",
                    "economy/",
                    "tech/",
                    "sports/",
                    "social-justice/",
                    "europe/",
                    "middle-east/",
                    "world-news/")
                    

#### Functions ####

#function to get all titles and urls for a given day
#takes date (as class date), and a topic string
#returns tbl with columns: title, url, source, date_accessed

fetch_day_titles = function(day, topic_string) {
  
  #error return
  day_results_error_tbl = tibble(title = NA,
                                 descriptions = NA,
                                 url = NA)
  
  #make and get url. url_base is set globally
  day_url = str_c(url_base, "/", topic_string, strftime(day, format = "%Y/%m/%d/"))
  response = GET(day_url)
  
  #on GET error print message and return error tbl
  if (http_error(response)) {
    warning("GET call to ", day_url, " returned error:\n\t", http_status(response), "\n")
    return(day_results_error_tbl)
  } else {
    message("Fetched titles under topic ", topic_string, " for: ", strftime(day, format = "%d/%m/%Y"))
  }
  
  # parse
  day_page = read_html(response)
  
  day_page_titles = day_page %>%
    xml_find_all("//div[@class='tC']/h2/a") %>%
    xml_text()
  
  #sometimes more than 1 description paragraph, so make a list of all <div>s then iterate joining sub-paras
  day_page_descriptions = day_page %>%
    xml_find_all("//div[@class='tC']/div[@class='excerpt']") %>%
    map_chr(~ xml_find_all(., ".//p") %>%  xml_text() %>% str_c(collapse = "\n"))
  
  day_page_urls = day_page %>%
    xml_find_all("//div[@class='tC']/h2/a") %>%
    xml_attr("href")
  
  #then return tibble of titles and urls, with error handling
  day_results_tbl = tryCatch({
    bind_cols(title = day_page_titles,
              description = day_page_descriptions,
              url = str_c(url_base, day_page_urls)) %>%
      return
  }, error = function(e) {
    warning("Error:\n\t", e, "\nCould not parse titles and/or urls for day ", strftime(day, format = "%d/%m/%Y"), "\nProbably because XML tags not recognised.\n")
    return(day_results_error_tbl)
  })
  
  return(day_results_tbl)
}

#function to get and parse one article
#takes url as string; returns a tbl

fetch_and_parse_article = function(url) {
  
  #error return
  article_error_tbl = tibble(published = NA,
                             last_modified = NA,
                             author = NA,
                             keywords = NA,
                             text = NA,
                             time_accessed = now(),
                             url = url)
  
  #if NA url, return error tbl
  if (is.na(url)) {
    warning("URL is a NA: returning NA tibble")
    return(article_error_tbl)
  }
  
  #fetch url
  response = GET(url)
  
  #on GET error, return error tbl.
  if (http_error(response)) {
    warning("GET call to ", url, " returned error:\n\t", http_status(response))
    return(article_error_tbl)
  } else {
    message("Fetched article from ", url)
  }
  
  #otherwise parse article
  article = read_html(response)
  
  #get info
  published = article %>%
    xml_find_first("//meta[@name='pubdate']") %>%
    xml_attr("content") %>%
    ymd_hms()
  
  last_modified = article %>%
    xml_find_first("//meta[@name='lastmod']") %>%
    xml_attr("content") %>%
    ymd_hms()
  
  author = article %>%
    xml_find_first("//meta[@name='author']") %>%
    xml_attr("content") 
  
  keywords = article %>%
    xml_find_first("//meta[@name='news_keywords']") %>%
    xml_attr("content")
  
  #get text, put into tbl
  text = article %>%
    xml_find_all("//div[@class='entry-content']//p") %>%
    xml_text() %>%
    #bind into one text block with paragraph separator
    str_c(collapse = "\n")
  #list of length 0, returned by xml_find_all() in event of no matches, causes later errors
  if (length(text) == 0) text = NA
  
  #make article tbl. catch errors if xml has gone funny.
  tryCatch({article_tbl = list(published = published,
                               last_modified = last_modified,
                               author = author,
                               keywords = keywords,
                               text = text,
                               time_accessed = now(),
                               url = url)},
           error = function(e) {
             warning("Error parsing html from the following url. Returning NAs. ", url)
             return(article_error_tbl)
           })
  
  return(article_tbl)
}


#### Main ####

#1) create list of dates to get
days_to_fetch = seq(dmy("07-02-2017"), dmy(end_date), by = "days") %>%
  #avoid conversion to numerical in the for loop below by converting to chararacter
  #then reconvert when calling fetch_day_titles
  strftime(format = "%d-%m-%Y") 
list_fetched_titles = fetched_titles
#2) fetch the titles and urls. 
#for loops used to keep memory in variable fetched_titles in event of fatal error
fetched_titles = tibble()

for (day in days_to_fetch) {
  for (topic in topic_strings) {
    fetched_data = fetch_day_titles(dmy(day), topic)
    new_row = list(day = dmy(day),
                   topic = topic,
                   fetched_data = list(fetched_data)) #list column of tbls
    fetched_titles = bind_rows(fetched_titles, new_row)
  }
}

#unnest the list column
fetched_titles = fetched_titles %>%
  unnest(fetched_data)

#display results
message("Fetched titles:")
fetched_titles

#3) keyword search
if (search_on) {
  #(?i) is a case-insensitive flag.
  search_regexp = str_c("(?i)(", str_c(search_query, collapse = "|"), ")")
  if (search_description) {
    titles_to_get = fetched_titles %>%
      filter(str_detect(title, search_regexp) | str_detect(description, search_regexp))
  } else {
    titles_to_get = fetched_titles %>%
      filter(str_detect(title, search_regexp))
  }
} else {
  titles_to_get = fetched_titles
}

#4) fetch the articles.
fetched_articles = tibble()

for (url in titles_to_get$url) {
  fetched_data = fetch_and_parse_article(url)
  fetched_articles = bind_rows(fetched_articles, fetched_data)
}

#5) add those back into titles_tbl
full_scrape = left_join(titles_to_get, fetched_articles, by = "url")

#display results
message("Fetched articles:")
full_scrape

#6) write to file
filename = str_c("breitbart_from_",
                 start_date, "_to_", end_date,
                 "_topics_",
                 str_remove_all(topic_strings, "/") %>% str_c(collapse = "-"),
                 "_search_terms_",
                 str_c(search_query, collapse = "-"),
                 ".csv",
                 sep = "")

write_csv(full_scrape, filename)
message("Written results to : ", filename)

#7) if desired write all titles to separate file
filename_titles = str_c("breitbart_titles_from_",
                        start_date, "_to_", end_date,
                        "_topics_",
                        str_remove_all(topic_strings, "/") %>% str_c(collapse = "-"),
                        ".csv",
                        sep = "")

write_csv(fetched_titles, filename_titles)
message("Written titles to : ", filename_titles)

### END ###



