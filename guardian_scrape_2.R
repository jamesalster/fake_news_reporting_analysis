#### guardian scrape script, version 2 ####

#To use, set the global variables at the head of the script
#Setting a large date range takes AGES because it fetches ALL the published titles, then searches within them
#Search is a case-insensitive string match in title of article
#Because of guardian site organisation, you have to select topics to search within.
#Decent error handling for e.g url not found, no xml matches found in page.
#Date-times are gmt
#The script definitely could be improved by modifying the xpath search strings.

library(tidyverse)
library(httr)
library(xml2)
library(lubridate)

#### Global Variables, to set ####

#only articles from 2003 and afterwards available this way; for others, see 
#https://www.theguardian.com/info/2017/jun/26/how-to-access-guardian-and-observer-digital-archive
start_date = "01-11-2016" #dmy format
end_date = "31-03-2017"
#will match any of the search terms (edit variable search_regexp in script to change that)
search_query = c("fake news", "trump", "inauguration")
#set to FALSE to turn off search
search_on = TRUE
#domain name base
url_base = "https://www.theguardian.com/"
#set this vector to search within topics. supports searching multiple topics
topic_strings = c("commentisfree/", "profile/editorial/", "world/", "politics/", "us-news/", "media/")

#what strings to use?
#the guardian website is arranged first by topic, then date
#so you need to provide a topic date string
#best found by just browsing the site
#but some examples given here
example_strings = c("commentisfree/",
                    "culture/",
                    "politics/",
                    "profile/editorial/",
                    "uk-news/",
                    "education/",
                    "society/",
                    "uk/business/",
                    "us/business/",
                    "lifeandstyle/",
                    "tone/",
                    "world/",
                    "us-news/",
                    "food/",
                    "sport/")

#### Functions ####

#function to get all titles and urls for a given day
#takes date (as class date), and a topic string
#returns tbl with columns: title, url, source, date_accessed

fetch_day_titles = function(day, topic_string) {

  #error return
  day_results_error_tbl = tibble(title = NA,
                                 url = NA)
  
  #make and get url. url_base is set globally
  day_url = str_c(url_base, topic_string, strftime(day, format = "%Y/%b/%d/"), "all")
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
    xml_find_all("//span[@class='js-headline-text']") %>%
    xml_text()
  day_page_urls = day_page %>%
    xml_find_all("//a[@class='fc-item__link']") %>%
    xml_attr("href")
  
  #then return tibble of titles and urls, with error handling
  day_results_tbl = tryCatch({
    bind_cols(title = day_page_titles,
              url = day_page_urls) %>%
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
                             description = NA,
                             author = NA,
                             keywords = NA,
                             text = NA,
                             captions = NA,
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
    xml_find_first("//time[@itemprop='datePublished']") %>% #sometimes > 1
    xml_attr("datetime") %>%
    ymd_hms()
  
  description = article %>%
    xml_find_first("//meta[@property='og:description']") %>%
    xml_attr("content")
  
  author = article %>%
    xml_find_first("//meta[@name='author']") %>%
    xml_attr("content") 
  #there are a few irregularities with the author, some alternative XPath
  if(length(author) == 0) {
    author = article %>%
      xml_find_first("//meta[@property='article:author']") %>%
      xml_attr("content")
  }
  
  keywords = article %>%
    xml_find_first("//meta[@name='keywords']") %>%
    xml_attr("content")
  
  #get text, put into tbl
  text = article %>%
    xml_find_all("//div[@itemprop='articleBody']//p") %>%
    xml_text() %>%
    #bind into one text block with paragraph separator
    str_c(collapse = "\n")
  #list of length 0, returned by xml_find_all() in event of no matches, causes later errors
  if (length(text) == 0) text = NA
  
  #get captions
  captions = article %>%
    xml_find_all("//figcaption[@itemprop='description']") %>%
    xml_text() %>%
    str_c(collapse = "\n")
  if (length(captions) == 0) captions = NA
  
  #make article tbl. catch errors if xml has gone funny.
  tryCatch({article_tbl = list(published = published,
                                 description = description,
                                 author = author,
                                 keywords = keywords,
                                 text = text,
                                 captions = captions,
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
days_to_fetch = seq(dmy(start_date), dmy(end_date), by = "days") %>%
  #avoid conversion to numerical in the for loop below by converting to chararacter
  #then reconvert when calling fetch_day_titles
  strftime(format = "%d-%m-%Y") 

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

#3) title search
#(the search could also use a field on the archive-date page, a bit like a description
#it would have XPath: "//div[@class=fc-item__standfirst]"
#but not all articles have it, so i've not used it.)

if (search_on) {
  #(?i) is a case-insensitive flag.
  search_regexp = str_c("(?i)(", str_c(search_query, collapse = "|"), ")")
  titles_to_get = fetched_titles %>%
    filter(str_detect(title, search_regexp))
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
filename = str_c("guardian_articles_from_",
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
filename_titles = str_c("guardian_titles_from_",
                 start_date, "_to_", end_date,
                 "_topics_",
                 str_remove_all(topic_strings, "/") %>% str_c(collapse = "-"),
                 ".csv",
                 sep = "")

write_csv(fetched_titles, filename_titles)
message("Written titles to : ", filename_titles)

### END ###
