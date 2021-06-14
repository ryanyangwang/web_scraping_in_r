# MedHelp (https://www.medhelp.org)

## Pakcages required
library(rvest)
library(dplyr)
library(stringr)
library(purrr)
library(RCurl)
library(tidyr)

## Setting up functions
scrape_thread_titles <- function(html){
  read_html(html) %>%
    html_nodes(".subj_title a") %>%
    html_text()
}

scrape_thread_links <- function(html){
  read_html(html) %>%
    html_nodes(".subj_title a") %>%
    html_attr("href") %>%
    paste0("https://www.medhelp.org", .)
}

scrape_poster_ids <- function(html){
  read_html(html) %>%
    html_nodes(css = "span span") %>%
    html_text()
}

scrape_posts <- function(html){
  read_html(html) %>%
    html_nodes(".resp_body , #subject_msg") %>%
    html_text() %>%
    str_replace_all("\r|\n", "") %>%
    str_trim()
}

scrape_poster_dates <- function(html){
  read_html(html) %>%
    html_nodes(".subj_header .mh_timestamp, .resp_info .mh_timestamp") %>%
    html_attr("datetime")
}


## Scraping Page 1 on the Coronavirus Community
page_html_1 <- getURL("https://www.medhelp.org/forums/Coronavirus/show/2203?page=1")

thread_titles <- map(page_html_1, scrape_thread_titles) %>%
  discard(~ length(.x) == 0)

correct_n_pages <- length(thread_titles)

thread_titles <- thread_titles %>%
  flatten_chr()

thread_links <- map(page_html_1, scrape_thread_links) %>%
  `[`(seq_len(correct_n_pages)) %>%
  flatten_chr()

master_data_page1 <- tibble(thread_titles, thread_links)

thread_htmls <- map_chr(master_data_page1$thread_links, getURL)

html <- thread_htmls[1]

link <- master_data_page1$thread_links[1]

medhelp_post_page1 <- master_data_page1 %>%
  mutate(
    poster_ids = map(thread_htmls, scrape_poster_ids),
    posts = map(thread_htmls, scrape_posts),
    date = map(thread_htmls, scrape_poster_dates)
  ) %>%
  unnest(cols = c(poster_ids, posts, date))



