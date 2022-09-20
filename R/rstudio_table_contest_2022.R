# RStudio Table Contest
# 2022

# Load packages ----

library(tidyverse)
library(datefixR)
library(gt)

# Creating the dataset ----

url <- "https://en.wikipedia.org/wiki/List_of_Apollo_missions"

webpage <- rvest::read_html(url)

tables <- rvest::html_nodes(webpage, "table.wikitable") %>%
  rvest::html_table(header = TRUE, na.strings = c(NA, ""), convert = TRUE)

raw_tbl <- tables[[6]]

clean_tbl <- raw_tbl %>% 
  janitor::clean_names() %>% 
  dplyr::select(-refs) %>% 
  tidyr::separate(launch_date, into = c("date", "time", "site"), sep = "\n") %>% 
  dplyr::mutate(date = lubridate::mdy(date),
                site = case_when(mission == "Apollo 1" ~ time,
                                 TRUE ~ as.character(site)),
                time = case_when(mission == "Apollo 1" ~ NA_character_,
                                 TRUE ~ as.character(time)),
                site = str_replace(site, " \\s*\\([^\\)]+\\)", ""))

str_replace_all(clean_tbl$crew,  "((?<=[a-z])[A-Z]|[A-Z](?=[a-z]))",  " \\1")


test <- clean_tbl %>% 
  separate(duration, into = c("d", "h", "m", "s"), sep = "[dhms]")

%>% 
  tidyr::separate(crew, into = c("crew1", "crew2", "crew3"), sep = "\n")
  

head(clean_tbl)

%>% 
  dplyr::mutate(site = case_when(mission == "Apollo 1" ~ time,
                                 TRUE ~ as.character(site))) %>% 
  dplyr::mutate(time = case_when(mission == "Apollo 1" ~ NA_character_,
                                 TRUE ~ as.character(time))) %>% 
  dplyr::mut

clean_tbl$time[2] %>% 
  str_remove(" GMT") %>% 
  str_replace(":", "-")

  lubridate::ymd_hm()


# Testing {gt} package ----

datardis::drwho_episodes %>% 
  gt()
