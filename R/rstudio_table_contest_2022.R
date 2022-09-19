# RStudio Table Contest
# 2022

# Load packages ----

library(tidyverse)
library(gt)

# Creating the dataset ----

url <- "https://en.wikipedia.org/wiki/List_of_Apollo_missions"

webpage <- rvest::read_html(url)

tables <- rvest::html_nodes(webpage, "table.wikitable") %>%
  rvest::html_table(header = TRUE, na.strings = c(NA, ""), convert = TRUE)

d1 <- tables[[6]]

d1 <- d1 %>% 
  mutate(launch_info = str_split(`Launch date`, "\n"))


# Testing {gt} package ----

datardis::drwho_episodes %>% 
  gt()
