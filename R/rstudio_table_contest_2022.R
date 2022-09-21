# RStudio Table Contest
# 2022

# Load packages ----

library(tidyverse)
library(gt)
library(gtExtras)

# Links ----

# https://bjnnowak.netlify.app/2021/10/04/r-beautiful-tables-with-gt-and-gtextras/

# Creating the dataset ----

url <- "https://en.wikipedia.org/wiki/List_of_Apollo_missions"

webpage <- rvest::read_html(url)

tables <- rvest::html_nodes(webpage, "table.wikitable") %>%
  rvest::html_table(header = TRUE, na.strings = c(NA, ""), convert = TRUE)

raw_tbl <- tables[[6]]

rm(tables, webpage, url)

# Cleaning the dataset - column names ----

d1 <- raw_tbl %>% 
  janitor::clean_names() %>% 
  dplyr::select(mission:crew,
                launch_vehicle = launch_vehicle_b,
                everything())

# Cleaning the dataset - launch date + time ----

d2 <- d1 %>% 
  dplyr::mutate(launch_date = str_replace(launch_date, " \\s*\\([^\\)]+\\)", "")) %>% 
  tidyr::separate(launch_date,
                  into = c("launch_date", "launch_time", "launch_site"),
                  sep = "\n") %>% 
  dplyr::mutate(launch_site = case_when(mission == "Apollo 1" ~ launch_time,
                                        TRUE ~ launch_site),
                launch_time = case_when(mission == "Apollo 1" ~ NA_character_,
                                        TRUE ~ launch_time)) %>% 
  dplyr::mutate(launch_date = lubridate::mdy(launch_date),
                launch_time = lubridate::hm(launch_time))

# Cleaning the dataset - crew ----

d3 <- d2 %>% 
  dplyr::mutate(crew = str_replace(crew, " \\s*\\([^\\)]+\\)", ""),
                crew = gsub('"', "", crew),
                crew = str_remove(crew, " Buzz")) %>% 
  dplyr::mutate(crew = stringr::str_replace_all(crew, fixed(" "), "")) %>% 
  dplyr::mutate(crew = stringr::str_replace_all(crew, "([[:upper:]])", " \\1")) %>% 
  dplyr::mutate(crew = stringr::str_trim(crew)) %>% 
  dplyr::mutate(commander = case_when(mission %in% c("Apollo 9", "Apollo 10") ~ stringr::word(crew, 1L, 3L),
                                      TRUE ~ stringr::word(crew, 1L, 2L)),
                cm_pilot = case_when(mission == "Apollo 7" ~ stringr::word(crew, 3L, 5L),
                                                 mission == "Apollo 12" ~stringr::word(crew, 3L, 6L),
                                                 mission %in% c("Apollo 9", "Apollo 10") ~ stringr::word(crew, 4L, 5L),
                                                 TRUE ~ stringr::word(crew, 3L, 4L)),
                lm_pilot = case_when(mission == "Apollo 1" ~ stringr::word(crew, 5L, 7L),
                                               mission == "Apollo 12" ~ stringr::word(crew, 7L, 8L),
                                               mission %in% c("Apollo 7", "Apollo 9", "Apollo 10") ~ stringr::word(crew, 6L, 7L),
                                               TRUE ~ stringr::word(crew, 5L, 6L))) %>% 
  dplyr::select(mission:launch_site, commander:lm_pilot, launch_vehicle:remarks)

# Cleaning the dataset - cm & lm names : add NAs ----

d4 <- d3 %>% 
  dplyr::mutate(cm_name = case_when(nchar(cm_name) < 2 ~ NA_character_,
                                    TRUE ~ cm_name),
                lm_name = case_when(nchar(lm_name) < 2 ~ NA_character_,
                                    TRUE ~ lm_name))

# Cleaning the dataset - duration ----

d5 <- d4 %>% 
  dplyr::mutate(duration_2 = stringr::str_remove_all(duration, " [a-z]"),
                duration_2 = stringr::str_remove(duration_2, "d")) %>% 
  tidyr::separate(duration_2, into = c("d", "h", "m", "s"), sep = " ") %>% 
  dplyr::mutate(d = stringr::str_remove(d, "^0"),
                h = stringr::str_remove(h, "^0"),
                m = stringr::str_remove(m, "^0"),
                s = stringr::str_remove(s, "^0")) %>% 
  dplyr::mutate(duration = case_when(nchar(duration) < 2 ~ NA_character_,
                                     TRUE ~ duration),
                d = case_when(mission == "Apollo 1" ~ NA_character_,
                              TRUE ~ d)) %>% 
  dplyr::mutate(duration_corrected = paste0(d, "d ", h, "H ", m, "M ", s, "S")) %>% 
  dplyr::mutate(duration = lubridate::duration(duration_corrected)) %>% 
  dplyr::select(mission:remarks)
  
# Testing {gt} package ----

d5 %>% 
  mutate(duration_seconds = as.numeric(word(duration, 1, sep = "s"))) %>% 
  select(mission, duration_seconds) %>% 
  gt() %>% 
  gt_plt_bar(column = duration_seconds, color = "blue", text_color = "dark")

d5 %>% 
  select(mission, launch_date)

mtcars %>% 
  group_by(cyl) %>% 
  summarise(mpg_data = list(mpg), .groups = "drop") %>% 
  gt() %>% 
  gt_plt_sparkline(mpg_data, type = "default")


bullet_df <- tibble::rownames_to_column(mtcars) %>%
  dplyr::select(rowname, cyl:drat, mpg) %>%
  dplyr::group_by(cyl) %>%
  dplyr::mutate(target_col = mean(mpg)) %>%
  dplyr::slice_sample(n = 3) %>% 
  ungroup()

bullet_df

bullet_df %>%
  gt() %>%
  gt_plt_bullet(column = mpg, target = target_col, width = 45,
                palette = c("lightblue", "black"))

d5 %>% 
  select(mission, launch_date) %>% 
  mutate(timeline = max(launch_date)) %>% 
  gt() %>% 
  gt_plt_bullet(column = timeline, target = launch_date)
