# RStudio Table Contest
# 2022

# Load packages ----

library(tidyverse)
library(gt)
library(gtExtras)

# Links ----

# https://bjnnowak.netlify.app/2021/10/04/r-beautiful-tables-with-gt-and-gtextras/

# Scraping the data from wikipedia ----

url <- "https://en.wikipedia.org/wiki/List_of_Apollo_missions"

webpage <- rvest::read_html(url)

tables <- rvest::html_nodes(webpage, "table.wikitable") %>%
  rvest::html_table(header = TRUE, na.strings = c(NA, ""), convert = TRUE)

raw_tbl <- tables[[6]]

rm(tables, webpage, url)

# Cleaning the dataset ----

d1 <- raw_tbl %>% 
  # clean column names
  janitor::clean_names() %>%
  # select columns
  dplyr::select(-c(launch_vehicle_b, refs)) %>%
  # filter missions : missions with moon landings
  dplyr::filter(mission %in% c("Apollo 11", "Apollo 12", "Apollo 14",  
                               'Apollo 15', "Apollo 16", "Apollo 17")) %>% 
  # split launch_date into date, time and site
  tidyr::separate(launch_date,
                  into = c("launch_date", "launch_time", "launch_site"),
                  sep = "\n") %>% 
  # remove launch site
  dplyr::select(-launch_site) %>% 
  # define launch_date as date
  dplyr::mutate(launch_date = lubridate::mdy(launch_date)) %>% 
  # define launch_time as time
  dplyr::mutate(launch_time = lubridate::hm(launch_time)) %>% 
  # in crew column, remove strings between parentheses
  dplyr::mutate(crew = stringr::str_replace(crew, " \\s*\\([^\\)]+\\)", "")) %>% 
  # remove "Buzz" from crew for Apollo 11
  dplyr::mutate(crew = stringr::str_remove(crew, "\"Buzz\"")) %>% 
  # remove white spaces from crew column
  dplyr::mutate(crew = stringr::str_replace_all(crew, fixed(" "), "")) %>% 
  # split crew string on capital letters
  dplyr::mutate(crew = stringr::str_replace_all(crew, "([[:upper:]])", " \\1")) %>% 
  # remove white spaces from left and right of crew string
  dplyr::mutate(crew = stringr::str_trim(crew)) %>% 
  # split crew into commander, cm pilot and lm pilot
  dplyr::mutate(commander = stringr::word(crew, 1L, 2L),
                cm_pilot = dplyr::case_when(mission == "Apollo 12" ~ stringr::word(crew, 3L, 6L),
                                            TRUE ~ stringr::word(crew, 3L, 4L)),
                lm_pilot = dplyr::case_when(mission == "Apollo 12" ~ stringr::word(crew, 7L, 8L),
                                            TRUE ~ stringr::word(crew, 5L, 6L))) %>% 
  # select columns
  dplyr::select(mission:launch_time, commander, lm_pilot, cm_pilot,
                cm_name:remarks)
  
  



# Cleaning the dataset - crew ----

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

readr::write_csv(d5, "clean_data.csv")

# Cleaning dataset - add patches ----

# d6 <- d5 %>% 
#   mutate(patch = c("https://upload.wikimedia.org/wikipedia/commons/3/35/Apollo_1_patch.png",
#                    "https://upload.wikimedia.org/wikipedia/commons/3/34/AP7lucky7.png",
#                    "https://upload.wikimedia.org/wikipedia/commons/8/8b/Apollo-8-patch.png",
#                    "https://upload.wikimedia.org/wikipedia/commons/0/07/Apollo-9-patch.png",
#                    "https://upload.wikimedia.org/wikipedia/commons/6/64/Apollo-10-LOGO.png",
#                    "https://upload.wikimedia.org/wikipedia/commons/2/27/Apollo_11_insignia.png",
#                    "https://upload.wikimedia.org/wikipedia/commons/8/8d/Apollo_12_insignia.png",
#                    "https://upload.wikimedia.org/wikipedia/commons/a/ac/Apollo_13-insignia.png",
#                    "https://upload.wikimedia.org/wikipedia/commons/d/d7/Apollo_14-insignia.png",
#                    "https://upload.wikimedia.org/wikipedia/commons/e/e5/Apollo_15-insignia.png",
#                    "https://upload.wikimedia.org/wikipedia/commons/6/66/Apollo-16-LOGO.png",
#                    "https://upload.wikimedia.org/wikipedia/commons/7/76/Apollo_17-insignia.png")
#   )

# Creating the table ----

d1 <- readr::read_csv("clean_data.csv")

d1 %>% 
  mutate(duration_seconds = as.numeric(word(duration, 1, sep = "s"))) %>% 
  select(mission, launch_date, duration_seconds) %>% 
  pivot_longer(cols = launch_date:duration_seconds, names_to = "param", values_to = "value")


# Testing {gt} package ----

d6 %>% 
  mutate(crew = paste(commander, cm_pilot, lm_pilot, sep = "<br>")) %>% 
  mutate(hourglass = "https://static.thenounproject.com/png/1279503-200.png") %>% 
  mutate(calendar = "https://static.thenounproject.com/png/1068769-200.png") %>% 
  select(mission, crew, hourglass, calendar, duration) %>% 
  gt() %>% 
  fmt_markdown(columns = crew) %>% 
  gt_img_rows(columns = hourglass) %>% 
  gt_img_rows(columns = calendar)



%>% 
  gt() %>% 
  fmt_markdown(columns = crew) %>% 
  gt_img_rows(columns = hourglass)

  gt_img_rows(columns = clock, img_source = "web", height = 50) %>% 
  fmt_markdown(columns = team)

gtsave(tab, "tab.png")

  select(mission, commander, cm_pilot, lm_pilot) %>% 
  
  gt()

d6 %>% 
  select(mission, patch) %>% 
  gt() %>% 
  gt_img_rows(columns = patch, img_source = "web", height = 50)
  


d5 %>% 
  mutate(duration_seconds = as.numeric(word(duration, 1, sep = "s"))) %>% 
  select(mission, duration_seconds) %>% 
  gt() %>% 
  gt_plt_bar(column = duration_seconds, color = "blue", text_color = "dark")

test <- d5 %>% 
  select(mission, launch_date) %>%
  mutate(min_date = min(launch_date),
         max_date = max(launch_date))

total_days <- lubridate::interval(start = test$min_date, end = test$max_date) %/% lubridate::days() %>% 
  unique()

head(test)

mission_days <- abs(lubridate::interval(test$launch_date, test$min_date) %/% lubridate::days())
mission_days

test <- test %>% 
  mutate(nb_days = mission_days)

test %>% 
  mutate(ct = 1) %>% 
  complete(mission, nb_days, fill = list(ct = 1:2116))

test2 <- test %>% 
  select(nb_days, mission)

launch_timeline <- tibble(
  day = 0:total_days) %>% 
  left_join(test2, by = c("day" = "nb_days"))

launch_timeline2 <- launch_timeline %>% 
  mutate(ct = case_when(!is.na(mission) ~ 1,
                        TRUE ~ 0)) %>% 
  complete(day, mission, fill = list(ct = 0)) %>% 
  group_by(mission) %>% 
  summarise(Timeline = list(ct))

test_timeline <- d5 %>% 
  left_join(launch_timeline2) %>% 
  select(mission, Timeline)

test_timeline %>% 
  gt() %>% 
  gtExtras::gt_plt_sparkline(
    Timeline, label = FALSE, palette = c("red", "#ABB4C4", "#ABB4C4", "red", "#ABB4C4"))
