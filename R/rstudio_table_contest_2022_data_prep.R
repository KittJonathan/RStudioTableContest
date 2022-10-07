# RStudio Table Contest
# 2022
# Data preparation
# Last updated 2022-10-07

# Load packages ----

# library(gt)
# library(gtExtras)
library(lubridate)
library(tidyverse)


# Scrape the data from wikipedia ----

url <- "https://en.wikipedia.org/wiki/List_of_Apollo_missions"

webpage <- rvest::read_html(url)

tables <- rvest::html_nodes(webpage, "table.wikitable") %>%
  rvest::html_table(header = TRUE, na.strings = c(NA, ""), convert = TRUE)

raw_tbl <- tables[[6]]

rm(tables, webpage, url)

# Create splashdown maps ----

world <- ggplot2::map_data("world") %>% 
  dplyr::filter(region != "Antarctica")

splashdown_data <- dplyr::tibble(
  mission = paste0("Apollo ", c(11, 12, 14:17))) |> 
  # add splashdown coordinates
  # add splashdown coordinates
  dplyr::mutate(splashdown_x = c(-169.15, -165.15, -172.65, -158.13, -156.22, -166.18),
                splashdown_y = c(13.32, -15.78, -27.02, 26.12, -0.72, -18.47))

splashdown_data

for (i in 1:nrow(d1)) {
  
  nb_mission <- stringr::word(d1$mission[i], 2L, 2L)
  
  p <- ggplot() +
    geom_polygon(data = world,
                 aes(x = long, y = lat, group = group),
                 colour = "lightgrey", fill = "lightgrey") +
    geom_point(data = filter(splashdown_data, row_number() == i),
               aes(x = splashdown_x, y = splashdown_y),
               colour = "red", size = 20) +
    coord_fixed(ratio = 1.3,
                xlim = c(-175, -35),
                ylim = c(-30, 40)) +
    theme_void() +
    theme(panel.background = element_rect(fill = "#333333", colour = "#333333"),
          plot.background = element_rect(fill = "#333333", colour = "#333333"))
  
  ggsave(paste0("img/splashdown_apollo_", nb_mission , ".png"), p, dpi = 320, width = 12, height = 6)
  
}


# Preparing the dataset ----

d1 <- raw_tbl |> 
  # clean column names
  janitor::clean_names() |>
  # select columns
  dplyr::select(mission, crew, launch_date, duration) |> 
  # filter missions
  dplyr::filter(mission %in% c(paste("Apollo", c(11, 12, 14:17)))) |> 
  # remove strings between parentheses in crew column
  dplyr::mutate(crew = stringr::str_replace(crew, " \\s*\\([^\\)]+\\)", "")) |> 
  # remove "Buzz" surname from Apollo 11 crew
  dplyr::mutate(crew = stringr::str_remove(crew, "\"Buzz\"")) |> 
  # remove white spaces from crew column
  dplyr::mutate(crew = stringr::str_replace_all(crew, fixed(" "), "")) |> 
  # split crew string on capital letters
  dplyr::mutate(crew = stringr::str_replace_all(crew, "([[:upper:]])", " \\1")) |> 
  # remove white spaces from left and right of crew string
  dplyr::mutate(crew = stringr::str_trim(crew)) |> 
  # split crew into commander, cm pilot and lm pilot
  dplyr::mutate(commander = stringr::word(crew, 1L, 2L),
                cm_pilot = dplyr::case_when(mission == "Apollo 12" ~ stringr::word(crew, 3L, 6L),
                                            TRUE ~ stringr::word(crew, 3L, 4L)),
                lm_pilot = dplyr::case_when(mission == "Apollo 12" ~ stringr::word(crew, 7L, 8L),
                                            TRUE ~ stringr::word(crew, 5L, 6L))) |> 
  # regroup crew in one column
  dplyr::mutate(crew = paste(commander, lm_pilot, cm_pilot, sep = "<br>")) |> 
  # select columns
  dplyr::select(mission, crew, launch_date, duration) |> 
  # split launch_date into date, time and site
  tidyr::separate(launch_date,
                  into = c("launch_date", "launch_time", "launch_site"),
                  sep = "\n") |> 
  # remove launch_site column
  dplyr::select(-launch_site) |> 
  # remove characters from duration
  dplyr::mutate(duration = stringr::str_remove_all(duration, " [a-z]")) |> 
  # remove "d" from duration for Apollo 17
  dplyr::mutate(duration = stringr::str_remove(duration, "d")) |> 
  # split duration into d, h, m, s
  tidyr::separate(duration, into = c("d", "h", "m", "s"), sep = " ") |> 
  # remove lead zeros from d, h, m, s
  dplyr::mutate(across(d:s, ~ stringr::str_remove(., "^0"))) |> 
  # regroup duration information
  dplyr::mutate(mission_duration = paste0(d, "d ", h, "h ", m, "m ")) |> 
  # select columns
  dplyr::select(mission, mission_duration, crew, launch_date, launch_time) |> 
  # add number of days spent on the moon
  dplyr::mutate(moon_on = c(lubridate::make_datetime(1969, 07, 20, 20, 17, 40),
                            lubridate::make_datetime(1969, 11, 19, 06, 54, 35),
                            lubridate::make_datetime(1971, 02, 05, 09, 18, 11),
                            lubridate::make_datetime(1971, 07, 30, 22, 16, 29),
                            lubridate::make_datetime(1972, 04, 21, 02, 23, 35),
                            lubridate::make_datetime(1972, 12, 11, 19, 54, 58)),
                moon_off = c(lubridate::make_datetime(1969, 07, 21, 17, 54, 00),
                             lubridate::make_datetime(1969, 11, 20, 14, 25, 47),
                             lubridate::make_datetime(1971, 02, 06, 18, 48, 42),
                             lubridate::make_datetime(1971, 08, 02, 17, 11, 23),
                             lubridate::make_datetime(1972, 04, 24, 01, 25, 47),
                             lubridate::make_datetime(1972, 12, 14, 22, 54, 37))) |> 
  dplyr::mutate(hours_on_the_moon = round(lubridate::interval(moon_on, moon_off) / lubridate::hours(1),
                                          digits = 0)) |> 
  # add numbers of lunar EVAs and mass of samples collected
  dplyr::mutate(lunar_evas = c(1, 2, 2, 3, 3, 3),
                samples_mass = round(c(21.55, 34.35, 42.80, 77, 95.71, 115), digits = 0)) |> 
  # select columns
  dplyr::select(mission:launch_time, hours_on_the_moon:samples_mass) |> 
  # add splashdown date and time
  dplyr::mutate(splashdown_date = c("July 24, 1969",
                                    "November 24, 1969",
                                    "February 9, 1971",
                                    "August 7, 1971",
                                    "April 27, 1972",
                                    "December 19, 1972"),
                splashdown_time = c("16:51 GMT",
                                    "20:58 GMT",
                                    "21:05 GMT",
                                    "20:46 GMT",
                                    "19:45 GMT",
                                    "19:25 GMT")) |> 
  # add mission patches
  dplyr::mutate(patch = c("img/apollo_11.png",
                          "img/apollo_12.png",
                          "img/apollo_14.png",
                          "img/apollo_15.png",
                          "img/apollo_16.png",
                          "img/apollo_17.png")) |> 
  # select columns
  dplyr::select(patch, dplyr::everything()) |> 
  # add splashdown maps paths
  dplyr::mutate(splashdown_map = paste0("img/splashdown_apollo_", c(11, 12, 14:17), ".png"))

# Save clean data ----

readr::write_csv(d1, "data/clean_data.csv")