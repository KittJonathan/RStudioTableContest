# RStudio Table Contest
# 2022
# Last updated 2022-09-23

# Load packages ----

library(lubridate)
library(tidyverse)
library(gt)
library(gtExtras)

# Links ----

# https://bjnnowak.netlify.app/2021/10/04/r-beautiful-tables-with-gt-and-gtextras/
# https://en.wikipedia.org/wiki/List_of_Apollo_missions

# Scraping the data from wikipedia ----

url <- "https://en.wikipedia.org/wiki/List_of_Apollo_missions"

webpage <- rvest::read_html(url)

tables <- rvest::html_nodes(webpage, "table.wikitable") %>%
  rvest::html_table(header = TRUE, na.strings = c(NA, ""), convert = TRUE)

raw_tbl <- tables[[6]]

rm(tables, webpage, url)

# Cleaning the dataset ----

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
  dplyr::mutate(mission_duration = paste0(d, "d ", h, "h ", m, "m ", s, "s")) |> 
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
  dplyr::mutate(hours_on_the_moon = round(lubridate::interval(moon_on, moon_off) / lubridate::hours(1), digits = 1)) |> 
  # add numbers of lunar EVAs and mass of samples collected
  dplyr::mutate(lunar_evas = c(1, 2, 2, 3, 3, 3),
                samples_mass = c(21.55, 34.35, 42.80, 77, 95.71, 115)) |> 
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
                                    "05:33 GMT")) |> 
  # add paths to splashdown maps 
  dplyr::mutate(splashdown_map = paste0("img/splashdown_", 1:6, ".png"))
  
  

d1 |> 
  gt() |> 
  gt_merge_stack(col1 = mission, col2 = mission_duration,
                 palette = c("white", "grey"),
                 font_size = c("22px", "18px"),
                 font_weight = c("bold", "normal")) |> 
  fmt_markdown(crew) |> 
  gt_merge_stack(col1 = launch_date, col2 = launch_time,
                 palette = c("white", "grey"),
                 font_size = c("20px", "16px"),
                 font_weight = c("bold", "normal")) |> 
  gt_plt_bar(column = hours_on_the_moon, keep_column = FALSE, width = 25,
             scale_type = "number") |> 
  gt_merge_stack(col1 = splashdown_date, col2 = splashdown_time,
                 palette = c("white", "grey"),
                 font_size = c("20px", "16px"),
                 font_weight = c("bold", "normal")) |> 
  gt_img_rows(columns = splashdown_map, img_source = "local", height = 100) |> 
  gt_theme_dark() |> 
  tab_header(title = "There and back again",
             subtitle = "Apollo missions that landed on the moon") |> 
  gt::cols_label(launch_date = "Launch",
                 hours_on_the_moon = "Hours",
                 lunar_evas = "EVAs",
                 samples_mass = "Samples (kgs)",
                 splashdown_date = "",
                 splashdown_map = "") |> 
  tab_spanner(label = "BACK TO EARTH",
              columns = c(splashdown_date, splashdown_time, splashdown_map), replace = TRUE) |> 
  tab_spanner(label = "ON THE MOON",
              columns = c(hours_on_the_moon, lunar_evas, samples_mass), replace = TRUE)
  
  

|> 
  # add earth_off, moon_on, moon_off & earth_on date-times
  dplyr::mutate(earth_off = c(lubridate::make_datetime(1969, 07, 16, 13, 32, 00),
                              lubridate::make_datetime(1969, 11, 14, 16, 22, 00),
                              lubridate::make_datetime(1971, 01, 31, 21, 03, 00),
                              lubridate::make_datetime(1971, 07, 26, 13, 34, 00),
                              lubridate::make_datetime(1972, 04, 16, 17, 54, 00),
                              lubridate::make_datetime(1972, 12, 07, 05, 33, 00)),
                moon_on = c(lubridate::make_datetime(1969, 07, 20, 20, 17, 40),
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
                             lubridate::make_datetime(1972, 12, 14, 22, 54, 37)),
                earth_on = c(lubridate::make_datetime(1969, 07, 24, 16, 50, 35),
                             lubridate::make_datetime(1969, 11, 24, 20, 58, 24),
                             lubridate::make_datetime(1971, 02, 09, 21, 05, 00),
                             lubridate::make_datetime(1971, 08, 07, 20, 45, 53),
                             lubridate::make_datetime(1972, 04, 27, 19, 45, 05),
                             lubridate::make_datetime(1972, 12, 19, 05, 33, 00))) |> 
  # calculate mission duration in days & days spent on the moon
  dplyr::mutate(mission_duration_days = lubridate::interval(earth_off, earth_on) / lubridate::days(1),
                hours_on_the_moon = lubridate::interval(moon_on, moon_off) / lubridate::hours(1))

d1 |> 
  select(mission, mission_duration_days, hours_on_the_moon) |>
  gt() |> 
  fmt_duration(columns = mission_duration_days, input_units = "days")


d1 |> 
  select(mission, mission_duration_days, days_on_the_moon) |> 
  gt() |> 
  gt_plt_bar(column = mission_duration_days) |> 
  gt_plt_bar(column = days_on_the_moon)


|> 
  # calculate intervals : earth -> moon, on moon & moon -> earth
  dplyr::mutate(step1 = lubridate::interval(earth_off, moon_on) / lubridate::days(1),
                step2 = lubridate::interval(moon_on, moon_off) / lubridate::days(1),
                step3 = lubridate::interval(moon_off, earth_on) / lubridate::days(1),
                total = lubridate::interval(earth_off, earth_on) / lubridate::days(1)) |> 
  # calculate percent of time for each step of the mission
  dplyr::mutate(step1_pct = 100 * step1 / total,
                step2_pct = 100 * step2 / total,
                step3_pct = 100 * step3 / total) |> 
  # create list of step*_pct
  dplyr::group_by(mission) |> 
  dplyr::mutate(mission_steps = list(c(step1_pct, step2_pct, step3_pct))) |> 
  dplyr::ungroup()
  
d1  

d1 |> 
  select(mission, mission_days) |> 
  gt() |> 
  gt_plt_bar_stack(column = mission_days, position = "stack", width = 150, labels = c("Moon", "Travel"))

####


  # add lunar take-off date-time
  dplyr::mutate(lunar_takeoff_dt = c(lubridate::make_datetime(1969, 07, 21, 17, 54, 00),
                                     lubridate::make_datetime(1969, 11, 20, 14, 25, 47),
                                     lubridate::make_datetime(1971, 02, 06, 18, 48, 42),
                                     lubridate::make_datetime(1971, 08, 02, 17, 11, 23),
                                     lubridate::make_datetime(1972, 04, 24, 01, 25, 47),
                                     lubridate::make_datetime(1972, 12, 14, 22, 54, 37))) %>% 
  # add splashdown date-time
  dplyr::mutate(splashdown_dt = c(lubridate::make_datetime(1969, 07, 24, 16, 50, 35),
                                  lubridate::make_datetime(1969, 11, 24, 20, 58, 24),
                                  lubridate::make_datetime(1971, 02, 09, 21, 05, 00),
                                  lubridate::make_datetime(1971, 08, 07, 20, 45, 53),
                                  lubridate::make_datetime(1972, 04, 27, 19, 45, 05),
                                  lubridate::make_datetime(1972, 12, 19, 05, 33, 00))) %>% 
  # add nb of days from the earth to the moon surface
  dplyr::mutate(to_the_moon_days = lubridate::interval(launch_dt, lunar_landing_dt) / days(1)) %>% 
  # add nb of days on the moon
  dplyr::mutate(on_the_moon_days = lubridate::interval(lunar_landing_dt, lunar_takeoff_dt) / days(1)) %>% 
  # add splashdown coordinates
  dplyr::mutate(splashdown_x = c(-169.15, -165.15, -172.65, -158.13, -156.22, -166.18),
                splashdown_y = c(13.32, -15.78, -27.02, 26.12, -0.72, -18.47)) |> 
  # add mission duration
  dplyr::mutate(mission_duration = lubridate::interval(launch_dt, splashdown_dt) / days(1)) |> 
  # add mission duration details
  dplyr::mutate(earth_moon = lubridate::interval(launch_dt, lunar_landing_dt) / days(1),
                on_moon = lubridate::interval(lunar_landing_dt, lunar_takeoff_dt) / days(1),
                moon_earth = lubridate::interval(lunar_takeoff_dt, splashdown_dt) / days(1),
                earth_tkoff_lunar_tkoff = lubridate::interval(launch_dt, lunar_takeoff_dt) / days(1)) |> 
  dplyr::mutate(travel_days = earth_moon + moon_earth) |> 
  dplyr::group_by(mission) |> 
  dplyr::mutate(mission_details = list(c(earth_moon, earth_tkoff_lunar_tkoff))) |> 
  dplyr::ungroup() |> 
  dplyr::mutate(step1_pct = (lubridate::interval(launch_dt, lunar_landing_dt) / days(1)) / (lubridate::interval(launch_dt, splashdown_dt) / days(1)),
                step2_pct = lubridate::interval(lunar_landing_dt, lunar_takeoff_dt) / days(1) / (lubridate::interval(launch_dt, splashdown_dt) / days(1)),
                step3_pct = lubridate::interval(lunar_landing_dt, splashdown_dt) / days(1) / (lubridate::interval(launch_dt, splashdown_dt) / days(1))) |> 
  dplyr::mutate(check = step1_pct + step2_pct + step3_pct)


  # # add nb of days from the moon surface to splashdown
  # dplyr::mutate(to_the_earth_days = lubridate::interval(lunar_takeoff_dt, splashdown_dt) / days(1)) %>% 
  # # create list of nb of days
  # dplyr::group_by(mission) %>% 
  # dplyr::mutate(mission_duration = list(c(to_the_moon_days, on_the_moon_days, to_the_earth_days))) %>% 
  # dplyr::ungroup()

# # define launch_date as date
# dplyr::mutate(launch_date = lubridate::mdy(launch_date)) %>% 
#   # define launch_time as time
#   dplyr::mutate(launch_time = lubridate::hm(launch_time)) %>% 
#   
#   # select columns
#   dplyr::select(mission:launch_time, commander, lm_pilot, cm_pilot,
#                 cm_name:remarks) %>% 

#   # calculate mission duration in days
#   dplyr::mutate(duration_days = d + h/24 + m/1440 + s/86400) %>% 
#   # select columns
#   dplyr::select(mission:lm_name, duration_days, d:s) %>% 
#   # add lunar landing datatime, EVAs count + duration, sample mass and lunar take-off datetime
#   dplyr::mutate(lunar_landing = c(lubridate::make_datetime(1969, 07, 20, 20, 17, 40),
#                                   lubridate::make_datetime(1969, 11, 19, 06, 54, 35),
#                                   lubridate::make_datetime(1971, 02, 05, 09, 18, 11),
#                                   lubridate::make_datetime(1971, 07, 30, 22, 16, 29),
#                                   lubridate::make_datetime(1972, 04, 21, 02, 23, 35),
#                                   lubridate::make_datetime(1972, 12, 11, 19, 54, 58)),
#                 lunar_evas_count = c(1, 2, 2, 3, 3, 3),
#                 lunar_evas_duration = c(lubridate::duration("2H 31M 40S"),
#                                         lubridate::duration("7H 45M 18S"),
#                                         lubridate::duration("9H 22M 31S"),
#                                         lubridate::duration("18H 34M 46S"),
#                                         lubridate::duration("20H 14M 14S"),
#                                         lubridate::duration("22H 3M 57S")),
#                 sample_mass_kgs = c(21.55, 34.35, 42.8, 77, 95.71, 115),
                # lunar_takeoff = c(lubridate::make_datetime(1969, 07, 21, 17, 54, 00),
                #                   lubridate::make_datetime(1969, 11, 20, 14, 25, 47),
                #                   lubridate::make_datetime(1971, 02, 06, 18, 48, 42),
                #                   lubridate::make_datetime(1971, 08, 02, 17, 11, 23),
                #                   lubridate::make_datetime(1972, 04, 24, 01, 25, 47),
                #                   lubridate::make_datetime(1972, 12, 14, 22, 54, 37)),
#                 splashdown = c(lubridate::make_datetime(1969, 07, 24, 16, 50, 35),
#                                lubridate::make_datetime(1969, 11, 24, 20, 58, 24),
#                                lubridate::make_datetime(1971, 02, 09, 21, 05, 00),
#                                lubridate::make_datetime(1971, 08, 07, 20, 45, 53),
#                                lubridate::make_datetime(1972, 04, 27, 19, 45, 05),
#                                lubridate::make_datetime(1972, 12, 19, 05, 33, 00)),
#                 splashdown_long = c(-169.15, -165.15, -172.65, -158.13, -156.22, -166.18),
#                 splashdown_lat = c(13.32, -15.78, -27.02, 26.12, -0.72, -18.47)) %>% 
#   # add lunar landing coordinates
#   dplyr::mutate(landing_long = c(23.47, -23.42, -17.47, 3.63, 15.50, 30.77),
#                 landing_lat = c(0.67, -3.01, -3.65, 26.13, -8.97, 20.19)) %>% 
#   # add mission patches
#   # dplyr::mutate(patch = c("https://upload.wikimedia.org/wikipedia/commons/2/27/Apollo_11_insignia.png",
#   #                         "https://upload.wikimedia.org/wikipedia/commons/8/8d/Apollo_12_insignia.png",
#   #                         "https://upload.wikimedia.org/wikipedia/commons/d/d7/Apollo_14-insignia.png",
#   #                         "https://upload.wikimedia.org/wikipedia/commons/e/e5/Apollo_15-insignia.png",
#   #                         "https://upload.wikimedia.org/wikipedia/commons/6/66/Apollo-16-LOGO.png",
#   #                         "https://upload.wikimedia.org/wikipedia/commons/7/76/Apollo_17-insignia.png"))
#   dplyr::mutate(patch = c("img/apollo_11.png", "img/apollo_12.png", "img/apollo_14.png",
#                           "img/apollo_15.png", "img/apollo_16.png", "img/apollo_17.png"))
# 
# # add landing site names 
# dplyr::mutate(landing_site_name = c("Sea of Tranquility", "Ocean of Storms", 
#                                     "Fra Mauro", "Hadley-Apennine",
#                                     "Descartes Highlands", "Taurus-Littrow")) %>% 
#   # add number of lunar and deep space EVAs
#   dplyr::mutate(lunar_evas = c(1, 2, 2, 3, 3, 3),
#                 deep_space_evas = c(0, 0, 0, 1, 1, 1)) %>% 
#   # add lunar EVA durations 
#   dplyr::mutate(lunar_eva_h = c(2, NA, NA, NA, NA, NA),
#                 lunar_eva_m = c(31, NA, NA, NA, NA, NA)) %>% 
#   # add lunar rocks recolted mass
#   dplyr::mutate(lunar_rocks_kgs = c(21.55, 34.4, NA, NA, NA, NA)) %>% 
#   # add splashdown coordinates
#   dplyr::mutate(splashdown_x = c(-169.15, -23.42, -172.65, -158.13, -156.22, -166.18),
#                 splashdown_y = c(13.32, -3.01, -27.02, 26.12, -0.72, -18.47))

# Create splashdown maps ----

world <- map_data("world") %>% 
  filter(region != "Antarctica")

for (i in 1:nrow(d1)) {
  
  p <- ggplot() +
     geom_polygon(data = world,
                  aes(x = long, y = lat, group = group),
                  colour = "lightgrey", fill = "lightgrey") +
     geom_point(data = filter(d1, row_number() == i),
                aes(x = splashdown_x, y = splashdown_y),
                colour = "red", size = 20) +
     coord_fixed(ratio = 1.3,
                 xlim = c(-175, -35),
                 ylim = c(-30, 40)) +
     theme_void() +
     theme(panel.background = element_rect(fill = "#333333", colour = "#333333"),
           plot.background = element_rect(fill = "#333333", colour = "#333333"))
  
  ggsave(paste0("img/splashdown_", i, ".png"), p, dpi = 320, width = 12, height = 6)
  
}


# Create table ----

d1 %>% 
  select(mission:launch_time, mission_duration) %>% 
  mutate(splashdown_map = paste0("img/splashdown_", 1:6, ".png")) %>% 
  gt() %>% 
  fmt_markdown(crew) %>% 
  gt_merge_stack(col1 = mission, col2 = crew,
                 palette = c("white", "grey")) %>% 
  gt_merge_stack(col1 = launch_date, col2 = launch_time,
                 palette = c("white", "grey")) %>% 
  # gt_plt_bullet(column = mission_duration, target = mission_details) |> 
  # gt_plt_bar_stack(column = mission_duration,
  #                  position = "stack",
  #                  labels = c("To the moon", "On the moon", "Back to Earth"),
  #                  palette = c("lightblue", "darkgrey", "lightblue")) %>%
  # gt_plt_bar(column = travel_days, width = 25) %>%
  # gt_plt_bar(column = on_the_moon_days, width = 25) %>%
  # gt_plt_bar_stack(column = mission_details, position = "stack", width = 100) %>%
  # gt_plt_bar(column = mission_duration, color = "blue") %>%
  gt_plt_bar(column = mission_duration, color = "grey") |> 
  gt_img_rows(columns = splashdown_map, img_source = "local", height = 50) %>%
  gt_theme_dark()




# Creating the table ----

d1 %>% 
  select(mission, launch_date, launch_time) %>% 
  gt() %>% 
  gtExtras::gt_merge_stack(col1 = launch_date, col2 = launch_time,
                           palette = c("white", "grey")) %>%
  gt_theme_dark()
  
d1 %>% 
    select(patch, mission, commander:cm_pilot, launch_date) %>% 
    mutate(crew = paste(commander, lm_pilot, cm_pilot, sep = "<br>")) %>% 
    select(patch, mission, crew, launch_date) %>% 
    gt() %>% 
    tab_header(title = "There and back again") %>% 
    gtExtras::gt_theme_nytimes() %>% 
    gtExtras::gt_merge_stack(col1 = mission, col2 = crew, ) %>% 
    gt_img_rows(columns = patch, img_source = "local", height = 40) %>% 
    fmt_markdown(columns = mission)
  
gtsave(tab, "tab.png")


"Share of <span style = 'color: #b3cde0;'>hydroelectric</span>, <span style = 'color: #35a79c;'>wind</span> and <span style = 'color: #f6be00;'>solar energies</span>"

tab <- d1 %>% 
  select(mission, commander:cm_pilot) %>% 
  mutate(commander = paste0("<span style = 'color : blue'> ", "<b>", commander, "<b>", "</span>"),
         lm_pilot = paste0("<span style = 'color : green'> ", "<b>", lm_pilot, "<b>", "</span>"),
         cm_pilot = paste0("<span style = 'color : red'> ", cm_pilot, "</span>")) %>% 
  mutate(crew = paste0(commander, " <br>", lm_pilot, " <br>", cm_pilot)) %>% 
  select(mission, crew) %>% 
  mutate(plot = rep("img/splashdown.png", 6)) %>% 
  gt() %>% 
  fmt_markdown(columns = crew) %>%
  gt_img_rows(columns = plot, height = 50, img_source = "local") %>% 
  tab_header(title = md("**There and back again**"),
             subtitle = "Apollo missions that landed on the moon")

gtsave(tab, "tab.png")

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

  gt_img_rows(columns = clock, img_source = "web", height = 25) %>% 
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
