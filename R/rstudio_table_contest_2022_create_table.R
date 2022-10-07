# RStudio Table Contest
# 2022
# Create table
# Last updated 2022-10-07

# Load packages ----

library(gt)
library(gtExtras)
# library(lubridate)
library(tidyverse)

# Import clean data ----

d1 <- readr::read_csv("data/clean_data.csv")

# Create table ----

d1 |> 
  # transform into {gt} table
  gt::gt() |> 
  # set theme to "dark"
  gtExtras::gt_theme_dark() |> 
  # display mission patches
  gtExtras::gt_img_rows(columns = patch,
                        img_source = "local",
                        height = 75) |> 
  # merge mission & mission_duration columns
  gtExtras::gt_merge_stack(
    col1 = mission, 
    col2 = mission_duration,
    palette = c("white", "grey"),
    font_size = c("22px", "16px"),
    font_weight = c("bold", "normal")) |> 
  # read crew column as markdown
  gt::fmt_markdown(crew) |> 
  # add footnote for crew column
  gt::tab_footnote(footnote = "in order : commander, lunar module pilot & command module pilot",
                   locations = cells_column_labels(columns = crew)) |> 
  # merge launch_date & launch_time columns
  gtExtras::gt_merge_stack(col1 = launch_date,
                           col2 = launch_time,
                           palette = c("white", "grey"),
                           font_size = c("18px", "16px"),
                           font_weight = c("bold", "normal")) |> 
  # create tab spanner for patch, mission, crew & launch columns
  gt::tab_spanner(label = "MISSION",
                  columns = c(patch, mission, crew, launch_date), replace = TRUE) |> 
  # remove column labels for patch & mission + rename launch_date to launch
  gt::cols_label(patch = "",
                 mission = "",
                 launch_date = "launch") |> 
  # create tab spanner for hours_on_the_moon, lunar_evas & samples_mass columns
  gt::tab_spanner(label = "ON THE MOON",
                  columns = c(hours_on_the_moon, lunar_evas, samples_mass), replace = TRUE) |> 
  # rename columns
  gt::cols_label(hours_on_the_moon = "Hours",
                 lunar_evas = "EVA",
                 samples_mass = "Samples (kgs)") |> 
  # add footnote for EVAs
  gt::tab_footnote(footnote = "extravehicular activity",
                   locations = cells_column_labels(columns = lunar_evas)) |> 
  # merge splashdown_date & splashdown_time columns
  gtExtras::gt_merge_stack(col1 = splashdown_date,
                           col2 = splashdown_time,
                           palette = c("white", "grey"),
                           font_size = c("18px", "16px"),
                           font_weight = c("bold", "normal")) |> 
  # display splashdown maps
  gtExtras::gt_img_rows(columns = splashdown_map,
                        img_source = "local",
                        height = 75) |> 
  # create tab spanner for splashdown_date, & splashdown_map columns
  gt::tab_spanner(label = "BACK TO EARTH",
                  columns = c(splashdown_date, splashdown_map), replace = TRUE) |> 
  # rename columns
  gt::cols_label(splashdown_date = "Splashdown",
                 splashdown_map = "Site") |> 
  # add colours to numerical columns
  gt::data_color(columns = hours_on_the_moon,
                 colors = scales::col_numeric(
                   palette = c("#333333", "lightgrey"),
                   domain = c(0, 75))) |> 
  gt::data_color(columns = lunar_evas,
                 colors = scales::col_numeric(
                   palette = c("#333333", "lightgrey"),
                   domain = c(0, 3)))|> 
  gt::data_color(columns = samples_mass,
                 colors = scales::col_numeric(
                   palette = c("#333333", "lightgrey"),
                   domain = c(0, 115))) |> 
  # align text in columns
  gt::cols_align(align = "center") |> 
  # change font in table cells
  gt::opt_table_font(font = google_font(name = "Roboto Slab")) |> 
  # set columns widths
  gt::cols_width(mission ~ px(110),
                 crew ~ px(155),
                 launch_date ~ px(180),
                 hours_on_the_moon ~ px(45),
                 lunar_evas ~ px(45),
                 samples_mass ~ px(45),
                 splashdown_date ~ px(160),
                 splashdown_map ~ px(140)) |> 
  # add title and subtitle
  gt::tab_header(title = "There and back again") |> 
  # adjust title & subtitle font size
  gt::tab_options(heading.title.font.size = 30) |> 
  # change title & subtitle font
  gt::tab_style(
    locations = cells_title(groups = c("title")),
    style = list(cell_text(
      font = google_font(name = "Goldman")))) |> 
  # add tab source note
  gt::tab_source_note(source_note = "Source: Wikipedia | Created by: Jonathan Kitt") 

gtsave(tab, "tab.png")

# d1 |> 
#   gt() |> 
#   gt_merge_stack(col1 = mission, col2 = mission_duration,
#                  palette = c("white", "grey"),
#                  font_size = c("22px", "18px"),
#                  font_weight = c("bold", "normal")) |> 
  # fmt_markdown(crew) |> 
  # gt_merge_stack(col1 = launch_date, col2 = launch_time,
  #                palette = c("white", "grey"),
  #                font_size = c("20px", "16px"),
  #                font_weight = c("bold", "normal")) |> 
  # gt_merge_stack(col1 = splashdown_date, col2 = splashdown_time,
  #                palette = c("white", "grey"),
  #                font_size = c("20px", "16px"),
  #                font_weight = c("bold", "normal")) |> 
  # gt_img_rows(columns = splashdown_map, img_source = "local", height = 75) |> 
  # gt_theme_dark() |> 
  # tab_header(title = "There and back again",
  #            subtitle = "Apollo missions that landed on the moon") |> 
  # gt::cols_label(launch_date = "Launch",
  #                hours_on_the_moon = "Hours",
  #                lunar_evas = "EVAs",
  #                samples_mass = "Samples (kgs)",
  #                splashdown_date = "Splashdown",
  #                splashdown_map = "Site") |> 
  # tab_spanner(label = "BACK TO EARTH",
  #             columns = c(splashdown_date, splashdown_time, splashdown_map), replace = TRUE) |> 
  # tab_spanner(label = "ON THE MOON",
  #             columns = c(hours_on_the_moon, lunar_evas, samples_mass), replace = TRUE) |> 
  # data_color(columns = hours_on_the_moon,
  #            colors = scales::col_numeric(
  #              palette = c("#333333", "lightgrey"),
  #              domain = c(0, 75)
  #            ))|> 
  # data_color(columns = lunar_evas,
  #            colors = scales::col_numeric(
  #              palette = c("#333333", "lightgrey"),
  #              domain = c(0, 3)
  #            ))|> 
  # data_color(columns = samples_mass,
  #            colors = scales::col_numeric(
  #              palette = c("#333333", "lightgrey"),
  #              domain = c(0, 115)
  #            )) |> 
  # cols_width(mission ~ px(150),
  #            crew ~ px(150),
  #            launch_time ~ px(175),
  #            hours_on_the_moon ~ px(75),
  #            lunar_evas ~ px(75),
  #            samples_mass ~ px(75),
  #            splashdown_date ~ px(175),
  #            splashdown_map ~ px(175)) |> 
  # tab_footnote(footnote = "in order : commander, lunar module pilot & command module pilot",
  #              locations = cells_column_labels(columns = crew)) |> 
  # tab_footnote(footnote = "extravehicular activity",
  #              locations = cells_column_labels(columns = lunar_evas)) |> 
  # cols_align(align = "center") |> 
  # opt_table_font(font = google_font(name = "Roboto Slab")) |> 
  tab_options(heading.title.font.size = 35,
              heading.subtitle.font.size = 25) |> 
  tab_source_note(source_note = "Source: Wikipedia | Created by: Jonathan Kitt") |> 
  tab_style(
    locations = cells_title(groups = c("title", "subtitle")),
    style = list(
      cell_text(
        font = google_font(name = "Goldman")
      )
    )
  )
  
  

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
