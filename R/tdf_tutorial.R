# Riding tables with {gt} and {gtExtras}
# https://bjnnowak.netlify.app/2021/10/04/r-beautiful-tables-with-gt-and-gtextras/
# Last updated 2022-09-21

# Load packages ----

library(gt)
library(gtExtras)

# Import datasets ----

tdf_winners <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-04-07/tdf_winners.csv')
tdf_stages <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-04-07/tdf_stages.csv')

# First example : stage winners in 1961 ----

tdf_61 <- tdf_stages %>% 
  mutate(year = lubridate::year(Date)) %>% 
  filter(year == 1961) %>% 
  select(-year)

tab <- tdf_61 %>% 
  head(6) %>% 
  gt()

tab <- tab %>% 
  tab_header(title = "Stage winners",
             subtitle = md("Tour de France **1961**")) %>% 
  fmt_number(
    columns = Distance,
    decimals = 0,
    pattern = "{x} km"
  ) %>% 
  cols_label(
    Winner_Country = "Nationality"
  )

tab %>% 
  tab_style(
    locations = cells_title(groups = "title"),
    style = list(
      cell_text(
        font = google_font(name = "Bebas Neue"),
        size = "xx-large",
        color = "indianred"
      )
    )
  )

# Add plot in table ----

most_wins <- tdf_winners %>% 
  filter(winner_name != "Lance Armstrong") %>% 
  mutate(winner_name = case_when(
    winner_name == "Miguel Induràin" ~ "Miguel Indurain",
    TRUE ~ winner_name
  )) %>% 
  mutate(ct = 1) %>% 
  group_by(winner_name) %>% 
  summarise(
    Titles = sum(ct),
    Country = nationality[1],
    Nickname = nickname[1]) %>% 
  filter(Titles > 2) %>% 
  arrange(-Titles) %>% 
  select(Rider = winner_name)

names_most_wins <- most_wins %>% 
  pull(Rider)

year_wins <- tdf_winners %>% 
  mutate(Rider = case_when(
    winner_name == "Miguel Induràin" ~ "Miguel Indurain",
    TRUE ~ winner_name
  )) %>% 
  mutate(ct = 1) %>% 
  complete(Rider, edition, fill = list(ct = 0)) %>% 
  group_by(Rider) %>% 
  summarise(Timeline = list(ct)) %>% 
  filter(Rider %in% names_most_wins)

year_wins
