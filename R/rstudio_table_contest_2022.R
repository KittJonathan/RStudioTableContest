# RStudio Table Contest
# 2022

# Load packages ----

library(tidyverse)
library(gt)
library(datardis)

# Testing {gt} package ----

datardis::drwho_episodes %>% 
  gt()
