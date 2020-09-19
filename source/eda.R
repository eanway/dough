# exploratory data analysis for dough data
# 9/19/2020
# ECA

if(!require(pacman)) install.packages("pacman"); library(pacman)

# Import ####
p_load(
  readr, 
  here
)

df_dough <- read_rds(here::here("working", "out", "df_dough_standardize.Rds"))
