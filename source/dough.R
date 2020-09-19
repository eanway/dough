# exploratory data analysis for dough data
# 9/19/2020
# ECA

if(!require(pacman)) install.packages("pacman"); library(pacman)

# Import ####
p_load(
  readxl, 
  here
)

df_dough <- read_excel(here::here("working", "data", "dough.xlsx"))
