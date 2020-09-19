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

# Standardize ####
p_load(
  dplyr, 
  janitor, 
  lubridate
)

df_dough_standard <- df_dough %>%
  clean_names() %>%
  select(
    batch_number, 
    day, 
    final_dough_temperature, 
    room_temperature, 
    humidity, 
    hydration, 
    leaven_percentage, 
    auto_hour, 
    bulk_hour, 
    bench_hour, 
    proof_hour, 
    bake_hour, 
    bake_finish,
    units, 
    mill_date, 
    rating
  ) %>%
  filter(
    !is.na(day)
  ) %>%
  mutate(
    across(ends_with("hour|finish"), ymd_hms), 
    mill_date = as.Date(as.integer(mill_date), origin = "1899-12-30")
  )


# Analyze ####
shift_day <- function(early_hour, late_hour) {
  as_datetime(ifelse(late_hour < early_hour & !is.na(early_hour), late_hour + days(1), late_hour))
}

df_dough_analysis <- df_dough_standard %>%
  mutate(
    bulk_hour = shift_day(auto_hour, bulk_hour)
  )
