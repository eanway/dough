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

initialize_date <- function(start_date, time) {
  date_time <- ymd_hms(time)
  start_date + hours(hour(date_time)) + minutes(minute(date_time))
}

shift_day <- function(early_hour, late_hour) {
  as_datetime(ifelse(late_hour < early_hour & !is.na(early_hour), late_hour + days(1), late_hour))
}

standardize_rating <- function(rating, min, max) {
  rating_int <- rating %>%
    recode(
      "yes" = as.character(max), 
      "no" = as.character(min)
    ) %>%
    as.integer()
  
  (rating_int - min) / (10 - min)
}

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
  # standardize dates
  mutate(
    mill_date = as.Date(as.integer(mill_date), origin = "1899-12-30")
  ) %>%
  mutate(
    across(ends_with(c("hour", "finish")), ~initialize_date(day, .))
  ) %>%
  mutate(
    bulk_hour = shift_day(auto_hour, bulk_hour), 
    bench_hour = shift_day(bulk_hour, bench_hour), 
    proof_hour = shift_day(bench_hour, proof_hour), 
    bake_hour = shift_day(proof_hour, bake_hour), 
    bake_finish = shift_day(bake_hour, bake_finish)
  ) %>%
  # standardize ratings
  mutate(
    rating_new = standardize_rating(rating, 0, 10)
  )


# Analyze ####
df_dough_analysis <- df_dough_standard %>%
  # not including fermentation duration because it's the sum of bulk, bench, and proof
  mutate(
    auto_duration = difftime(bulk_hour, auto_hour, units = "mins"), 
    bulk_duration = difftime(bench_hour, bulk_hour, units = "mins"), 
    bench_duration = difftime(proof_hour, bench_hour, units = "mins"), 
    proof_duration = difftime(bake_hour, proof_hour, units = "mins"), 
    bake_duration = difftime(bake_finish, bake_hour, units = "mins")
  )
