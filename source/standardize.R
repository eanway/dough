# standardize dough data
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
  rating %>%
    recode(
      "yes" = as.character(max), 
      "no" = as.character(min)
    ) %>%
    as.integer()
}

vec_core <- c(
  "batch_number", 
  "final_dough_temperature", 
  "room_temperature", 
  "humidity", 
  "hydration", 
  "leaven_percentage", 
  "units", 
  "rating"
)

df_dough_thin <- df_dough %>%
  clean_names() %>%
  select(
    all_of(vec_core), 
    day, 
    ends_with(c("hour", "finish", "date"))
  ) %>%
  filter(
    !is.na(day)
  )

# Standardize dates ####
df_dough_dates <- df_dough_thin %>%
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
  )

# Standardize ratings ####
df_dough_rating <- df_dough_dates %>%
  mutate(
    rating = standardize_rating(rating, 0, 10)
  )


# Analyze ####
df_dough_duration <- df_dough_rating %>%
  # not including fermentation duration because it's the sum of bulk, bench, and proof
  mutate(
    auto_duration_mins = difftime(bulk_hour, auto_hour, units = "mins"), 
    bulk_duration_mins = difftime(bench_hour, bulk_hour, units = "mins"), 
    bench_duration_mins = difftime(proof_hour, bench_hour, units = "mins"), 
    proof_duration_mins = difftime(bake_hour, proof_hour, units = "mins"), 
    bake_duration_mins = difftime(bake_finish, bake_hour, units = "mins")
  ) %>%
  mutate(
    flour_age_days = difftime(day, mill_date, units = "days"), 
    bench_time_per_loaf = bench_duration_mins / units
  ) %>%
  select(
    all_of(vec_core), 
    ends_with("duration_mins"), 
    flour_age_days, 
    bench_time_per_loaf
  )


# Set Data Types ####
df_dough_standard <- df_dough_duration %>%
  mutate(
    across(c("batch_number"), as.factor), 
    across(
      c("final_dough_temperature", "room_temperature", 
        "humidity", "hydration", "leaven_percentage", "bench_time_per_loaf"), 
      as.numeric
    ), 
    across(c(where(is.difftime), "units", "rating"), as.integer)
  )

summary(df_dough_standard)


# Export ####
p_load(
  readr
)

write_rds(df_dough_standard, here::here("working", "out", "df_dough_standard.rds"))
write_csv(df_dough_standard, here::here("working", "out", "df_dough_standard.csv"), na = "")
