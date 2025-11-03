if(!dir.exists(file.path("data", "mp01"))) {
  dir.create(file.path("data", "mp01"),
             showWarnings = FALSE,
             recursive = TRUE)
}

GLOBAL_TOP_10_FILENAME <- file.path("data", "mp01", "global_top10_alltime.csv")

if (!file.exists(GLOBAL_TOP_10_FILENAME)) {
  download.file("https://www.netflix.com/tudum/top10/data/all-weeks-global.tsv",
                destfile = GLOBAL_TOP_10_FILENAME)
}

COUNTRY_TOP_10_FILENAME <- file.path("data", "mp01", "country_top10_alltime.csv")

if (!file.exists(COUNTRY_TOP_10_FILENAME)) {
  download.file("https://www.netflix.com/tudum/top10/data/all-weeks-countries.tsv",
                destfile = COUNTRY_TOP_10_FILENAME)
}

if (!require("tidyverse"))
  install.packages("tidyverse")
install.packages("kableExtra")


GLOBAL_GLOBAL_TOP_10 <- read_tsv(GLOBAL_TOP_10_FILENAME)

COUNTRY_TOP_10 <- read_tsv(COUNTRY_TOP_10_FILENAME, na = c("N/A", "NA"))

str(GLOBAL_TOP_10)

glimpse(GLOBAL_TOP_10)

GLOBAL_TOP_10 <- GLOBAL_TOP_10 |>
  mutate(season_title = if_else(
    is.na(season_title)
    | season_title == 'N/A',
    NA_character_,
    season_title
  ))
library(readr)
library(dplyr)
library(tidyverse)
library(knitr)
library(kableExtra)
library(DT)
library(stringr)
library(scales)

GLOBAL_TOP_10 |>
  head(n = 20) |>
  datatable(options = list(searching = FALSE, info = FALSE))

format_titles <- function(df) {
  colnames(df) <- str_replace_all(colnames(df), "_", " ") |> str_to_title()
  df
}

GLOBAL_TOP_10 |>
  format_titles() |>
  head(n = 20) |>
  datatable(options = list(searching = FALSE, info = FALSE)) |>
  formatRound(c('Weekly Hours Viewed', 'Weekly Views'))

GLOBAL_TOP_10 |>
  select(-season_title) |>
  format_titles() |>
  head(n = 20) |>
  datatable(options = list(searching = FALSE, info = FALSE)) |>
  formatRound(c('Weekly Hours Viewed', 'Weekly Views'))

GLOBAL_TOP_10 |>
  select(-season_title) |>
  format_titles() |>
  head(n = 20) |>
  datatable(options = list(searching = FALSE, info = FALSE)) |>
  formatRound(c('Weekly Hours Viewed', 'Weekly Views'))

GLOBAL_TOP_10 |>
  mutate(`runtime_(minutes)` = round(60 * runtime)) |>
  select(-season_title, -runtime) |>
  format_titles() |>
  head(n = 20) |>
  datatable(options = list(searching = FALSE, info = FALSE)) |>
  formatRound(c('Weekly Hours Viewed', 'Weekly Views'))

# How many different countries Netflix Operates in?
COUNTRY_TOP_10 |>
  summarize("Number of countries Netflix operates in" = n_distinct(country_name))

##Which non-English-language film has spent the most cumulative weeks in the global top 10?
##How many weeks did it spend?

GLOBAL_TOP_10 |>
  filter(category == 'Films (Non-English)') |>
  select(`Film` = show_title, `Weeks in top 10` = cumulative_weeks_in_top_10) |>
  slice_max(`Weeks in top 10`) |>
  kable(caption = "Top Global Non-English Film by Weeks")

#What is the longest film (English or non-English) to have ever appeared in the Netflix global Top 10?
#How long is it in minutes?
GLOBAL_TOP_10 |>
  filter(category == 'Films (Non-English)' |
           category == 'Films (English)') |>
  mutate(`Runtime (minutes)` = round(60 * runtime)) |>
  select(`Film` = show_title, `Runtime (minutes)`) |>
  distinct() |>
  slice_max(`Runtime (minutes)`)|>
  kable(caption = "Longest Film ever in the top 10")

#For each of the four categories, what program has the most total hours of global viewership?
GLOBAL_TOP_10 |>
  select(category, show_title, weekly_hours_viewed) |>
  group_by(category, show_title) |>
  summarise(`Total hours watched` = sum(weekly_hours_viewed),
            .groups = "drop_last")|>
  slice_max(`Total hours watched`) |>
  ungroup()|>
  rename(`Category` = category, `Title` = show_title)|>
  kable(caption = "What programs have been viewed the longest?")

#Which TV show had the longest consecutive run in a country’s Top 10?
# How long was this run and in what country did it occur



COUNTRY_TOP_10 |>
  filter(category == 'TV') |>
  arrange(country_name, show_title, week) |>
  group_by(country_name, show_title) |>
  mutate(

    week_diff = as.numeric(week - lag(week)),
    group_id = cumsum(ifelse(is.na(week_diff) |
                               week_diff != 7, 1, 0))
  ) |>
  group_by(country_name, show_title, group_id) |>
  summarise(successive_weeks = n(), .groups = "drop") |>
  group_by(country_name, show_title) |>
  summarise(longest_successive_run = max(successive_weeks),
            .groups = "drop") |>
  slice_max(longest_successive_run) |>
  rename(
    `Country` = country_name,
    `Title` = show_title,
    `Longest Successive Run (Weeks)` = longest_successive_run
  ) |>
  kable(caption = "What TV show had the longest uninterrupted weekly run in the top 10?")



##Netflix provides over 200 weeks of service history for all but one country in our data set.
##Which country is this and when did Netflix cease operations in that country?

less_200 <- COUNTRY_TOP_10 |>
  group_by(country_name) |>
  summarise(total_weeks = n_distinct(week), .groups = "drop") |>
  filter(total_weeks < 200) |>
  select(country_name, total_weeks)

last_week <- COUNTRY_TOP_10 |>
  group_by(country_name) |>
  summarise(last_week = max(week), .groups = "drop")

less_200 |>
  inner_join(last_week, by = "country_name")|>
  mutate(last_week = format(last_week, "%B %d, %Y")) |>
  rename(
    `Country` = country_name,
    `Total weeks of service` = total_weeks,
    `Last week of Service` = last_week
  )|>
  kable(caption = "Countries Where Netflix Ceased Service")

#What is the total viewership of the TV show Squid Game?
GLOBAL_TOP_10|>
  filter(show_title == 'Squid Game')|>
  group_by(show_title)|>
  summarize(total_views = sum(weekly_views, na.rm = TRUE))|>
  pull(total_views)|>
  kable(caption = "Squid Game's Total Views")
# The movie Red Notice has a runtime of 1 hour and 58 minutes.
## Approximately how many views did it receive in 2021?

GLOBAL_TOP_10 |>
  filter(show_title == 'Red Notice' & year(week) %in% c(2021))|>
  group_by(show_title)|>
  summarize(`Total Views` =
              sum(weekly_hours_viewed, na.rm = TRUE) / 1.97)|>
  pull(`Total Views`)|>
  kable(caption = "Red Notice's Total Views in 2021")

#How many Films reached Number 1 in the US but did not originally debut there?
##That is, find films that first appeared on the Top 10 chart at,
##e.g., Number 4 but then became more popular and eventually hit Number 1?
##What is the most recent film to pull this off?

COUNTRY_FIRST <- COUNTRY_TOP_10 |>
  filter(category == 'Films', country_iso2 == "US", )|>
  group_by(show_title) |>
  mutate(ever_1st = any(weekly_rank == 1)) |>
  ungroup()

COUNTRY_FIRST |>
  filter(ever_1st == TRUE) |>
  group_by(show_title) |>
  arrange(week) |>
  summarize(
    debut_week = first(week),
    debut_rank = first(weekly_rank),
    .groups = "drop"
  ) |>
  filter(debut_rank > 1) |>
  summarize(`Number of films reaching #1 later on` = n_distinct(show_title))|>
  kable()

COUNTRY_FIRST |>
  filter(ever_1st == TRUE) |>
  group_by(show_title) |>
  arrange(week) |>
  mutate(first_week_1st = min(week[weekly_rank == 1])) |>
  summarize(
    debut_week = first(week),
    debut_rank = first(weekly_rank),
    week_reached_1st = first(first_week_1st),
    .groups = "drop"
  ) |>
  filter(debut_rank > 1) |>
  slice_max(debut_week, n = 1)|>
  kable(caption = "Recent Film that did this")

#Which TV show/season hit the top 10 in the most countries in its debut week?
##In how many countries did it chart?
COUNTRY_TOP_10|>
  filter(category == 'TV')|>
  group_by(show_title, season_title)|>
  summarize(
    debut_week = first(week),
    debut_rank = first(weekly_rank),
    num_of_countries = n_distinct(country_iso2[week == first(week)]),
    .groups = "drop"
  )|>
  slice_max(num_of_countries)|>
  rename(
    `Show` = show_title,
    `Season` = season_title,
    `Week Debuted` = debut_week,
    `Initial Rank` = debut_rank,
    `Number of Countries` = num_of_countries
  )|>
  kable(caption = "TV shows hitting top 10 in the most Countries")
