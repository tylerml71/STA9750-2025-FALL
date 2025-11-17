if(!dir.exists(file.path("data", "mp02"))) {
  dir.create(file.path("data", "mp02"),
             showWarnings = FALSE,
             recursive = TRUE)
}

ensure_package <- function(pkg) {
  pkg <- as.character(substitute(pkg))
  options(repos = c(CRAN = "https://cloud.r-project.org"))
  if (!require(pkg, character.only = TRUE, quietly = TRUE))
    install.packages(pkg)
  stopifnot(require(pkg, character.only = TRUE, quietly = TRUE))
}

ensure_package(tidyverse)
ensure_package(glue)
ensure_package(readxl)
ensure_package(tidycensus)
ensure_package(scales)
ensure_package(data.table)
ensure_package(kableExtra)
ensure_package(ggthemes)
ensure_package(cowplot)
ensure_package(ggiraph)
ensure_package(shiny)
ensure_package(shinylive)
ensure_package(gghighlight)
ensure_package(RcppRoll)
ensure_package(plotly)
library(httr2)
library(rvest)

format_titles <- function(df) {
  colnames(df) <- str_replace_all(colnames(df), "_", " ") |> str_to_title()
  df
}

get_acs_all_years <- function(variable,
                              geography = "cbsa",
                              start_year = 2009,
                              end_year = 2023) {
  fname <- glue("{variable}_{geography}_{start_year}_{end_year}.csv")
  fname <- file.path("data", "mp02", fname)
  
  if (!file.exists(fname)) {
    YEARS <- seq(start_year, end_year)
    YEARS <- YEARS[YEARS != 2020] # Drop 2020 - No survey (covid)
    
    ALL_DATA <- map(YEARS, function(yy) {
      tidycensus::get_acs(geography, variable, year = yy, survey = "acs1") |>
        mutate(year = yy) |>
        select(-moe, -variable) |>
        rename(!!variable := estimate)
    }) |> bind_rows()
    
    write_csv(ALL_DATA, fname)
  }
  
  read_csv(fname, show_col_types = FALSE)
}

# Household income (12 month)
INCOME <- get_acs_all_years("B19013_001") |>
  rename(household_income = B19013_001)
INCOME
# Monthly rent
RENT <- get_acs_all_years("B25064_001") |>
  rename(monthly_rent = B25064_001)

# Total population
POPULATION <- get_acs_all_years("B01003_001") |>
  rename(population = B01003_001)

# Total number of households
HOUSEHOLDS <- get_acs_all_years("B11001_001") |>
  rename(households = B11001_001)


# To get the number of new housing units built each year.
get_building_permits <- function(start_year = 2009,
                                 end_year = 2023) {
  fname <- glue("housing_units_{start_year}_{end_year}.csv")
  fname <- file.path("data", "mp02", fname)
  
  if (!file.exists(fname)) {
    HISTORICAL_YEARS <- seq(start_year, 2018)
    
    HISTORICAL_DATA <- map(HISTORICAL_YEARS, function(yy) {
      historical_url <- glue("https://www.census.gov/construction/bps/txt/tb3u{yy}.txt")
      
      LINES <- readLines(historical_url)[-c(1:11)]
      
      CBSA_LINES <- str_detect(LINES, "^[[:digit:]]")
      CBSA <- as.integer(str_sub(LINES[CBSA_LINES], 5, 10))
      
      PERMIT_LINES <- str_detect(str_sub(LINES, 48, 53), "[[:digit:]]")
      PERMITS <- as.integer(str_sub(LINES[PERMIT_LINES], 48, 53))
      
      data_frame(
        CBSA = CBSA,
        new_housing_units_permitted = PERMITS,
        year = yy
      )
    }) |> bind_rows()
    
    CURRENT_YEARS <- seq(2019, end_year)
    
    CURRENT_DATA <- map(CURRENT_YEARS, function(yy) {
      current_url <- glue("https://www.census.gov/construction/bps/xls/msaannual_{yy}99.xls")
      
      temp <- tempfile()
      
      download.file(current_url, destfile = temp, mode = "wb")
      
      fallback <- function(.f1, .f2) {
        function(...) {
          tryCatch(
            .f1(...),
            error = function(e)
              .f2(...)
          )
        }
      }
      
      reader <- fallback(read_xlsx, read_xls)
      
      reader(temp, skip = 5) |>
        na.omit() |>
        select(CBSA, Total) |>
        mutate(year = yy) |>
        rename(new_housing_units_permitted = Total)
    }) |> bind_rows()
    
    ALL_DATA <- rbind(HISTORICAL_DATA, CURRENT_DATA)
    
    write_csv(ALL_DATA, fname)
    
  }
  
  read_csv(fname, show_col_types = FALSE)
}

PERMITS <- get_building_permits()

# Core-Based Statistical Areas or CBSAs, to get BLS records from NAICS coding system

get_bls_industry_codes <- function() {
  fname <- file.path("data", "mp02", "bls_industry_codes.csv")
  library(dplyr)
  library(tidyr)
  library(readr)
  
  if (!file.exists(fname)) {
    resp <- request("https://www.bls.gov") |>
      req_url_path("cew",
                   "classifications",
                   "industry",
                   "industry-titles.htm") |>
      req_headers(`User-Agent` = "Mozilla/5.0 (Macintosh; Intel Mac OS X 10.15; rv:143.0) Gecko/20100101 Firefox/143.0") |>
      req_error(is_error = \(resp) FALSE) |>
      req_perform()
    
    resp_check_status(resp)
    
    naics_table <- resp_body_html(resp) |>
      html_element("#naics_titles") |>
      html_table() |>
      mutate(title = str_trim(str_remove(
        str_remove(`Industry Title`, Code), "NAICS"
      ))) |>
      select(-`Industry Title`) |>
      mutate(depth = if_else(nchar(Code) <= 5, nchar(Code) - 1, NA)) |>
      filter(!is.na(depth))
    
    # These were looked up manually on bls.gov after finding
    # they were presented as ranges. Since there are only three
    # it was easier to manually handle than to special-case everything else
    naics_missing <- tibble::tribble(
      ~ Code,
      ~ title,
      ~ depth,
      "31",
      "Manufacturing",
      1,
      "32",
      "Manufacturing",
      1,
      "33",
      "Manufacturing",
      1,
      "44",
      "Retail",
      1,
      "45",
      "Retail",
      1,
      "48",
      "Transportation and Warehousing",
      1,
      "49",
      "Transportation and Warehousing",
      1
    )
    
    naics_table <- bind_rows(naics_table, naics_missing)
    
    naics_table <- naics_table |>
      filter(depth == 4) |>
      rename(level4_title = title) |>
      mutate(
        level1_code = str_sub(Code, end = 2),
        level2_code = str_sub(Code, end = 3),
        level3_code = str_sub(Code, end = 4)
      ) |>
      left_join(naics_table, join_by(level1_code == Code)) |>
      rename(level1_title = title) |>
      left_join(naics_table, join_by(level2_code == Code)) |>
      rename(level2_title = title) |>
      left_join(naics_table, join_by(level3_code == Code)) |>
      rename(level3_title = title) |>
      select(-starts_with("depth")) |>
      rename(level4_code = Code) |>
      select(
        level1_title,
        level2_title,
        level3_title,
        level4_title,
        level1_code,
        level2_code,
        level3_code,
        level4_code
      ) |>
      drop_na() |>
      mutate(across(contains("code"), as.integer))
    
    write_csv(naics_table, fname)
  }
  
  read_csv(fname, show_col_types = FALSE)
}

INDUSTRY_CODES <- get_bls_industry_codes()

# BLS Quarterly census of Employment and Wages

get_bls_qcew_annual_averages <- function(start_year = 2009,
                                         end_year = 2023) {
  fname <- glue("bls_qcew_{start_year}_{end_year}.csv.gz")
  fname <- file.path("data", "mp02", fname)
  
  YEARS <- seq(start_year, end_year)
  YEARS <- YEARS[YEARS != 2020] # Drop Covid year to match ACS
  
  if (!file.exists(fname)) {
    ALL_DATA <- map(YEARS, .progress = TRUE, possibly(function(yy) {
      fname_inner <- file.path("data",
                               "mp02",
                               glue("{yy}_qcew_annual_singlefile.zip"))
      
      if (!file.exists(fname_inner)) {
        request("https://www.bls.gov") |>
          req_url_path("cew",
                       "data",
                       "files",
                       yy,
                       "csv",
                       glue("{yy}_annual_singlefile.zip")) |>
          req_headers(`User-Agent` = "Mozilla/5.0 (Macintosh; Intel Mac OS X 10.15; rv:143.0) Gecko/20100101 Firefox/143.0") |>
          req_retry(max_tries = 5) |>
          req_perform(fname_inner)
      }
      
      if (file.info(fname_inner)$size < 755e5) {
        warning(sQuote(fname_inner),
                "appears corrupted. Please delete and retry this step.")
      }
      
      read_csv(fname_inner, show_col_types = FALSE) |>
        mutate(YEAR = yy) |>
        select(area_fips,
               industry_code,
               annual_avg_emplvl,
               total_annual_wages,
               YEAR) |>
        filter(nchar(industry_code) <= 5, str_starts(area_fips, "C")) |>
        filter(str_detect(industry_code, "-", negate = TRUE)) |>
        mutate(
          FIPS = area_fips,
          INDUSTRY = as.integer(industry_code),
          EMPLOYMENT = as.integer(annual_avg_emplvl),
          TOTAL_WAGES = total_annual_wages
        ) |>
        select(-area_fips,-industry_code,-annual_avg_emplvl,-total_annual_wages) |>
        # 10 is a special value: "all industries" , so omit
        filter(INDUSTRY != 10) |>
        mutate(AVG_WAGE = TOTAL_WAGES / EMPLOYMENT)
    })) |> bind_rows()
    
    write_csv(ALL_DATA, fname)
  }
  
  ALL_DATA <- read_csv(fname, show_col_types = FALSE)
  
  ALL_DATA_YEARS <- unique(ALL_DATA$YEAR)
  
  YEARS_DIFF <- setdiff(YEARS, ALL_DATA_YEARS)
  
  if (length(YEARS_DIFF) > 0) {
    stop(
      "Download failed for the following years: ",
      YEARS_DIFF,
      ". Please delete intermediate files and try again."
    )
  }
  
  ALL_DATA
}

WAGES <- get_bls_qcew_annual_averages()

WAGES_NEW <- WAGES |>
  mutate(GEOID = as.numeric(paste0(str_remove(FIPS, "^C"), "0")))


sum(is.na(WAGES_NEW$GEOID))


#Reading Data
glimpse(HOUSEHOLDS)
glimpse(INCOME)
glimpse(INDUSTRY_CODES)
glimpse(PERMITS)
glimpse(POPULATION)
glimpse(RENT)
glimpse(WAGES_NEW)



# Which CBSA (by name) permitted the largest number of new housing units in
# the decade from 2010 to 2019 (inclusive)?
PERMITS |>
  filter(year >= 2010 & year <= 2019) |>
  group_by(CBSA) |>
  summarize(
    total_permitted_housing_units = sum(new_housing_units_permitted, na.rm = TRUE),
    .groups = "drop"
  ) |>
  # Join HOUSEHOLDS to add location names
  left_join(
    HOUSEHOLDS |>
      # Count occurrences of each name per GEOID
      count(GEOID, NAME) |>
      group_by(GEOID) |>
      # Pick the most common name to resolve duplicates
      slice_max(n, with_ties = FALSE) |>
      select(GEOID, NAME),
    by = c("CBSA" = "GEOID")
  ) |>
  # Find the CBSA with the highest total permitted housing units
  slice_max(total_permitted_housing_units)


# 2.In what year did Albuquerque, NM (CBSA Number 10740) permit the most new housing units?
# Hint: There is a Covid-19 data artifact here that may trip you up if you do not look at your answer closely.

PERMITS |>
  filter(CBSA == 10740) |>
  slice_max(new_housing_units_permitted)

PERMITS |>
  filter(CBSA == 10740, year != 2021) |>   # exclude 2020 initial Covid year
  slice_max(new_housing_units_permitted)

#3 Which state (not CBSA) had the highest average individual income in 2015?

HOUSEHOLDS |>
  inner_join(INCOME, by = c(
    "GEOID" = "GEOID",
    "year" = "year",
    "NAME" = "NAME"
  )) |>
  inner_join(POPULATION,
             by = c(
               "GEOID" = "GEOID",
               "year" = "year",
               "NAME" = "NAME"
             )) |>
  mutate(
    state = str_extract(NAME, ", (.{2})", group = 1),
    total_income_each = household_income * households
  )|>
  filter(year == 2015)|>
  group_by(state) |>
  summarize(total_income = sum(total_income_each), sum(population))|>
  slice_max(total_income)

# Data scientists and business analysts are recorded under NAICS code 5182.
# What is the last year in which the NYC CBSA had the most data scientists in the country?

DATA_JOBS<- WAGES_NEW|>
  select(-AVG_WAGE, -FIPS, -TOTAL_WAGES)|>
  filter(INDUSTRY == 5182)


HOUSEHOLDS |>
  inner_join(DATA_JOBS, by = c("GEOID" = "GEOID", "year" = "YEAR")) |>
  group_by(year, NAME) |>
  summarize(total_employed = sum(EMPLOYMENT, na.rm = TRUE),
            .groups = "drop_last") |>
  mutate(most_employed = total_employed == max(total_employed)) |>
  filter(most_employed, )|>
  select(-most_employed)

#What fraction of total wages in the NYC CBSA was earned by people employed in
#the finance and insurance industries (NAICS code 52)? In what year did this fraction peak?

PCT_WAGES<- HOUSEHOLDS|>
  filter(str_starts(NAME, "New York"))|>
  select(-households)|>
  inner_join(
    WAGES_NEW |>
      select(GEOID, YEAR, INDUSTRY, TOTAL_WAGES),
    by = c("GEOID" = "GEOID", "year" = "YEAR")
  )|>
  mutate(PCT_NUM = TOTAL_WAGES / sum(TOTAL_WAGES))

PCT_TOT<-PCT_WAGES |>
  filter(INDUSTRY == 52) |>
  group_by(GEOID) |>
  summarize(PCT_NUM = sum(PCT_NUM), .groups = "drop") |>
  slice_max(PCT_NUM) |>
  mutate(PERCENT_OF_TOTAL_WAGES = percent(PCT_NUM, accuracy = 0.01),
         year = "TOTAL") |>
  select(year, PERCENT_OF_TOTAL_WAGES)

PCT_WAGES |>
  filter(INDUSTRY == 52)|>
  group_by(year)|>
  summarize(PCT_NUM = sum(PCT_NUM))|>
  mutate(PERCENT_OF_TOTAL_WAGES = percent(PCT_NUM, accuracy = .01))|>
  select(year, PERCENT_OF_TOTAL_WAGES)|>
  rbind(PCT_TOT)

#The relationship between monthly rent and average household income per CBSA in 2009.

RENT|>
  filter(year == 2009)|>
  full_join(INCOME |>
              filter(year == 2009))|>
  ggplot(aes(y = monthly_rent, x = household_income))+
  geom_point(
    pch = 21,
    fill = "orchid",
    color = "black",
    alpha = 0.7
  )+
  stat_smooth(method = "lm",
              se = FALSE,
              color = "black")+
  xlab("Average Income per Household")+
  scale_x_continuous(label = dollar_format())+
  ylab("Average rent per month")+
  scale_y_continuous(label = dollar_format())+
  ggtitle("Relationship between Household Income \nand Monthly Rent (2009)")+
  theme_cowplot()

#The relationship between total employment and total employment in the health care
# and social services sector (NAICS 62) across different CBSAs.
#Design your visualization so that it is possible to see the evolution of this
#relationship over time.


EMP_PLOTS <- lapply(years, function(y) plot_year(EMP_COMP, y))
names(EMP_PLOTS) <- years

EMP_COMP <- WAGES_NEW |>
  group_by(GEOID, YEAR) |>
  summarize(
    total_emp = sum(EMPLOYMENT, na.rm = TRUE),
    health_emp = sum(EMPLOYMENT[INDUSTRY == 52], na.rm = TRUE),
    .groups = "drop_last"
  ) |>
  mutate(YEAR = as.character(YEAR)) |>
  bind_rows(
    WAGES_NEW |>
      group_by(GEOID) |>
      summarize(
        total_emp = sum(EMPLOYMENT, na.rm = TRUE),
        health_emp = sum(EMPLOYMENT[INDUSTRY == 52], na.rm = TRUE),
        .groups = "keep"
      ) |>
      mutate(YEAR = "Total")
  )

years <- sort(unique(EMP_COMP$YEAR))

plot_year <- function(df, year_label) {
  df |>
    filter(YEAR == year_label) |>
    ggplot(aes(x = total_emp, y = health_emp)) +
    geom_point(pch = 21, fill = "orchid", color = "black", alpha = 0.7) +
    geom_smooth(method = "lm", se = FALSE, color = "black") +
    scale_x_continuous(labels = unit_format(unit = "M", scale = 1e-6)) +
    scale_y_continuous(labels = unit_format(unit = "M", scale = 1e-6)) +
    xlab("Total Employment") +
    ylab("Healthcare and Social Services Employment") +
    ggtitle(paste0("Relationship between Total and Healthcare Employment — ", year_label)) +
    theme_cowplot()
}

EMP_PLOTS <- lapply(years, function(y) plot_year(EMP_COMP, y))
names(EMP_PLOTS) <- years



# The evolution of average household size over time. Use different
# lines to represent different CBSAs.
HOUSE_SIZE<- HOUSEHOLDS |>
  inner_join(POPULATION |>
               select(-NAME),
             by = c("GEOID" = "GEOID", "year" = "year"))|>
  group_by(GEOID, year)|>
  mutate(Avg_household_size = population / households)|>
  ungroup() |>
  mutate(
    metro_group = case_when(
      str_detect(NAME, "NY-NJ-PA Metro Area") ~ "New York City",
      str_starts(NAME, "Los Angeles") ~ "Los Angeles",
      TRUE ~ "Other CBSAs"
    )
  )
HOUSE_SIZE

HOUSE_SIZE |>
  ggplot(aes(
    x = year,
    y = Avg_household_size,
    group = GEOID,
    color = metro_group
  )) +
  geom_line(aes(alpha = metro_group, linewidth = metro_group)) +
  scale_x_continuous(breaks = seq(2009, 2023, 3)) +
  scale_color_manual(values = c(
    "New York City" = "orchid",
    "Los Angeles" = "#1b9e77",
    "Other CBSAs" = "grey70"
  )) +
  scale_alpha_manual(
    values = c(
      "New York City" = 1,
      "Los Angeles" = 1,
      "Other CBSAs" = 0.5
    ),
    guide = "none"
  ) +
  scale_linewidth_manual(
    values = c(
      "New York City" = .8,
      "Los Angeles" = .8,
      "Other CBSAs" = 0.4
    ),
    guide = "none"
  ) +
  labs(
    color = "Area",
    x = "Year",
    y = "Average Household Size",
    title = "Average Household Size Over Time by Area",
    subtitle = "From 2029 to 2023"
  ) +
  theme_minimal()+
  theme(legend.position = "bottom")

# Join together the INCOME and RENT tables. Using this data,
# construct a suitable measure of rent burden

RENT_BURDEN<- INCOME |>
  inner_join(RENT |>
               select(-NAME), by = c("GEOID" = "GEOID", "year" = "year"))|>
  mutate(
    burden_ratio = monthly_rent / household_income,
    med_ratio = burden_ratio / median(burden_ratio) * 100,
  )|>
  select(-household_income, -monthly_rent)

RENT_BURDEN|>
  filter(is.na(med_ratio))

#brings up Guayama, PR
RENT_BURDEN |>
  group_by(GEOID) |>
  filter(max(med_ratio) - min(med_ratio) > 70) |>
  ungroup() |>
  select(year, med_ratio) |>
  mutate(med_ratio = round(med_ratio, 1)) |>
  pivot_wider(names_from = year, values_from = med_ratio) |>
  kable(
    escape = FALSE,
    caption = "<span style='color:#222222;'>Guama, Puerto Rico's Rent Burden Index (Median Scaled)</span> <br>
             <span style='font-size:80%;'>Shows where rent burden changed significantly</span>
             "
  ) |>
  kable_styling(
    full_width = FALSE,
    bootstrap_options = c("striped", "condensed", "responsive")
  )

#Metro Areas highest and lowest Rent burden
RENT_BURDEN |>
  filter(med_ratio == max(med_ratio) |
           med_ratio == min(med_ratio)) |>
  mutate(NAME = str_remove(NAME, "Metro Area"),
         med_ratio = round(med_ratio, 1)) |>
  select(`Area` = NAME, year, `Rent Burden Index` = med_ratio) |>
  format_titles() |>
  kable(caption = "Metro Areas with the highest and lowest Rent Burden") |>
  kable_styling(
    full_width = FALSE,
    bootstrap_options = c("striped", "condensed", "responsive", "bordered")
  ) |>
  column_spec(3, width = "9em", bold = TRUE)

# Task 5, construct a suitable measure of housing growth: that is,
# how many new housing units are permitted in a CBSA, relative to both
# the current number of residents and the overall population growth of that CBSA

HOUSING_GROWTH <- POPULATION |>
  inner_join(PERMITS, by = c("GEOID" = "CBSA", "year" = "year")) |>
  group_by(GEOID) |>
  arrange(GEOID, year) |>
  filter(n() >= 5)|> #at least 5 years of history
  mutate(
    pop_lag = lag(population, n = 5),
    pop_growth_rate = (population - pop_lag) / pop_lag,
    pop_pct = pop_growth_rate * 100,
    #instant measure
    housing_per_1000 = new_housing_units_permitted / population * 1000,
    #per person housing permit rate vs pop. growth rate
    housing_growth = (new_housing_units_permitted / population) / pop_growth_rate *
      100,
    # 5 year rolling average of housing_growth
    housing_growth_roll = roll_mean(
      housing_growth,
      n = 3,
      align = "right",
      fill = NA
    )
  )|>
  ungroup()

HOUSING_GROWTH |>
  filter(
    housing_per_1000 == min(housing_per_1000) |
      housing_per_1000 == max(housing_per_1000)
  )|>
  mutate(housing_per_1000 = round(housing_per_1000, digits = 3))|>
  select(NAME, year, new_housing_per_1000_people = housing_per_1000)|>
  format_titles() |>
  kable(caption = "<span style='color:#222222;'>Metro Areas with the Highest and Lowest Housing Permitted</span> <br>
             <span style='font-size:80%;'>Per 1000 people</span>") |>
  kable_styling(
    full_width = FALSE,
    bootstrap_options = c("striped", "condensed", "responsive", "bordered")
  ) |>
  column_spec(3, color = "red")

HOUSING_GROWTH |>
  filter(
    housing_growth_roll == min(housing_growth_roll, na.rm = TRUE) |
      housing_growth_roll == max(housing_growth_roll, na.rm = TRUE)
  ) |>
  mutate(housing_growth_roll = round(housing_growth_roll, digits = 2)) |>
  select(NAME, year, yearly_growth_rate = housing_growth_roll) |>
  format_titles() |>
  kable(
    escape = FALSE,
    caption = "<span style='color:#222222;'>Highest and lowest Housing
            Growth (100 point Baseline)</span> <br>
            <span style='font-size:80%;'>Per-person housing permit rate vs
            population growth rate</span>"
  ) |>
  kable_styling(
    full_width = FALSE,
    bootstrap_options = c("striped", "condensed", "responsive", "bordered")
  ) |>
  column_spec(3, color = "red")



#Adding composite Score


HOUSING_GROWTH <- HOUSING_GROWTH |>
  mutate(
    per_1000_scaled = 100 * (housing_per_1000 - min(housing_per_1000, na.rm =
                                                      TRUE)) /
      (
        max(housing_per_1000, na.rm = TRUE) - min(housing_per_1000, na.rm = TRUE)
      ),
    roll_scaled = if (all(is.na(housing_growth_roll))) {
      NA_real_   # keep NA if nothing to scale
    } else {
      100 * (housing_growth_roll - min(housing_growth_roll, na.rm = TRUE)) /
        (max(housing_growth_roll, na.rm = TRUE) - min(housing_growth_roll, na.rm =
                                                        TRUE))
    },
    composite_score = rowMeans(cbind(.35 * per_1000_scaled, .65 * roll_scaled), na.rm = TRUE)
  )


HOUSING_GROWTH |>
  filter(composite_score == min(composite_score) |
           composite_score == max(composite_score))|>
  mutate(composite_score = round(composite_score, digits = 3))|>
  select(NAME, year, composite_score)|>
  format_titles() |>
  kable(caption = "<span style='color:#222222;'>Composite Scale</span> <br>
             <span style='font-size:80%;'>Weighted combination of both metrics</span>") |>
  kable_styling(
    full_width = FALSE,
    bootstrap_options = c("striped", "condensed", "responsive", "bordered")
  ) |>
  column_spec(3, color = "red")


# two visualizations to investigate the relationships between
# your Rent Burden and Housing Growth metrics.
# Using these plots, identify the most “YIMBY” CBSAs as ones which:

#relatively high YMBY CBSAs

library(purrr)
library(broom)


RENT_BURDEN_TREND <- RENT_BURDEN |>
  filter(!is.na(med_ratio)) |>
  group_by(GEOID) |>
  #filtering for any year 2014 or before that had a
  #greater than 30% rent burden than average
  filter(any(year <= 2014 & med_ratio > 130))|>
  nest() |>
  mutate(
    #making a model and slope for rent burden ratio per GEOID
    rent_model = map(data, ~ lm(med_ratio ~ year, data = .x)),
    rent_slope = map_dbl(rent_model, ~ coef(.x)[["year"]])
  ) |>
  unnest(data) |>
  ungroup()

HOUSING_GROWTH_TREND <- HOUSING_GROWTH |>
  left_join(HOUSING_SLOPES, by = "GEOID")

RENT_BURDEN_TREND


HOUSING_GROWTH_TREND <- HOUSING_GROWTH |>
  filter(!is.na(pop_pct), !is.na(housing_growth)) |>
  group_by(GEOID) |>
  filter(n() >= 3) |>
  nest() |>
  mutate(
    pop_model = map(data, ~ lm(pop_pct ~ year, data = .x)),
    pop_slope = map_dbl(pop_model, ~ coef(.x)[["year"]]),
    pop_slope = pop_slope * 100,
    housing_model = map(data, ~ lm(housing_growth ~ year, data = .x)),
    housing_slope = map_dbl(housing_model, ~ coef(.x)[["year"]]),
  ) |>
  select(GEOID, pop_slope, housing_slope)|>
  ungroup()



YIMBY_TRENDS<- RENT_BURDEN_TREND|>
  #filtering for areas with rent burden ratio decreasing over time
  filter(rent_slope < 0)|>
  inner_join(
    HOUSING_GROWTH_TREND |>
      #filtering for population growth increasing over time, and
      #hosing slope being higher than average.
      filter(pop_slope > 0 &
               housing_slope > mean(housing_slope)) |>
      select(-NAME, -pop_lag),
    by = c("GEOID" = "GEOID", "year" = "year")
  )
glimpse(YIMBY_TRENDS)

# “The plot shows all CBSAs that had above-median rent burden in the
# early study period (≤2014).
# Lines are color-coded to highlight CBSAs with decreasing rent burden over the study period.”


TOP_HIGH_BURDEN <- RENT_BURDEN |>
  filter(year <= 2014) |>
  arrange(desc(med_ratio)) |>
  slice_head(n = 20)|>
  pull(GEOID)

RENT_BURDEN_FLAGGED <- RENT_BURDEN_TREND |>
  mutate(
    rent_trend = case_when(
      is.na(rent_slope) ~ NA_character_,
      rent_slope < -1   ~ "Strongly Decreasing",
      rent_slope < 0    ~ "Decreasing",
      TRUE              ~ "Increasing or Flat"
    ),
    rent_trend = factor(
      rent_trend,
      levels = c("Strongly Decreasing", "Decreasing", "Increasing or Flat")
    )
  )|>
  mutate(
    # Only top burdened CBSAs get full alpha; others faint
    line_alpha = ifelse(GEOID %in% TOP_HIGH_BURDEN, 1, 0.15),
    line_size  = ifelse(GEOID %in% TOP_HIGH_BURDEN, 2, 0.8),
    # Only show hover for non-NA trends
    hover_text = ifelse(
      !is.na(rent_trend),
      paste0(NAME, " ", year, "<br>Rent-to-Income: ", round(med_ratio, 1), "%"),
      NA
    )
  )

FAINT<- RENT_BURDEN_FLAGGED |>
  filter(!GEOID %in% TOP_HIGH_BURDEN)

HIGHLIGHTED <- RENT_BURDEN_FLAGGED |>
  filter(GEOID %in% TOP_HIGH_BURDEN)


caption_text <- "Only CBSAs with above-median rent burden in the early study period (≤2014) are colored; the rest are faint and thin."

RENT_BURDEN_PLOT <- FAINT |>
  ggplot() +
  geom_line(
    aes(x = year, y = med_ratio, group = GEOID),
    color = "lightgray",
    size = 0.5,
    alpha = 0.15
  ) +
  geom_line(
    data = HIGHLIGHTED,
    aes(
      x = year,
      y = med_ratio,
      group = GEOID,
      color = rent_trend,
      text = paste0(NAME, " ", year, "<br>Rent-to-Income: ", round(med_ratio, 1), "%")
    ),
    size = 0.8,
    alpha = 1
  ) +
  scale_color_manual(
    values = c(
      "Strongly Decreasing" = "#4A148C",
      "Decreasing" = "#7E57C2",
      "Increasing or Flat" = "#B39DDB"
    )
  ) +
  scale_x_continuous(breaks = seq(2009, 2023, 3), expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  labs(
    title = "Rent Burden Over Time by CBSA",
    subtitle = "Dark purple lines indicate CBSAs with strongly decreasing rent burden over time, medium purple for moderately decreasing, and light purple for stable or increasing.",
    x = "Year",
    y = "Median Rent-to-Income Ratio (%)",
    color = "Rent Burden Trend"
  ) +
  theme_cowplot() +
  theme(
    legend.position = "bottom",
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 10),
    plot.subtitle = element_text(size = 10, margin = margin(t = 10, b = 10))
  )

# Convert to Plotly
ggplotly(RENT_BURDEN_PLOT, tooltip = "text") %>%
  layout(
    legend = list(
      orientation = "h",
      x = 0.5,
      xanchor = "center",
      y = -0.28  # Moved the legend further down
    ),
    margin = list(b = 120),
    # Increased bottom margin to fit the caption
    annotations = list(
      list(
        x = 0.5,
        xref = "paper",
        y = -0.47,
        yref = "paper",
        # Moved the caption further down
        text = caption_text,
        showarrow = FALSE,
        xanchor = "center",
        yanchor = "bottom",
        font = list(size = 9, color = "gray")
      )
    )
  )


# Part 2

# Step 0: Flag CBSAs by growth trends

HOUSING_GROWTH_FLAGGED <- HOUSING_GROWTH |>
  left_join(HOUSING_GROWTH_TREND |> select(GEOID, housing_slope, pop_slope),
            by = "GEOID") |>
  mutate(
    housing_trend = case_when(
      is.na(housing_slope)                     ~ NA_character_,
      housing_slope >= 8 & pop_slope > 20      ~ "Strong Growth",
      housing_slope >= 1 &
        housing_slope < 8 & pop_slope > 0  ~ "Moderate Growth",
      TRUE                                     ~ "Low Growth"
    ),
    housing_trend = factor(
      housing_trend,
      levels = c("Low Growth", "Moderate Growth", "Strong Growth")
    ),
    
    # Line aesthetics
    line_alpha = case_when(
      housing_trend == "Strong Growth"   ~ 1,
      housing_trend == "Moderate Growth" ~ 0.7,
      TRUE                               ~ 0.5
    ),
    line_size = case_when(
      housing_trend == "Strong Growth"   ~ 1,
      housing_trend == "Moderate Growth" ~ 0.6,
      TRUE                               ~ 0.5
    ),
    
    # Hover text
    hover_text = ifelse(
      housing_trend %in% c("Strong Growth", "Moderate Growth"),
      paste0(
        "In ",
        year,
        ", ",
        NAME,
        " has an",
        "<br>Annual average housing increase by ",
        round(housing_slope, 2),
        "%",
        "<br>",
        round(housing_per_1000, 1),
        " new houses per 1000 people",
        "<br>Annual average population increase by ",
        round(pop_slope, 2),
        "%"
      ),
      NA
    )
  )

# Step 1: Select top 10 GEOIDs per trend for highlighted lines
TOP_GEOIDS <- HOUSING_GROWTH_FLAGGED |>
  filter(housing_trend %in% c("Moderate Growth", "Strong Growth")) |>
  group_by(housing_trend, GEOID) |>
  summarize(max_housing_slope = max(housing_slope, na.rm = TRUE),
            .groups = "drop") |>
  group_by(housing_trend) |>
  slice_max(max_housing_slope, n = 10) |>
  pull(GEOID)

HIGHLIGHTED_HOUSING <- HOUSING_GROWTH_FLAGGED |>
  filter(GEOID %in% TOP_GEOIDS)

FAINT_HOUSING <- HOUSING_GROWTH_FLAGGED |>
  filter(!GEOID %in% TOP_GEOIDS)

# Caption text
caption_text <- "
Strong Growth: Housing increased by ≥ 8% per year, population growth ≥ 20%
Moderate Growth: Housing increased 1–8% per year, population growth > 0%
Low Growth: Housing growth < 1% per year
"

# Step 2: Plot
HOUSING_GROWTH_PLOT <- FAINT_HOUSING |>
  filter(housing_per_1000 < 13) |>
  ggplot() +
  # Faint gray lines
  geom_line(
    aes(x = year, y = housing_per_1000, group = GEOID),
    color = "lightgray",
    size = 0.4,
    alpha = 0.3
  ) +
  # Highlighted lines
  geom_line(
    data = HIGHLIGHTED_HOUSING,
    aes(
      x = year,
      y = housing_per_1000,
      group = GEOID,
      color = housing_trend,
      text = hover_text
    ),
    size = 0.9,
    alpha = 0.7
  ) +
  # Make all points hoverable for highlighted lines
  geom_point(
    data = HIGHLIGHTED_HOUSING,
    aes(
      x = year,
      y = housing_per_1000,
      text = hover_text,
      group = GEOID
    ),
    inherit.aes = FALSE,
    color = "transparent"
  ) +
  scale_color_manual(values = c(
    "Moderate Growth" = "#7E57C2",
    "Strong Growth" = "#4A148C"
  )) +
  scale_x_continuous(breaks = seq(2009, 2023, 3), expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  labs(
    title = "Proportional Housing Growth Over Time by CBSA",
    subtitle = "Purple lines indicate CBSAs with top Moderate or Strong housing growth; faint gray lines show lower growth.",
    x = "Year",
    y = "Housing Units per 1000 People",
    color = "Housing Trend"
  ) +
  theme_cowplot() +
  theme(
    legend.position = "bottom",
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 10),
    plot.subtitle = element_text(size = 10, margin = margin(t = 10, b = 10))
  )

# Step 3: Convert to Plotly
ggplotly(HOUSING_GROWTH_PLOT, tooltip = "text") %>%
  layout(
    legend = list(
      orientation = "h",
      x = 0.5,
      xanchor = "center",
      y = -0.28
    ),
    hoverlabel = list(align = "left"),
    # <-- force left alignment
    margin = list(b = 130),
    annotations = list(
      list(
        x = -0.08,
        xref = "paper",
        y = -0.55,
        yref = "paper",
        text = caption_text,
        showarrow = FALSE,
        xanchor = "left",
        yanchor = "bottom",
        font = list(size = 9, color = "gray"),
        align = "left"
      )
    )
  )







HOUSING_HIGHLIGHT_CHECK <- HOUSING_GROWTH_FLAGGED |>
  filter(pop_slope > 0.5 &
           housing_slope > mean(housing_slope, na.rm = TRUE)) |>
  select(NAME, GEOID, year, pop_slope, housing_slope)

# Preview the first few
HOUSING_GROWTH_TREND |>
  select(housing_slope, pop_slope)|>
  print(n = 40)


HOUSING_GROWTH_TREND|>
  summarize(mean(pop_slope))
