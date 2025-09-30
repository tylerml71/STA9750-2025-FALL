if(!require("tidyverse")) install.packages("tidyverse")
if(!require("nycflights13")) install.packages("nycflights13")
library(tidyverse)
library(nycflights13)
glimpse(flights)
flights |> filter(arr_delay == max(arr_delay))
flights |> filter(arr_delay == 60)
flights |> summarize(max(arr_delay))
any(c(TRUE, TRUE))
any(c(FALSE, TRUE))
today_temp    <- NA
tomorrow_temp <- NA

today_temp == tomorrow_temp


flights |> summarize(max(arr_delay, na.rm=TRUE)) #na.rm renoves NA values
flights |> filter(arr_delay == max(arr_delay, na.rm=TRUE)) #Either works

flights |> filter(!is.na(arr_delay)) #Remove NA values

flights |> 
  filter(!is.na(arr_delay)) |>
  filter(arr_delay == max(arr_delay)) |>
  glimpse()

flights |> filter(!is.na(arr_delay)) |> summarize(mean(arr_delay)) #Looks for TRUE, not "not false"

flights |> filter(is.na(arr_delay)) |> NROW()
flights |> filter(is.na(arr_delay), !is.na(arr_time)) |> NROW()

library(nycflights13) # Load the flights data
filter(flights, month == 1, day == 1)
jan1<- filter(flights, month == 1, day == 1)

nov_dec <- filter(flights, month %in% c(11, 12))
nov_dec

dml1 <- filter(flights, !(arr_delay > 120 | dep_delay > 120)) #same way to find flights that weren't delayed by more than 2 hours
dml2 <- filter(flights, arr_delay <= 120, dep_delay <= 120)
identical(dml1, dml2)
dml1

flights |> filter(dest == 'IAH' | dest == 'HOU') |>select(dest)
flights |> filter(xor(month<5,month<9))
flights |> View()

