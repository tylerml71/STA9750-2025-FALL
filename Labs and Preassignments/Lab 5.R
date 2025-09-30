library(nycflights13)
library(DT)
glimpse(flights)
glimpse(airlines)
glimpse(airports)
glimpse(planes)
glimpse(weather)

inner_join(flights, airports, join_by(origin == faa))

inner_join(flights, 
           weather, 
           join_by(origin == origin,
                   year == year,
                   month == month,
                   day == day,
                   hour == hour))
flights |> 
  inner_join(airlines, join_by(carrier == carrier)) |> 
  inner_join(airports, join_by(origin == faa)) 

flights |> 
  inner_join(airlines, join_by(carrier == carrier)) |> 
  inner_join(airports, 
             join_by(origin == faa), 
             suffix = c("_airline", "_origin_airport")) 
west_coast_airports <- airports |> filter(tzone == "America/Los_Angeles")

inner_join(flights, west_coast_airports, join_by(dest == faa))

datatable(flights)|>
  
#What is the name of the airline with the longest average departure delay?
flights |> inner_join(airlines, join_by(carrier==carrier)) |> 
  group_by(name)|>
  summarise(max_avg_dep_delay = mean(dep_delay,na.rm=TRUE))|>
  slice_max(name)|>
    datatable()

#What is the name of the origin airport with the longest average departure delay?
flights |>
  group_by(origin)|>
  summarize(mean(dep_delay, na.rm=TRUE))|>
  inner_join(airports, join_by(origin==faa))|>
  slice_max(name)|>
  pull(name)

#What is the name of the destination airport with the longest average departure delay?
flights |>
  group_by(dest)|>
  summarize(mean(dep_delay, na.rm=TRUE))|>
  inner_join(airports, join_by(dest==faa))|>
  slice_max(name)|>
  pull(name)

#Are average delays longer for East-coast destinations or West-coast destinations?
flights |> 
  group_by(dest) |> 
  summarize(mean_dep_delay = mean(dep_delay, na.rm=TRUE)) |> 
  inner_join(airports, join_by(dest == faa)) |> 
  group_by(tzone) |> 
  summarize(mean_dep_delay = mean(mean_dep_delay, na.rm=TRUE)) |> 
  filter(tzone %in% c("America/New_York", "America/Los_Angeles")) |> 
  slice_max(mean_dep_delay) |> 
  pull(tzone)

