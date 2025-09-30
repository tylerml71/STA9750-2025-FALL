library(tidyverse)
options(max.print=25) 
head(penguins, 10)

select(penguins, species, island) # subsetting columns

penguins |> select(species, island) # same output

penguins |> select(-bill_len, -bill_dep, -flipper_len) #dropping columns instead of selecting 2

penguins |> filter(sex == "male", bill_len > 38) # data that passes ALL tests

penguins |> filter( (sex == "male") | (bill_len > 38)) #data that passes either test

penguins |> filter(species == "Adelie") |> slice_max(body_mass, n=5) # Slice performes the "top k" operations

penguins |> mutate(bill_len_in = bill_len / 25.4) #converiing millileters to inches

penguins |> rename(body_mass_g = body_mass) #Changing column name

penguins |> 
  filter(sex == "male") |>
  summarize(number = n()) #n() counts the number of rows; summarize combines and reduces rows
