library(tidycensus)

load_variables(2020, dataset = "pl")
dec <- get_decennial("county", variables = "P1_001N", year = "2020")

highest <- dec |> 
  mutate(state = str_extract(NAME, "(?<=, ).*")) |> 
  arrange(state, desc(value)) |> 
  group_by(state) |> 
  mutate(id = row_number()) |> 
  filter(id == 1) |> 
  select(county = NAME)

write_csv(highest, "data/county_list.csv")
