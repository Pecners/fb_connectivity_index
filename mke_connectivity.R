library(tidyverse)
library(tigris)
library(sf)
library(MetBrewer)

d <- read_tsv("data/county_county.tsv")

all_c <- counties(state = state.abb)

mke <- all_c |> 
  filter(NAME == "Milwaukee") |> 
  pull(GEOID)


dc <- left_join(d |> 
                  filter(user_loc == mke),
                all_c |> 
                  select(fr_loc = GEOID))

quants <- quantile(dc$scaled_sci, seq(from = 0, to = 1, by = .15))

dc_ranked <- dc |> 
  mutate(rank = case_when(scaled_sci < quants[2] ~ "0-15%",
                          scaled_sci < quants[3] ~ "15-30%",
                          scaled_sci < quants[4] ~ "30-45%",
                          scaled_sci < quants[5] ~ "45-60%",
                          scaled_sci < quants[6] ~ "60-75%",
                          scaled_sci < quants[7] ~ "75-90%",
                          TRUE ~ "+90%"),
         rank = factor(rank, levels = c("0-15%",
                                           "15-30%",
                                           "30-45%",
                                           "45-60%",
                                           "60-75%",
                                           "75-90%",
                                           "+90%")))

dc_ranked |> 
  ggplot(aes(x = as.numeric(fr_loc),
             y = scaled_sci)) +
  geom_point()

p <- dc_ranked |> 
  filter(!str_detect(fr_loc, "^02.*|^15.*|^55.*")) |> 
  st_as_sf() |> 
  ggplot(aes(fill = rank)) +
  geom_sf(color = NA) +
  scale_fill_met_d(name = "VanGogh3") +
  theme_void()

ggsave("mke.png", p, bg = "grey70")
