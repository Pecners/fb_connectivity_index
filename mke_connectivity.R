library(tidyverse)
library(tigris)
library(sf)
library(MetBrewer)
library(colorspace)
library(magick)
library(glue)
library(ggstar)
library(showtext)
library(crayon)
source("R/trim_map.R")

font_add("Poller One", "/Library/Fonts/PollerOne-Regular.ttf")

d <- read_tsv("data/county_county.tsv")

all_c <- counties(state = state.abb)
states <- states()
exclude_st <- c(
  "60",
  "69",
  "11",
  "66",
  "72",
  "78",
  "02",
  "15"
)
contig <- states |> 
  filter(!GEOID %in% exclude_st)

skinny_s <- trim_map(contig) |> 
  filter(!STATEFP %in% c("02", "15"))

skinny_c <- trim_map(all_c)

with_name <- left_join(skinny_c, 
                       skinny_s |> 
                         as_tibble() |> 
                         select(STATEFP = GEOID, STATE = NAME)) |> 
  transmute(name = paste(NAMELSAD, STATE, sep = ", "),
            GEOID)

joined <- left_join(d, 
                    with_name |> 
                      select(fr_loc = GEOID))

these_counties <- read_csv("data/county_list_citied.csv")

c_pal <- met.brewer("Hiroshige", n = 14)
cp <- c_pal[7:1]
swatchplot(c_pal)
swatchplot(cp)

sec_font <- "El Messiri"

walk2(these_counties$county, these_counties$city, function(tc, city) {
  cat(cyan(glue("Starting: {tc}")), "\n")
  this_county <- with_name |> 
    filter(name == tc) |> 
    pull(GEOID)
  
  
  skinny_dc <- joined |> 
    filter(user_loc == this_county) |> 
    st_as_sf()
  
  quants <- quantile(skinny_dc$scaled_sci, seq(from = 0, to = 1, by = .15))
  
  dc_ranked <- skinny_dc |> 
    mutate(rank = case_when(scaled_sci < quants[2] ~ "Bottom 15%",
                            scaled_sci < quants[3] ~ "15-30%",
                            scaled_sci < quants[4] ~ "30-45%",
                            scaled_sci < quants[5] ~ "45-60%",
                            scaled_sci < quants[6] ~ "60-75%",
                            scaled_sci < quants[7] ~ "75-90%",
                            TRUE ~ "Top 10%"),
           rank = factor(rank, levels = c("Bottom 15%",
                                          "15-30%",
                                          "30-45%",
                                          "45-60%",
                                          "60-75%",
                                          "75-90%",
                                          "Top 10%")))
  
  cent <- st_centroid(skinny_dc |> 
                        filter(fr_loc == this_county)) 
  
  p <- dc_ranked |> 
    filter(!str_detect(fr_loc, "^02.*|^15.*")) |> 
    ggplot(aes(fill = rank)) +
    geom_sf(color = "white", linewidth = .01) +
    geom_sf(data = skinny_s, color = "white", fill = NA) +
    geom_sf(data = cent, shape = 23, size = 4,
            fill = "white", color = c_pal[14], stroke = 3,
            inherit.aes = FALSE) +
    scale_fill_manual(values = cp) +
    coord_sf(crs = 5070) +
    theme_void() +
    theme(text = element_text(color = c_pal[7]),
          legend.position = "bottom", 
          plot.margin = margin(r = 25, l = 25)) +
    labs(fill = "Rank by Connectedness") +
    guides(fill = guide_legend(title.position = "top", 
                               title.theme = element_text(size = 18,
                                                          color = c_pal[7], 
                                                          family = "Poller One"),
                               label.theme = element_text(size = 14,
                                                          color = c_pal[7],
                                                          family = sec_font),
                               nrow = 1, byrow = TRUE))

  tmp_f <- tempfile(fileext = ".png")
  ggsave(glue("{tmp_f}"), p, bg = c_pal[12],
         w = 9, h = 16)
  
  img <- image_read(glue("{tmp_f}"))
  # image_info(img)
  
  sub <- glue("Social connectedness with other counties") 
  
  if (str_length(tc) > 25) {
    big_font <- 100
  } else {
    big_font <- 125
  }
  
  img |> 
    image_annotate(text = tc, 
                   gravity = "west", 
                   size = big_font,
                   color = c_pal[6],
                   font = "Poller One",
                   location = "+150-1600",
                   weight = 900) |> 
    image_annotate(text = glue("Major City: {city}"), 
                   gravity = "west", 
                   size = 80,
                   color = c_pal[7],
                   font = "Poller One",
                   location = "+150-1400",
                   weight = 900) |> 
    image_annotate(text = sub, 
                   gravity = "west", 
                   size = 100,
                   color = c_pal[7],
                   font = sec_font,
                   location = "+150-1200") |> 
    # left arrow
    image_annotate(text = "\u2190", 
                   gravity = "west", 
                   size = 150,
                   color = c_pal[7],
                   location = "+150+1050",
                   weight = 900) |> 
    image_annotate(text = "less connected", 
                   gravity = "west", 
                   size = 60,
                   color = c_pal[7],
                   font = sec_font,
                   location = "+325+1065") |> 
    # right arrow
    image_annotate(text = "\u2192", 
                   gravity = "east", 
                   size = 150,
                   color = c_pal[7],
                   location = "+175+1050",
                   weight = 900) |> 
    image_annotate(text = "more connected", 
                   gravity = "east", 
                   size = 60,
                   color = c_pal[7],
                   font = sec_font,
                   location = "+350+1065") |> 
    # caption
    image_annotate(text = glue("Graphic by Spencer Schien (@MrPecners) | ",
                               "Data from Facebook's Social Connectedness Index"), 
                   gravity = "south", 
                   size = 40,
                   color = alpha(c_pal[7], .5),
                   font = sec_font,
                   location = "+0+1200") |> 
    image_write(glue("images/{tc}.png"))
  
  
})

