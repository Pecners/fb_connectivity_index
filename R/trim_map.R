library(rnaturalearth)



trim_map <- function(map) {
  l <- rnaturalearth::ne_download(type = "lakes", category = "physical", scale = "large")  %>%
    st_as_sf(., crs = st_crs(map))
  
  lakes <- c("Lake Erie",
             "Lake Michigan",
             "Lake Superior",
             "Lake Huron",
             "Lake Ontario")
  gl <- l %>%
    filter(name %in% lakes) %>%
    st_transform(crs = st_crs(map)) |> 
    st_union()
  
  land <- rnaturalearth::ne_download(type = "land", category = "physical", scale = "large")  %>%
    st_as_sf() %>%
    st_transform(., crs = st_crs(map)) |> 
    st_union()
  
  
  skinny_s <- st_difference(map, gl)
  
  skinny_s <- st_intersection(skinny_s, land)
}

