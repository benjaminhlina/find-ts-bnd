# ---- bring packages ---- 
{
  library(dplyr)
  library(ggplot2)
  library(here)
  library(lwgeom) # st_startpoint
  library(mapview)
  library(purrr)
  library(readr)
  library(sf)
  make_line <- function(lon, lat, llon, llat) {
    st_linestring(matrix(c(lon, llon, lat, llat), 2, 2,))
  }
}

# bring in csv 
dat <- read_csv(here("data", 
                     "example_movement_data.csv"))



glimpse(dat)

dat 
# ---- create example boundary ---- 
bnd <- st_read(dsn = here("shapefile", 
                          "."), 
               layer = "example_mpa")

# ---- plot tracks ----- 

ggplot() + 
  geom_sf(data = bnd, fill = NA, colour = "blue") + 
  geom_line(data = dat, aes(x = lon, y = lat)) + 
  theme_bw(
    base_size = 15
  ) + 
  theme(
    panel.grid = element_blank(),
    axis.text = element_text(colour = "black")
  ) + 
  labs(
    x = "Longitude", 
    y = "Latitude"
  )

# ---- create from and to dataframe ---- 
to_from <- dat %>% 
  mutate(
    from_ts = dplyr::lag(time_stamp, default = first(time_stamp)),
    to_ts = time_stamp,
    llat = dplyr::lag(lat, default = first(lat)),
    llon = dplyr::lag(lon, default = first(lon)),
  ) %>% 
  dplyr::select(-time_stamp)

to_from



tracks_sf <- to_from %>%
  dplyr::select(lon, lat, llon,llat) %>% 
  pmap(make_line) %>%
  st_as_sfc(crs = 4326) %>%
  st_sf() %>% 
  mutate(
    lon = st_startpoint(.) %>%
      st_coordinates(.) %>%
      as_tibble() %>%
      .$X %>% 
      as.character(),
    llon = st_endpoint(.) %>%
      st_coordinates(.) %>%
      as_tibble() %>%
      .$X %>% 
      as.character()
  )

to_from_select <- to_from %>% 
  mutate(across(.cols = c(lon, lat, llon, llat), as.character)) %>% 
  dplyr::select(id, lon, llon, from_ts, to_ts)


tracks_sf <- tracks_sf %>% 
  left_join(to_from_select,
            by = c("lon", "llon")
  ) %>% 
  dplyr::select(-c("lon", "llon"))



tracks_sf <- tracks_sf %>% 
  mutate(
    x_bnd = st_intersects(bnd, tracks_sf, sparse = FALSE)[TRUE]
  )

glimpse(tracks_sf)

tracks_sf_x <- tracks_sf %>% 
  filter(x_bnd %in% TRUE)


# ----- plot all tracks that cross boundary ---- 
ggplot() + 
  geom_sf(data = bnd, colour = "blue", size = 1) + 
  geom_sf(data = tracks_sf_x)


# ---- create time differnce column ---- 
tracks_sf_x <- tracks_sf %>% 
  mutate(
    time_diff = difftime(to_ts, from_ts)
  )


tracks_sf_x

# you will notice that the difference in time is consistent as the exmaple dataset 
# use a manufactured sequence I created that is equally spaced so there more than 
# won't be any change 