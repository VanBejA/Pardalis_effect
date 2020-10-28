#' ---
#' title: Pardalis effect
#' author: 
#' date: 2020-10-28
#' ---

# packages ----------------------------------------------------------------
library(here)
library(tidyverse)
library(sf)
library(raster)
library(ecospat)

# download ----------------------------------------------------------------
# data paper
download.file(url = "https://esajournals.onlinelibrary.wiley.com/action/downloadSupplement?doi=10.1002%2Fecy.3128&file=ecy3128-sup-0001-DataS1.zip",
              destfile = here::here("01_communities", "ecy3128-sup-0001-DataS1.zip"), mode = "wb")

# unzip
unzip(zipfile = here::here("01_communities", "ecy3128-sup-0001-DataS1.zip"),
      exdir = here::here("01_communities"))

# import ------------------------------------------------------------------
# communities
co <- readr::read_csv(here::here("01_communities", "NEOTROPICAL_CARNIVORES_DATASET_2020-04.csv"))
co

# vector
co_v <- co %>% 
  tidyr::drop_na(LONG_X, LAT_Y) %>% 
  dplyr::mutate(lon = LONG_X, lat = LAT_Y) %>% 
  sf::st_as_sf(coords = c("lon", "lat"), crs = 4326)
co_v

plot(co_v$geometry, pch = 20)

# prepare desaggregation
co_id <- co %>% 
  tidyr::drop_na(STUDY_RECORD_ID, LONG_X, LAT_Y) %>% 
  dplyr::mutate(STUDY_RECORD_ID = stringr::str_trim(as.character(STUDY_RECORD_ID)), 
                x = LONG_X, 
                y = LAT_Y) %>% 
  dplyr::select(STUDY_RECORD_ID, x, y) %>% 
  dplyr::distinct(x, y, .keep_all = TRUE) %>% 
  dplyr::arrange(STUDY_RECORD_ID) %>% 
  as.data.frame()
co_id
co_id %>% count(STUDY_RECORD_ID) %>% arrange(n)

# desaggregation
co_des <- ecospat::ecospat.occ.desaggregation(
  xy = co_id,
  min.dist = .5, 
  by = "STUDY_RECORD_ID")
co_des



