
library(tigris)
library(tidyverse)
library(sf)
library(spData)


st <- states()  %>% 
  select(NAME)

xy <- data.frame(
  x = c(-100.72, -100.67, -124.12, -125.31),
  y = c(50.00, 31.43, 31.26, 50.00)
) %>%
  st_as_sf(coords = c('x', 'y'), crs = 4269) %>% 
  st_union() %>% 
  st_cast('POLYGON') %>% 
  st_as_sf()

st_west <- st_crop(st, xy)


ak <- filter(us_states, NAME == 'Alaska')
il <- filter(us_states, NAME == 'Illinois')
flora_CHI_region  <- counties(state = 'IL') %>% 
  filter(NAME %in% c('Boone', 'Cook', 'DeKalb', 'DuPage', 'Kane', 'Kankakee', 
                     'Kendall', 'Grundy','Lake', 'McHenry','Will')) %>% 
  st_intersection(., il)


admin_floras <- data.frame(
  states = c(
    'California', 'Colorado', 'New Mexico',
    'Montana', 'Wyoming', 'Utah', 
    'Illinois', 'Oregon', 'Arizona'
  ),
  flora = c(
    'Jepson Manual 2nd', 'A Colorado Flora 2nd', 'Flora Neomexicana',
    'Manual of Montana Vascular Plants 2nd', 'Vascular Plants of Wyoming', 'A Utah Flora 3rd', 
    'Vascular Flora of Illinois 4th', 'Flora of Oregon', 'Arizona Flora 2nd'
  ),
  authors = c(
    'Baldwin, Goldman, Keil, Patterson, Patterson, Wilken', 'Ackerfield', 'Allred', 
    'Lesica', 'Dorn', 'Welsh Atwood, Higgins, Goodrich', 
    'Mohlenbrock',  'Meyers, Jaster, Mitchell, Hardison, Harvey', 'Kearney, Peebles'
  ),
  status = c(
    'Current - Online', 'Current', 'Current', 
    'Current', 'Aging', 'Aging', 
    'Curent', 'Current - Not complete', 'Aged - Not complete'
  )
)


regional_floras <- data.frame(
  regions = c(
    'Great Basin', 'Pacific Northwest', 'Great Plains', 
    'Uinta', 'Four Corners', 'Steens Mountains', 
    'Chicago'
  ), 
  flora = c(
    'Intermountain Flora', 'Flora of the Pacific Northwest', 'Flora of the Great Plains', 
    'Uinta Flora', 'Flora of the Four Corners Region', 'Flora of Steens Mountains', 
    'Flora of the Chicago Region'
  ),
  authors = c(
    'Cronquist, Reveal, Holmgrens, Barneby', 'Hitchcock, Cronquist', 'Many',
    'Goodrich, Huber', 'Heil, Kane, Reeves, Clifford', 'Mansfield', 
    'Wilhelm, Rericha'
  ), 
  status = c(
    'Aged-Current', 'Current', 'Aging', 
    'Current', 'Current', 'Current', 
    'Current'
  )
)





