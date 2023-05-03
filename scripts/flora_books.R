library(tigris)
library(tidyverse)
library(sf)
library(spData)
library(patchwork)


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

ak <- filter(st, NAME == 'Alaska') %>% select(NAME)
il <- filter(us_states, NAME == 'Illinois') %>% select(NAME)
f_chi_r  <- counties(state = 'IL') %>% 
  filter(NAME %in% c('Boone', 'Cook', 'DeKalb', 'DuPage', 'Kane', 'Kankakee', 
                     'Kendall', 'Grundy','Lake', 'McHenry','Will')) %>% 
  st_intersection(., il) %>% 
  st_union() %>% 
  st_as_sf() %>% 
  rename(geometry = x) %>% 
  mutate(region_code = 'CHI', .before = 1) %>% 
  st_transform(4326)

st_west <- st_crop(st, xy) %>% 
  bind_rows(., il) %>% 
  bind_rows(., ak) %>% 
  st_transform(4326)

rm(xy, ak, il, st)

admin_floras <- data.frame(
  states = c(
    'California', 'Colorado', 'New Mexico',
    'Montana', 'Wyoming', 'Utah', 
    'Illinois', 'Oregon', 'Arizona', 'Alaska'
  ),
  flora = c(
    'Jepson Manual 2nd', 'A Colorado Flora 2nd', 'Flora Neomexicana',
    'Manual of Montana Vascular Plants 2nd', 'Vascular Plants of Wyoming', 'A Utah Flora 3rd', 
    'Vascular Flora of Illinois 4th', 'Flora of Oregon', 'Arizona Flora 2nd', 'Flora of Alaska'
  ),
  authors = c(
    'Baldwin, Goldman, Keil, Patterson, Patterson, Wilken', 'Ackerfield', 'Allred', 
    'Lesica', 'Dorn', 'Welsh Atwood, Higgins, Goodrich', 
    'Mohlenbrock',  'Meyers, Jaster, Mitchell, Hardison, Harvey', 'Kearney, Peebles', 'Anderson, Welsh'
  ),
  status = c(
    'Current - Online', 'Current', 'Current', 
    'Current', 'Aging', 'Aging', 
    'Curent', 'Current - Not complete', 'Aged - Not complete', 'Aged'
  )
) %>% 
  left_join(., st_west, by = c('states' = 'NAME')) %>% 
  st_as_sf()



regional_floras <- data.frame(
  regions = c(
    'Great Basin', 'Pacific Northwest', 'Great Plains', 
    'Uinta', 'Four Corners', 'Steens Mountain', 
    'Chicago'
  ), 
  flora = c(
    'Intermountain Flora', 'Flora of the Pacific Northwest', 'Flora of the Great Plains', 
    'Uinta Flora', 'Flora of the Four Corners Region', 'Flora of Steens Mountain', 
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
  ),
  region_code = c('IMF', 'FPNW', 'FGPR', 'Uintah', 'FFCR', 'Steens', 'CHI')
)


p <- '../data/raw/flora_regions/'
fr <- paste0(p, 
  list.files(p, pattern = 'shp'))

regions <-  do.call(rbind, lapply(fr, function(x) st_read(x, quiet = T)))
regions <- bind_cols(regions, region_code = gsub('.shp', '', gsub(p, '', fr))) %>% 
  select(-id) %>% 
  bind_rows(., f_chi_r) %>% 
  st_make_valid() 

regions <- st_intersection(regions, st_union(st_west))
regional_floras <- left_join(regional_floras, regions, by = 'region_code') %>% 
  st_as_sf()

rm(p, fr, f_chi_r, regions, st_west)


################################################################################
#######                        COLOR PALETTES                           ########
################################################################################

cols1 <- setNames(
  c('#a6cee3','#1f78b4','#b2df8a',
           '#33a02c','#fb9a99','#e31a1c',
           '#fdbf6f','#ff7f00','#cab2d6',
           '#6a3d9a','#C07F00','#b15928'),
 c('Flora Neomexicana', 'Jepson Manual 2nd', 'Manual of Montana Vascular Plants 2nd', 
          'Flora of the Pacific Northwest', 'A Utah Flora 3rd', 'Vascular Plants of Wyoming', 
          'Intermountain Flora', 'A Colorado Flora 2nd', 'Flora of Oregon',
          'Flora of Steens Mountain', 'Flora of the Great Plains', 'Arizona Flora 2nd')
)

cols2 <- setNames(
  c('#434371', '#79AEA3', '#484041'), 
  c('Flora of the Chicago Region', 'Vascular Flora of Illinois 4th', 'Flora of Alaska')
)

cols3 <- setNames(
  c('#CB8589', '#86836D'), 
  c('Uinta Flora', 'Flora of the Four Corners Region')
)

f_cols <- c(cols1, cols2, cols3)
rm(cols1, cols2, cols3)

################################################################################
######                           ALASKA PLOT                            ########
################################################################################

ak <- filter(admin_floras, states == 'Alaska')
ak32 <- st_transform(ak, 3338)

akp <- ggplot() +
  geom_sf(data = ak32, aes(fill = flora)) +
  scale_fill_manual(values = f_cols) +
  theme_void() + 
  theme(legend.position = 'none')

################################################################################
######                         ILLINOIS PLOT                            ########
################################################################################

chi <- filter(regional_floras, regions == 'Chicago')
il <- filter(admin_floras, states == 'Illinois')

il <- ggplot() +
  geom_sf(data = il, aes(fill = flora)) +
  geom_sf(data = chi, aes(fill = flora)) + 
  scale_fill_manual(values = f_cols) +
  theme_void() +
  theme(legend.position = 'none')

################################################################################
#########                    WESTERN PLOTS                              ########
################################################################################

reg1 <- filter(regional_floras, regions == 'Great Plains')
reg2 <- filter(regional_floras, regions %in% c('Great Basin', 'Pacific Northwest'))
reg3 <- filter(regional_floras, regions %in% c('Steens Mountain', 'Four Corners', 'Uinta'))
ad_sub <- filter(admin_floras, ! states %in% c('Illinois', 'Alaska'))

west <- ggplot() +
  geom_sf(data = reg1, aes(fill = flora)) + 
  geom_sf(data = ad_sub, aes(fill = flora)) +
  geom_sf(data = reg2, aes(fill = flora)) + 
  geom_sf(data = reg3, aes(fill = flora)) + 
  scale_fill_manual(values = f_cols) +
  theme_void() +
  theme(legend.position = 'none')

rm(reg1, reg2, reg3, ad_sub)

fd <- bind_rows(admin_floras, regional_floras) %>% 
  mutate(flora = str_pad(flora, str_length(.$flora) + 7, "right"))


leg <- ggpubr::get_legend(
  ggplot() + 
  geom_sf(data = admin_floras, aes(fill = flora)) + 
  geom_sf(data = regional_floras, aes(fill = flora)) + 
  scale_fill_manual('Flora', values = f_cols) +
  theme(legend.title.align = 0.5, 
        legend.spacing.x = unit(0.8, 'cm'),
        legend.spacing.y = unit(0.4, 'cm')) + 
  guides(fill=guide_legend(ncol=3))
)


(il | akp | west) /
    leg
ggsave('../results/FlorasByRegion.png', device = 'png', dpi = 150, units = "px",
       width = 1920, height = 1080)

# rm(il, akp, west, leg)
################################################################################
#######                        FLORA STATUS TABLE                       ########      
################################################################################

af <- admin_floras %>% 
  select(flora, authors, status) %>% 
  st_drop_geometry()

rf <- regional_floras %>% 
  select(flora, authors, status) %>% 
  st_drop_geometry()

f <- bind_rows(af, rf)
                   
mtcars[1:8, 1:8] %>%
  kbl() %>%
  kable_paper(full_width = F) %>%
  column_spec(2, color = spec_color(mtcars$mpg[1:8]),
              link = "https://haozhu233.github.io/kableExtra/") %>%
  column_spec(6, color = "white",
              background = spec_color(mtcars$drat[1:8], end = 0.7),
              popover = paste("am:", mtcars$am[1:8]))
