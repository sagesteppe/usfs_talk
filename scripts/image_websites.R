library(tigris)
library(tidyverse)
library(sf)
library(spData)
library(patchwork)
source('functions.R')

st <- states()  %>% 
  select(STUSPS, NAME)

xy <- data.frame(
  x = c(-100.72, -100.67, -124.12, -125.31),
  y = c(50.00, 30, 30, 50.00)
) %>%
  st_as_sf(coords = c('x', 'y'), crs = 4269) %>% 
  st_union() %>% 
  st_cast('POLYGON') %>% 
  st_as_sf()

ak <- filter(st, NAME == 'Alaska')
il <- filter(us_states, NAME == 'Illinois') %>%
  select(NAME) %>% 
  mutate(STUSPS = 'IL')

st_west <- st_crop(st, xy) %>% 
  bind_rows(., il) %>% 
  bind_rows(., ak) %>% 
  st_transform(4326)

rm(xy, ak, il, st)

################################################################################
###########        GRID OUT AND DIVIDE STATES INTO THREE SECTION        ########
################################################################################

# the grids will be divided into three groups per state, and clustered to make up
# to three bands of colors for the web resources

west <- filter(st_west, ! NAME %in% c('Alaska', 'Illinois'))
west_l <- split(west, west$NAME)

split_states <- lapply(west_l, state_splitter, pieces = 3) %>% 
  bind_rows() 

ggplot() + 
  geom_sf(data = split_states, fill = NA) 

ak <- filter(st_west, NAME == 'Alaska') %>% 
  st_transform(3338)
il <- filter(st_west, NAME == 'Illinois')

ak <- state_splitter(ak, pieces = 4) 
il <- state_splitter(il, pieces = 3)

rm(west_l, state_splitter)

# now if the state has less than three sections reduce 

websites <- data.frame(
  website = c(
    rep('Oregon Flora Image Project', 9),
    rep('Burke Herbarium Image Collection', 4), 
    rep('Montana Natural Heritage Program', 6),
    rep('Calflora-Calphotos', 3), 
    rep('Southwest CO Wildflowers', 4),
    rep('SEINet', 7),
    rep('American Southwest', 2),
    'Illinois Wildflowers', 'Illinois Plants', 'Phytokeys',
    'Alaska Wildflowers',
    rep('US Wildflowers', 3)
  ),
  state = c(
    'OR', 'MT', 'ID', 'CA', 'NV', 'CO', 'WY', 'AK', 'UT', # OFIP
    'WA', 'MT', 'ID', 'AK',  # BHIC
    'MT', 'ID', 'WY', 'AK', 'ND', 'SD', # MTNHP
    'CA', 'NV', 'AZ', # calflora
    'CO', 'UT', 'WY', 'NM', # sw co wildflowers
    'NM', 'NV', 'CA', 'UT', 'AZ', 'TX', 'CO', # SEINet
    'AZ', 'NM', # american southwest
    'IL', 'IL', 'IL',
    'AK',
    'NE', 'KS', 'OK'
  )
) %>% 
  arrange(state) %>% 
  group_by(state) %>% 
  mutate(TEMPGRP = LETTERS[1:n()])

grps_b_state <- websites %>% 
  count(state)

split_states <- left_join(split_states, grps_b_state, by = c('STUSPS' =  'state')) %>% 
  mutate(TEMPGRP = if_else(n == 1,  'A', TEMPGRP)) %>% 
  select(-n) %>% 
  left_join(., websites, by = c('STUSPS' = 'state',  'TEMPGRP')) %>% 
  group_by(STUSPS, TEMPGRP) %>% 
  st_make_valid() %>% 
  mutate(geometry = st_union(geometry)) %>% 
  distinct()

il <- left_join(il, websites, by = c('STUSPS' = 'state',  'TEMPGRP')) %>% 
  st_make_valid() %>% 
  group_by(STUSPS, TEMPGRP) %>% 
  mutate(geometry = st_union(geometry)) %>% 
  distinct(STUSPS, TEMPGRP, .keep_all = T)
ak <- left_join(ak, websites, by = c('STUSPS' = 'state',  'TEMPGRP')) %>% 
  st_make_valid() %>% 
  group_by(STUSPS, TEMPGRP) %>% 
  mutate(geometry = st_union(geometry)) %>% 
  distinct(STUSPS, TEMPGRP, .keep_all = T)

rm(grps_b_state, websites, west, st_west)

################################################################################
#######                        COLOR PALETTES                           ########
################################################################################


web <- c('Oregon Flora Image Project', 'Burke Herbarium Image Collection',
'Montana Natural Heritage Program', 'Calflora-Calphotos', 'Southwest CO Wildflowers', 
'SEINet', 'American Southwest', 'Illinois Wildflowers', 'Illinois Plants', 'Phytokeys',
'Alaska Wildflowers', 'US Wildflowers')

cols1 <- setNames(
  c('#8dd3c7','#ffffb3','#bebada','#fb8072','#80b1d3','#fdb462',
             '#b3de69','#fccde5','#d9d9d9','#bc80bd','#ccebc5','#ffed6f'),
             web
)

rm(web)

################################################################################
######                           ALASKA PLOT                            ########
################################################################################

ak32 <- st_transform(ak, 3338)

akp <- ggplot() +
  geom_sf(data = ak32, aes(fill = website)) +
  scale_fill_manual(values = cols1) +
  theme_void() + 
  theme(legend.position = 'none')

################################################################################
######                         ILLINOIS PLOT                            ########
################################################################################

ilp <- ggplot() +
  geom_sf(data = il, aes(fill = website)) +
  scale_fill_manual(values = cols1) +
  theme_void() +
  theme(legend.position = 'none')

################################################################################
#########                    WESTERN PLOTS                              ########
################################################################################

west <- ggplot() +
  geom_sf(data = split_states, aes(fill = website)) + 
  scale_fill_manual(values = cols1) +
  theme_void() +
  theme(legend.position = 'none')

leg <- ggpubr::get_legend(
  ggplot() + 
    geom_sf(data = split_states, aes(fill = website)) + 
    geom_sf(data = il, aes(fill = website)) + 
    geom_sf(data = ak, aes(fill = website)) + 
    scale_fill_manual('Plant Photo Websites', values = cols1) +
    theme(legend.title.align = 0.5, 
          legend.spacing.x = unit(1, 'cm'),
          legend.spacing.y = unit(0.4, 'cm')) + 
    guides(fill=guide_legend(ncol=3))
)



(ilp | akp | west) /
  leg
ggsave('../results/WebsitesByState.png', device = 'png', dpi = 150, units = "px",
       width = 1920, height = 1080)


rm(ilp, akp, west, leg, cols1, ak32, il, ak, split_states)
