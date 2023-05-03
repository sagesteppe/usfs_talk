library(tigris)
library(tidyverse)
library(sf)
library(spData)
library(patchwork)
source('functions.R')

st <- states()  %>% 
  select(NAME)

xy <- data.frame(
  x = c(-100.72, -100.67, -124.12, -125.31),
  y = c(50.00, 30, 30, 50.00)
) %>%
  st_as_sf(coords = c('x', 'y'), crs = 4269) %>% 
  st_union() %>% 
  st_cast('POLYGON') %>% 
  st_as_sf()

ak <- filter(st, NAME == 'Alaska') %>% select(NAME)
il <- filter(us_states, NAME == 'Illinois') %>% select(NAME)

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


# now if the state has less than three sections reduce 

websites <- data.frame(
  website = c(
    rep('Oregon Flora Image Project', 9),
    rep('Burke Herbarium Image Collection', 4), 
    rep('Montana Natural Heritage Program', 6),
    rep('Calflora-Calphotos', 3), 
    rep('SW CO Wildflowers', 4),
    rep('SEINet', 5),
    rep('American SW', 2),
    'Illinois Wildflowers', 'Illinois Plants', 'Phytokeys',
    'Alaska Wildflowers',
    rep('US Wildflowers', 2)
  ),
  state = c(
    'OR', 'MT', 'ID', 'CA', 'NV', 'CO', 'WY', 'AK', 'UT', # OFIP
    'WA', 'MT', 'ID', 'AK',  # BHIC
    'MT', 'ID', 'WY', 'AK', 'ND', 'SD', # MTNHP
    'CA', 'NV', 'AZ', # calflora
    'CO', 'UT', 'WY', 'NM', # sw co wildflowers
    'NV', 'CA', 'UT', 'AZ', 'TX', # SEINet
    'AZ', 'NM', # american southwest
    'IL', 'IL', 'IL',
    'AK',
    'NE', 'KA'
  )
) %>% 
  arrange(state)

grps_b_state <- websites %>% 
  count(state)


################################################################################
#######                        COLOR PALETTES                           ########
################################################################################


web <- c('Oregon Flora Image Project', 'Burke Herbarium Image Collection',
'Montana Natural Heritage Program', 'Calflora-Calphotos', 'SW CO Wildflowers', 
'SEINet', 'American SW', 'Illinois Wildflowers', 'Illinois Plants', 'Phytokeys',
'Alaska Wildflowers', 'US Wildflowers')


cols1 <- setNames(
  c('#8dd3c7','#ffffb3','#bebada','#fb8072','#80b1d3','#fdb462',
             '#b3de69','#fccde5','#d9d9d9','#bc80bd','#ccebc5','#ffed6f'),
             web
)


################################################################################
######                           ALASKA PLOT                            ########
################################################################################

ak <- filter(con, state == 'Alaska')
ak32 <- st_transform(ak, 3338)

akp <- ggplot() +
  geom_sf(data = ak32, aes(fill = consortium)) +
  scale_fill_manual(values = cols1) +
  theme_void() + 
  theme(legend.position = 'none')


################################################################################
######                         ILLINOIS PLOT                            ########
################################################################################

il <- filter(con, state == 'Illinois')

il <- ggplot() +
  geom_sf(data = il, aes(fill = consortium)) +
  scale_fill_manual(values = cols1) +
  theme_void() +
  theme(legend.position = 'none')

################################################################################
#########                    WESTERN PLOTS                              ########
################################################################################

west_sub <- filter(con, ! state %in% c('Alaska', 'Illinois'))

west <- ggplot() +
  geom_sf(data = west_sub, aes(fill = consortium)) + 
  scale_fill_manual(values = cols1) +
  theme_void() +
  theme(legend.position = 'none')

west

leg <- ggpubr::get_legend(
  ggplot() + 
    geom_sf(data = con, aes(fill = consortium)) + 
    scale_fill_manual('Herbaria Consortium', values = cols1) +
    theme(legend.title.align = 0.5, 
          legend.spacing.x = unit(1, 'cm'),
          legend.spacing.y = unit(0.4, 'cm')) + 
    guides(fill=guide_legend(ncol=3))
)


(il | akp | west) /
  leg
ggsave('../results/ConsortiumByRegion.png', device = 'png', dpi = 150, units = "px",
       width = 1920, height = 1080)
