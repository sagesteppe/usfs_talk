library(tigris)
library(tidyverse)
library(sf)
library(spData)
library(patchwork)

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

consort <- c('Cons. of Pacific Northwest Herbaria', 
            'California Consortium of Herbaria',
            'Intermountain Region Herbarium Network',
            'Southwest Biodiversity SEINet', 
            'Southern Rocky Mountain Herbaria',
            'Consortium of Midwest Herbaria',
            'Cons. of Northern Great Plains Herbaria',
            'Rocky Mountain Herbarium'
)

con <- data.frame(
  consortium = c(
    rep(consort[1], 5),
    consort[2], 
    rep(consort[3], 2),
    rep(consort[4], 2),
    consort[5],
    consort[6],
    rep(consort[7], 6),
    consort[8]
    ),
  state = c(
    'Alaska', 'Washington', 'Oregon', 'Idaho', 'Montana', # CPNWH
    'California', # CCH
    'Nevada', 'Utah', # IMH
    'Arizona', 'New Mexico', # SEINet
    'Colorado', # SORO
    'Illinois', # Midwest
    'North Dakota', 'South Dakota', 'Nebraska', 'Oklahoma', 'Kansas', 'Texas',
    'Wyoming'
    ),
  symbiota = c(
    rep(F, 5),
    rep(T, 13),
    F
  )
) %>% 
  left_join(., st_west, by = c('state'= 'NAME')) %>% 
  st_as_sf()

################################################################################
#######                        COLOR PALETTES                           ########
################################################################################



cols1 <- setNames(
  c('#1b9e77','#d95f02','#7570b3','#e7298a','#66a61e','#e6ab02','#a6761d', '#666666'),
  consort
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


pw <- (il | akp | west) /
  leg

pw + plot_annotation(
  title = 'herbaria consortia',
  subtitle = "", 
  theme = theme(
    plot.title = element_text(size = 32, family = "sans serif", hjust = 0.5),
    plot.subtitle = element_text(size = 22))
) 

ggsave('../results/ConsortiumByRegion.png', device = 'png', dpi = 150, units = "px",
       width = 1920, height = 1080)
