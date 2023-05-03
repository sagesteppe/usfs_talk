#' split each state into three somewhat similar area sections
#' 
#' @param x an sf dataset of polygons
#' @param z number of pieces to split polygon into
state_splitter <- function(x, pieces){
  
  # make the grid
  
  gr <- x %>% 
    st_make_grid(n = c(10, 10), what = 'polygons', square = F) %>% 
    st_crop(., x) %>% 
    st_collection_extract('POLYGON') %>% 
    st_as_sf() %>% 
    st_make_valid() %>% 
    rename( 'geometry' = 1)
  y <- gr[st_intersects(gr, x) %>% lengths() > 0, ]
  
  ############### now split
  
  t_area <- y[sf::st_intersects(y, x) |> lengths() > 0, ] %>% 
    dplyr::mutate(TEMPID = 1:nrow(.), .before = geometry)
  
  t_area <- st_intersection(t_area, x)
  
  n_cells <- round( nrow(t_area) / pieces, 0)
  grpL <- c( rep(n_cells, (pieces - 1)), nrow(t_area) - (n_cells * (pieces - 1)) ) 
  
  t_area <- t_area %>% 
    dplyr::mutate(TEMPGRP = rep( LETTERS[1:pieces], grpL), .before = geometry) %>% 
    dplyr::select(-TEMPID)
  
}
