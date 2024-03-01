return_st_bbox_df <- function(sf_df){
  
  bb <- sf::st_bbox(sf_df)

  data.frame(xmin = bb[1], ymin = bb[2],
             xmax = bb[3], ymax = bb[4])

}

add_xy_coords <- function(geo_df){

geo_df |>
    dplyr::pull(geometry) |>
    sf::st_zm() |>
    sf::st_point_on_surface() ->
  points_sf

the_coords <- do.call(rbind, sf::st_geometry(points_sf)) %>%
  tibble::as_tibble() %>% setNames(c("x","y"))

cbind(geo_df, the_coords)

}

define_layer_sf_temp <- function(ref_df,
                                 geom = NULL, 
                                 geom_default = ggplot2::GeomSf, 
                                 required_aes, 
                                 default_aes = ggplot2::aes(),
                                 stamp = FALSE,
                                 keep_default = NULL,
                                 drop_default = NULL,
                                 id_col_name = NULL, # for keep drop
                                 mapping = NULL,
                                 data = NULL,
                                 position = "identity",
                                 na.rm = FALSE,
                                 show.legend = NA,
                                 inherit.aes = TRUE, 
                                 crs = sf::st_crs(ref_df),
                                 ...){

   
  
ref_df_w_bb_and_xy_centers <- 
  ref_df |>
  dplyr::mutate(bb =
                  purrr::map(geometry,
                             return_st_bbox_df)) |>
  tidyr::unnest(bb) |>
  data.frame() |>
  add_xy_coords()

  if(is.null(id_col_name)){id_col_name <- 1}
  ref_df_w_bb_and_xy_centers$id_col <- ref_df[,id_col_name]

compute_panel_geo <- function(data, scales, keep_id = keep_default, drop_id = drop_default){
  
  if(!is.null(keep_id)){ data <- filter(data, id_col %in% keep_id) }
  if(!is.null(drop_id)){ data <- filter(data, !(id_col %in% drop_id)) }
  
  if(!stamp){data <- dplyr::inner_join(data, ref_df_w_bb_and_xy_centers)}
  if( stamp){data <- ref_df_w_bb_and_xy_centers }
  
  data
  
}


StatTempsf <- ggplot2::ggproto(`_class` = "StatTempsf",
                                `_inherit` = ggplot2::Stat,
                                required_aes = required_aes,
                                compute_panel = compute_panel_geo,
                               default_aes = default_aes)

  if(is.null(geom)){geom <- geom_default}

 c(ggplot2::layer_sf(
              stat = StatTempsf,  # proto object from step 2
              geom = geom,  # inherit other behavior
              data = data,
              mapping = mapping,
              position = position,
              show.legend = show.legend,
              inherit.aes = inherit.aes,
              params = rlang::list2(na.rm = na.rm, ...)
              ),
              
              coord_sf(crs = crs,
                       default_crs = sf::st_crs(crs),
                       datum = crs,
                       default = TRUE)
     )
}
