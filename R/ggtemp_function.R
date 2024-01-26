define_temp_geom_compute_panel <- function(
  required_aes,
  compute_panel = NULL, 
  compute_group = NULL,
  geom = ggplot2::GeomPoint, 
  mapping = NULL,
  data = NULL,
  position = "identity",
  na.rm = FALSE,
  show.legend = NA,
  inherit.aes = TRUE, 
  ...) {

  if(!is.null(compute_panel)){
StatTemp <- ggproto(
  `_class` = "StatTemp",
  `_inherit` = ggplot2::Stat,
  compute_panel = compute_panel,
  required_aes = required_aes)
  }
  
  if(!is.null(compute_group)){
StatTemp <- ggproto(
  `_class` = "StatTemp",
  `_inherit` = ggplot2::Stat,
  compute_group = compute_group,
  required_aes = required_aes)
  }  

  ggplot2::layer(
    stat = StatTemp,  # proto object from Step 2
    geom = geom,  # inherit other behavior
    data = data,
    mapping = mapping,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, ...)
  )
}
