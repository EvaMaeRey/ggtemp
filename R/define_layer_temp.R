define_layer_temp <- function(
  default_aes = ggplot2::aes(),
  required_aes = character(),
  dropped_aes = character(), 
  optional_aes = character(),
  non_missing_aes = character(),
  compute_group = NULL,
  compute_panel = NULL, 
  compute_layer = NULL,
  # finish_layer = # we'll work on making these stat ggproto slots accessible too
  # retransform
  # extra_params =
  # setup_params
  # parameters
  geom = NULL,
  geom_default = ggplot2::GeomPoint, 
  mapping = NULL,
  data = NULL,
  position = "identity",
  na.rm = FALSE,
  show.legend = NA,
  inherit.aes = TRUE, 
  
  ...) {

StatTemp <- ggproto(
  `_class` = "StatTemp",
  `_inherit` = ggplot2::Stat,
  default_aes = default_aes,
  required_aes = required_aes)

if(!is.null(compute_group)){StatTemp$compute_group <- compute_group}
if(!is.null(compute_panel)){StatTemp$compute_panel <- compute_panel}
if(!is.null(compute_layer)){StatTemp$compute_layer <- compute_layer}

  if(is.null(geom)){geom <- geom_default}

  ggplot2::layer(
    stat = StatTemp, 
    geom = geom, 
    data = data,
    mapping = mapping,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, ...)
  )
}
