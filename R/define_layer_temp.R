compute_group_default <-  function (self, data, scales) {
    cli::cli_abort("Not implemented.")
}


compute_panel_default <- function (self, data, scales, ...) {
    if (empty(data)) 
        return(ggplot2:::data_frame0())
    groups <- split(data, data$group)
    stats <- lapply(groups, function(group) {
        self$compute_group(data = group, scales = scales, ...)
    })
    non_constant_columns <- character(0)
    stats <- mapply(function(new, old) {
        if (empty(new)) 
            return(ggplot2:::data_frame0())
        old <- old[, !(names(old) %in% names(new)), drop = FALSE]
        non_constant <- vapply(old, vec_unique_count, integer(1)) > 
            1L
        non_constant_columns <<- c(non_constant_columns, names(old)[non_constant])
        vec_cbind(new, old[rep(1, nrow(new)), , drop = FALSE])
    }, stats, groups, SIMPLIFY = FALSE)
    non_constant_columns <- ggplot2:::unique0(non_constant_columns)
    dropped <- non_constant_columns[!non_constant_columns %in% 
        self$dropped_aes]
    if (length(dropped) > 0) {
        cli::cli_warn(c("The following aesthetics were dropped during statistical transformation: {.field {dropped}}.", 
            i = "This can happen when ggplot fails to infer the correct grouping structure in the data.", 
            i = "Did you forget to specify a {.code group} aesthetic or to convert a numerical variable into a factor?"))
    }
    data_new <- vec_rbind0(!!!stats)
    data_new[, !names(data_new) %in% non_constant_columns, drop = FALSE]
}




compute_layer_default <- function (self, data, params, layout) {
    ggplot2:::check_required_aesthetics(self$required_aes, c(names(data), 
        names(params)), ggplot2:::snake_class(self))
    required_aes <- intersect(names(data), unlist(strsplit(self$required_aes, 
        "|", fixed = TRUE)))
    data <- remove_missing(data, params$na.rm, c(required_aes, 
        self$non_missing_aes), ggplot2:::snake_class(self), finite = TRUE)
    params <- params[intersect(names(params), self$parameters())]
    args <- c(list(data = quote(data), scales = quote(scales)), 
        params)
    ggplot2:::dapply(data, "PANEL", function(data) {
        scales <- layout$get_scales(data$PANEL[1])
          rlang::try_fetch(rlang::inject(self$compute_panel(data = data, scales = scales, 
            !!!params)), error = function(cnd) {
            cli::cli_warn("Computation failed in {.fn {ggplot2:::snake_class(self)}}.", 
                parent = cnd)
            ggplot2:::data_frame0()
        })
    })
}


define_layer_temp <- function(
  # finish_layer = 
  # retransform
  # extra_params =
  # setup_params
  # parameters
  default_aes = ggplot2::aes(),
  required_aes = character(),
  dropped_aes = character(), 
  optional_aes = character(),
  non_missing_aes = character(),
  compute_group = compute_group_default,
  compute_panel = compute_panel_default, 
  compute_layer = compute_layer_default,
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
  compute_group = compute_group,
  compute_panel = compute_panel,
  required_aes = required_aes)

  if(is.null(geom)){geom <- geom_default}

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
