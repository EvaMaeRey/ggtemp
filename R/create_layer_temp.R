create_layer_temp <- function(fun_name ="geom_circle", 
                                    compute_panel = NULL,
                                    compute_group = NULL,
                                    required_aes = c("x0", "y0", "r"),
                                    geom = "point"){

  assign(x = fun_name, 
         value = function(...){
           
  define_layer_temp(
    required_aes = required_aes,
    compute_panel = compute_panel,
    compute_group = compute_group,
    geom = geom,
    ...)  },
  pos = 1
  )
  
}
