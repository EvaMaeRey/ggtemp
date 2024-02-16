create_layer_temp <- function(fun_name ="geom_circle", 
                              compute_panel = NULL,
                              compute_group = NULL,
                              required_aes = character(),
                              default_aes = aes(),
                              geom_default ="point", ...){

  assign(x = fun_name, 
         value = function(...){
           
  define_layer_temp(
    required_aes = required_aes,
    default_aes = default_aes, 
    compute_panel = compute_panel,
    compute_group = compute_group,
    geom_default = geom_default,
    ...)  },
  pos = 1
  )
  
}
