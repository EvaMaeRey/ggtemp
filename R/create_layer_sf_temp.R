create_layer_sf_temp <- function(ref_df, 
                                 fun_name ="geom_my_sf", 
                                 required_aes, 
                                 default_aes = ggplot2::aes(),
                                 geom_default = ggplot2::GeomSf,
                                 keep_default = NULL,
                                 drop_default = NULL,
                                 ...){

  assign(x = fun_name, 
         value = function(...){
           
           
  define_layer_sf_temp(ref_df = ref_df,
    required_aes = required_aes,
    geom_default = geom_default,
    default_aes = default_aes,
    keep = keep_default, 
    drop = drop_default,
    ...)  },
  pos = 1
  )
  
}
