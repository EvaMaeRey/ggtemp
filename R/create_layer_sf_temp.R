create_layer_sf_temp <- function(ref_df, 
                                 fun_name ="geom_my_sf", 
                                 required_aes, 
                                 geom_default = ggplot2::GeomSf,
                                 ...){

  assign(x = fun_name, 
         value = function(...){
           
           
  define_layer_sf_temp(ref_df = ref_df,
    required_aes = required_aes,
    geom_default = geom_default,
    ...)  },
  pos = 1
  )
  
}
