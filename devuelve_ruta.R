devuelve_ruta <- function(x) {
  
  # paguzmang - mayo 2020
  # Recibe una ruta y entrega la ruta inmeditamente anterior
  # Sirve para traer datos que estan en una carpeta anterior
  # al directorio de trabajo. Evita escribir rutas absolutas.
  # Use su resultado en combinacion con paste0.
  
  # x = ruta. Tipicamente el directorio de trabajo actual 
  #           producido por getwd()
  
  require(stringr)
  loc <- str_locate_all(x, '/')[[1]]  
  loc <- loc[nrow(loc), 1]
  n <- nchar(x)
  str_sub(x, 1, loc-1 )
}
