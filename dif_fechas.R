dif_fechas <- function(f1, f2){
  
  # paguzmang
  # 22-ago-2023
  
  # Funcion para calcular diferencia entre dos fechas
  # entregando el resultado en horas, dias y semanas
  
  # f1, f2 = fechas en formato 'aaaa-mm-dd hh:mm'
  #   ej: 2023-08-22 23:30 para el 22 de agosto
  #   del 2023 a las 23 horas y 30 minutos
  #   (11:30 PM)
  # Puede omitir la parte de la 'hh:mm' en f1 y f2
  
  # Codigo:
  f1 <- as.POSIXct(f1)
  f2 <- as.POSIXct(f2)
  d <- as.numeric(abs(f1 - f2))
  data.frame(
   horas =  d*24, dias = d, semanas = d/7
  )
  
  # Ejemplo:
  # dif_fechas(f1 = '2023-08-22 14:00', f2 = '2023-09-05 23:00')
  
}





