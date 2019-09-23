cuenta_nan <- function(x, dec = 2, pct = F){
  # Version: 3.0 | fecha: sep 2019 | Autor: paguzmang
  # Descripcion:
  # Esta funcion cuenta el nro. de NaN en un vector. Entrega tanto
  # el nro. de NaN's como su proporcion.
  
  # Argumentos:
  # x = vector que se desea evaluar.
  # dec = nro. de decimales para reportar la proporcion de NaN.
  #       Por defecto 2 decimales.
  # pct = Logico. Indica si la proporcion de NaN's se reportara como
  #       fraccion entre 0 y 1 (FALSE, por defecto) o 
  #       como porcentaje (entre 0 y 100) (TRUE).
  
  # Codigo:
  donde.nan <- is.nan(x)     # se crea vector logico indicando donde hay NaN
  n_nan <- sum( donde.nan )  # al sumar el vector logico, se cuenta los NaN
  p_nan <- round(n_nan/length(x), dec)   # se calcula la proporcion
  p_nan <- ifelse(pct, p_nan*100, p_nan) # Porcentaje o fraccion?
  data.frame(n = n_nan, p = p_nan)       # se entrgan las dos cantidades en un data.frame
  
  # Valor:
  # la funcion entrega un data.frame con una fila y dos columnas
  # que incluyen el nro. de NaN's y la proporcion NaN's (o porcentaje)
  
  # Ejemplo de uso:
  # datos <- c(10, 23, NaN, -2, 4, NaN)
  # cuenta_nan(x = datos, pct = T)
}