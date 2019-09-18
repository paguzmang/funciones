area_xy <- function(x0, x1, fun = function(x) x^2, a = 0.01, ...){
  # Creacion: 5-ago-2018
  # Actualizada: 17-sep-2019
  # Esta funcion (generica) entrega una lista con valores (x,y) para usarse
  # dentro de la funcion polygon para agregar sombras de areas bajo la curva
  # de una funcion matematica conocida en un intervalo x0 < x < x1.
  #
  # Argumentos:
  # x0 = numero que indica el limite inferior de x
  # x1 = numero que indica el limite superior de x para el area deseada
  # a = numero que indica el tamano de paso para generar los valores de x entre x0 y x1.
  # fun = comando que indica el nombre alguna funcion matematica. La funcion puede ser una
  #       definida por el usuario u cualquier otra ya definida en el R. Por defecto
  #       esta la funcion de una parabola.
  # ... argumentos adicionales para ser pasados a fun (p.e,  los parametros de la funcion)
  #
  rx <- seq(x0, x1, a)    # rango de valores de x solicitado
  d <- fun(x = rx, ...)   # densidad evaluada en rx
  x <- c(x0, rx, x1)      # ajuste de rango de x
  y <- c(0, d, 0)         # ajuste de valores de y
  list(x = x,y = y)       # entrega de resultados: (x, y) para polygon
  #
  # Forma de uso:
  # Con una funcion cualquiera definida por el usuario:
  # f <- function(x) exp(-5*x)  # se define la funcion
  # rp <- area_xy(x0 = 0.1, x1 = 0.6, fun = f)  # se aplica area_xy
  # curve(f, from = 0, to = 1.5, lwd = 2, col = 'blue')
  # polygon(rp, border = NA, col = 'grey70')
}
