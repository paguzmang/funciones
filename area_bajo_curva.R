cal_area_rect <- function(x0, x1, h = 0.5, n = NULL, fun, 
                          lado = c('cen', 'todos') ){
  # Oct 2019
  # DESCRIPCION: Calcula o aproxima el area bajo la curva de una funcion matematica en
  #   un intervalo particular, sumando el area de rectangulos bajo la curva. El calculo
  #   se hace teniendo en cuenta el numero de rectangulos y si estos se definen a derecha
  #   a izquierda o en el centro.   
  
  # ARGUMENTOS:
  # x0, x1 = numericos. Limites inicial y final sobre el cual se quiere calcular el area
  # h = numerico. Amplitud (constante) de los intervalos generados entre x0 y x1
  # n = nro. de intervalos generados entre x0 y x1. Si se asigna un valor, se usa este
  #     argumento en el lugar del argumento h.
  # fun = funcion matematica a evaluar (creada con function o cualquier funcion matematica que ya
  #    tenga el R guardada p.e., dnorm, dexp, dunif, etc.)
  # lado = de texto. Indica si quiere un area con los rectangulos centrados ('cen') o 
  #      si se quiere obtener el area desde las tres formas ('todos'): izquierda, derecha y centrada.
  #
  # CODIGO:  
  # Validacion del argumento lado:
  mensaje <- 
    "El argumento 'lado' debe ser alguno de: 'cen', 'todos'"
  if(!lado[1] %in% c('cen', 'todos')) stop(mensaje) 
  
  # Se crea el vector con la secuencia de numeros a evaluar usando 
  # un condicional anidado:
  if(is.null(n)){
    x <- seq(x0, x1, by = h)
    if(!identical(max(x), x1)) {
      x <- seq(x0, x1, length.out = length(x))
    }
  } else{
    x <- seq(x0, x1, length.out = n+1)
  }
  k <- length(x)      # se guarda la longitud del vector x
  delta <- x[2]-x[1]  # amplitud de cada intervalo (constante)
  
  # Calculo de sumas y entrega del resultado:
  # vector x de puntos en el centro de los rectangulos:
  x.cen <- seq(from = x[1]+delta/2, by = delta, length.out = k-1)
  A.cen <- delta*sum(fun(x.cen))   # Area en rectangulos centrados
  switch(lado[1],
         cen = A.cen,
         todos = data.frame(A.izq = delta*sum(fun(x[1:(k-1)])),
                            A.cen = A.cen,
                            A.der = delta*sum(fun(x[2:k])) 
         )
  )
  
  # VALOR:
  # Si lado = 'cen', entrega un solo numero. El area calculada con rectangulos centrados.
  # Si lado = 'todos', entrega un data.frame con los tres calculos del area.
  
  # EJEMPLOS:
  # cal_area_rect(
  #  x0 = 2, x1 = 4,
  #  h = 0.25, lado = 'todos',
  #  fun = function(x) x^2
  # )
  # Compare con la funcion integrate:
  # integrate(
  #  f = function(x) x^2,
  #  lower = 2, upper = 4
  # )
}

area_xy <- function(x0, x1, fun = function(x) x^2, a = 0.01, ...){
  # Creacion: 5-ago-2018
  # Actualizada: 17-sep-2019
  # DESCRIPCION:
  # Esta funcion (generica) entrega una lista con valores (x,y) para usarse
  # dentro de la funcion polygon para agregar sombras de areas bajo la curva
  # de una funcion matematica conocida en un intervalo x0 < x < x1.
  #
  # ARGUMENTOS:
  # x0 = numero que indica el limite inferior de x
  # x1 = numero que indica el limite superior de x para el area deseada
  # a = numero que indica el tamano de paso para generar los valores de x entre x0 y x1.
  # fun = comando que indica el nombre de alguna funcion matematica. La funcion puede ser una
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
  # VALOR:
  # Entrega una lista con vectores x, y, para ser usanda dentro del 
  # comando polygon.
  #
  # FORMA DE USO:
  # Con una funcion cualquiera definida por el usuario:
  # f <- function(x) exp(-5*x)  # se define la funcion
  # rp <- area_xy(x0 = 0.1, x1 = 0.6, fun = f)  # se aplica area_xy
  # curve(f, from = 0, to = 1.5, lwd = 2, col = 'blue')
  # polygon(rp, border = NA, col = 'grey70')
}

area_rect <- function(x0, x1, h = 0.5, n = NULL, fun, 
                      lado = c('der', 'izq', 'cen') ){
  # Oct 2019
  # DESCRIPCION: Agrega rectangulos al grafico de una funcion matematica
  #   como aproximacion del area bajo la curva. Actua como un comando de
  #   de bajo nivel. Se basa en el comando 'rect'.
  
  # ARGUMENTOS:
  # x0, x1 = numericos. Limites inicial y final sobre el cual se quiere definir el area
  # h = numerico. Amplitud (constante) de los intervalos generados entre x0 y x1
  # n = nro. de intervalos generados entre x0 y x1. Si se asigna un valor, se usa este
  #     argumento en el lugar del argumento h.
  # fun = funcion matematica a evaluar (creada con function o cualquier funcion matematica que ya
  #    tenga el R guardada p.e., dnorm, dexp, dunif, etc.)
  # lado = de texto. Indica si los rectantagulos se dibujaran a la derecha ('der'), 
  #    izquierda ('izq') o en el centro ('cen'). Por defecto, derecha.
  #
  # CODIGO:  
  # Validacion del argumento lado:
  mensaje <- 
    "El argumento 'lado' debe ser alguno de: 'der', 'izq', 'cen'"
  if(!lado[1] %in% c('der', 'izq', 'cen')) stop(mensaje) 
  
  # Se crea el vector con la secuencia de numeros a evaluar usando 
  # un condicional anidado:
  if(is.null(n)){
    x <- seq(x0, x1, by = h)
    if(!identical(max(x), x1)) {
      x <- seq(x0, x1, length.out = length(x))
    }
  } else{
    x <- seq(x0, x1, length.out = n+1)
  }
  k <- length(x)  # se guarda la longitud del vector x
  
  # Ciclo para poner los rectangulos uno a uno despues del otro:
  for(i in 1:(k-1)){
    rect(xleft = x[i], ybottom = 0, 
         xright = x[i+1], 
         ytop = switch(lado[1],  
                       der = fun(x[i+1]),  
                       izq = fun(x[i]), 
                       cen = fun(0.5*(x[i] + x[i+1])))  )
  }
  
  # VALOR:
  # No genera ningun resultado. Solo agrega rectangulos a un grafico ya dibujado.
  
  # EJEMPLOS:
  # f <- function(x) x^2
  # curve(f, from = 0, to = 6)
  # area_rect(x0 = 2, x1 = 4, h = 0.25, fun = f, lado = 'cen')
}