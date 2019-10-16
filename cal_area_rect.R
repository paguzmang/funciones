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