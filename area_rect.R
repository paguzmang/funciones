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