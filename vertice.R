vertice <- function(a,b,c){
  
  # paguzmang
  # 20-oct-202
  
  # Descripcion:
  # Calcula el vertice de una parabola.
  
  # Argumentos:
  # a, b, c = valores en la funcion de la parabola:
  #     a*x^2 + b*x + c
  #
  # Codigo R:
  f <- function(x,a,b,c) a*x^2 + b*x + c
  x = -b/(2*a)
  y = f(x, a = a, b = b, c = c)
  list(x = x, y = y)
  
  # Valor:
  # La funcion entrega una lista con las coordenadas x, y
  # del vertice
}