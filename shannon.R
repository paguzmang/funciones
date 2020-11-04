shannon <- function(x){
  # Version: 1.0 | Autor: paguzmang
  # Descripcion:
  # Esta funcion calcula el indice de diversidad de shannon desde un
  # vector de abundancias por especie.
  
  # Argumentos:
  # x = vector numerico de abundancias por especie. Pueden haber abundancias
  #     de cero.
  
  # Codigo:
  x <- x[x>0]       # se excluyen abundancias de cero
  n <-  sum(x)      # nro. total (n) de individuos en la muestra
  pi <- x / n       # se calculan las proporciones de cada especie
  -sum(pi*log(pi))  # se obtiene el indice de shannon (H') y se imprime
  
  # Valor:
  # Esta funcion entrega un solo numero correspondiente al indice de shannon
  
  # Ejemplo de uso:
  # ni <- c(20, 50, 12, 3)
  # shannon(x = ni)
}
