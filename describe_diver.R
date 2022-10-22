describe_diver <- function(x, res.texto = F){
  
  # Oct 2022
  # paguzmang
  
  # Descripcion:
  # Genera una descripcion de la diversidad desde un vector
  # de abundancias, calculando la abundancia total, la riqueza
  # la frec. de 'singletons' y 'doubletons', el indice de 
  # Shannon, la exponencial de Shannon y el indice de uniformidad
  # de Pielou
  
  # Argumentos:
  # x = vector de abundancias, se permiten abundancias de cero
  # res.texto = si TRUE, se entrega una cadena de texto con los
  #   indices descriptores separados por ";". Esto puede ser util
  #   para montar en un grafico. Si FALSE, se entrega un data.frame
  #   de un sola fila y cada descriptor en las columnas.
  
  # Calculos:
  x <- x[x > 0]          # solo abundancias mayores que cero
  s <- length(x)         # riqueza
  N <- sum(x)            # abundancia total
  p <- x / N             # proporcion por especie
  H <-  -sum(p*log(p))   # indice de Shannon
  q1 <- exp(H)           # numero efectivo de especies
  J <- log(s)            # Diversidad de Shannon maxima posible
  H1 <- H / J            # Uniformidad de Pielou (proporcion)
  f1 <- sum(x == 1)      # frec. absoluta de 'singletons'
  f2 <- sum(x == 2)      # frec. absoluta de 'doubletons'
  
  # Impresion
  if(res.texto) {
    paste0(
      "N = ", N, "; Riq = ", s, 
      "; f1 = ", round(f1/s,2),  # frec. relativa de 'singletons'
      "; f2 = ", round(f2/s,2),  # frec. relativa de 'doubletons'
      "\nH = ", round(H,2), 
      "; q1 = ", round(q1,1), "; H1 = ", round(H1, 2)
    )
  } else{
    data.frame(
      N, Riq = s, f1, f2, H, q1, H1
    )
  }
}
