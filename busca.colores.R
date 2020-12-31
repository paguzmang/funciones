busca.colores <- function(col = 'blue'){
  
  # Ago 2020
  # paguzmang
  # Genera un grafico con los colores del vector
  # colors() que coincidan con la palabra dada
  # en el argumento "col". Entrega el nombre
  # del color y la posicion.
  
  x.num <- grep(col, x = colors())
  x.txt <- colors()[x.num]
  n <- length(x.num)
  y <- rep(1, n)
  op <- par(mar = c(0.1,12,0.1,3), mgp = c(2,0.5,0), 
            cex = 0.8, las = 1)
  barplot(y, names.arg = paste(x.txt, x.num, sep = " - "), 
          horiz = T, xaxt = 'n', xlab = NA,
          col = x.txt)
  par(op)
}