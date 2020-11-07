dist_to_df <- function(x) {
  
  # Descripcion:
  # Funcion que transforma una matriz de distancia (generada con
  # el comando "dist") en un data.frame. Tomado del enlace:
  # https://stackoverflow.com/questions/23474729/convert-object-of-class-dist-into-data-frame-in-r
  # Consultada el: 7-nov-2020
  
  # Argumentos:
  # x = objeto de de clase "dist"
  
  # Codigo:
  if (class(x) != "dist") stop("wrong input type")
  A <- attr(x, "Size")
  B <- if (is.null(attr(x, "Labels"))) sequence(A) else attr(x, "Labels")
  if (isTRUE(attr(x, "Diag"))) attr(x, "Diag") <- FALSE
  if (isTRUE(attr(x, "Upper"))) attr(x, "Upper") <- FALSE
  data.frame(
    row = B[unlist(lapply(sequence(A)[-1], function(x) x:A))],
    col = rep(B[-length(B)], (length(B)-1):1),
    dist = as.vector(x))
}
