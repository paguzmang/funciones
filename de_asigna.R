de_asigna <- function(trat = 1:3, ue = 1:12,  b = NULL) {
  # Funcion que asigna un conjunto de unidades experimentales
  # a tratamientos de acuerdo a un DCA (balanceado) (cuando b = NULL) 
  # o un DBCA (cuando b es un vector o una matriz, ver abajo).
  # Cada que se ejecuta la funcion, produce una asignacion diferente.
  
  # Argumentos:
  # trat = vector con etiquetas de tratamientos
  # ue   = vector con etiquetas de las unidades exp. para un
  #        DCA.
  # b    = Si NULL (por defecto), se genera la asignacion
  #        bajo un DCA. De lo contrario, puede ser un 
  #        vector o una matriz (ver detalles). Y en cuyo caso, 
  #        se genera la asignacion bajo un DBCA y se omite cualquier
  #        valor dado en el argumento 'ue'.
  
  # Detalles:
  #        Si 'b' es un vector, debe tener las etiquetas de los 
  #        bloques. Se generara una matriz donde se aleatoriza
  #        la posicion de cada tratamiento en cada bloque. 
  #        Si 'b' es una matriz, debe estar armada de
  #        tal forma que las columnas representan bloques
  #        y las filas unidades experimentales. El nro. de filas
  #        debe ser igual al nro. de tratamientos.
  #        En este caso se aleatorizan las unidades exp. dentro de
  #        cada columna (bloque), y luego se etiquetan las filas
  #        usando el nombre de los tratamientos.
  #        Esta matriz 'b' puede o no tener nombres de columna 
  #        para etiquetar los bloques.
  #        La opcion de dar una matriz a 'b' se utiliza cuando
  #        el usuario previamente ya agrupo las unidades exp.
  #        en bloques. La opción de dar un vector a 'b'
  #        es cuando aun no se han conformado los bloques
  #        pero se quiere ver 'espacialmente' como quedarian
  #        los tratamientos en cada bloque. 
  
  # Codigo:
  k <- length(trat)  # nro. de tratamientos
  N <- length(ue)    # nro. de unidades exp.
  n <- N / k         # nro. de replicas por trat. (balanceado)
  if(is.null(b)){
    if(!trunc(n) == n) stop('N/k no es un entero')
    X <- matrix(data = sample(ue), ncol = k,
                dimnames = list(replica = 1:n, Trat = trat))
  } else {
    if(is.vector(b)){
      nro.b <- length(b)  # nro. de bloques
      X <- replicate(nro.b, sample(trat))
      colnames(X) <- paste('Bloq', b, sep = '.')
    } else{
      nro.b   <- ncol(b)
      nom.col <- colnames(b)
      if(nrow(b) != k) stop('Nro. de filas de b no es igual al nro. de trat.')
      X <- apply(b, 2, function(x) sample(x))
      dimnames(X) <- list(
        Trat = trat, 
        Bloques =  if(is.null(nom.col)){
          paste('Bloq', 1:nro.b, sep = '.')
        } else nom.col
      )
      }
    }
  
  # Entrega el resultado
  X
}
