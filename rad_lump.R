rad_lump <- function(x, s_keep = NULL, decreasing = TRUE, 
                     name_others = "Others", n_ind = 100){
  
  # Oct 2024
  # paguzmang

  # DESCRIPCION:
  # En el contexto de distribucion de abundancia de especies en Ecologia de comunidades, 
  # esta funcion realiza un RAD = Rank Abundance Distribucion. Entrega los datos para 
  # hacer un grafico. Trabaja para una sola muestra de individuos. Los datos debe estar
  # en un vector numerico (abundancias por especie) o en un vector de texto o factor (
  # especies para cada individuo). Opcionalmente, permite agrupar las especies de menor
  # frecuencia en una sola categoria al final. Esto es util si se quiere mostrar en un grafico
  # solo las especies mas abundantes debido a que sean muchas y, si se graficaran todas, 
  # ocuparian mucho espacio en un grafico. Esta ultima opcion fue inspirada en el comando
  # forcats::fct_lump que hace algo similar para un factor o vector de texto.
  
  # ARGUMENTOS:
  # x = Datos. Debe ser uno de dos tipos:
  #
  #       - Vector numerico de abundancias por especie
  #         ojala, este nombrado con el nombre o abreviacion de cada especie. La suma
  #         de este vector debe dar la abundancia total (sum(x)) y su 
  #         longitud la riqueza (length(x))
  #
  #       - Un vector de texto o factor indicando el nombre de la especie para cada 
  #         individuo en la muestra. La longitud de este vector sera la abundancia total.
  #         La longitud del vector de valores unicos sera la riqueza = length(unique(x))
  #
  # s_keep = Argumento para indicar el número de especies que se deben de mantera de forma
  #    individual en la tabla resultante antes de agrupar en una categoria de "otros". Es decir,
  #    la tabla mostraria las s_keep especies mas abundantes de primero. Las restantes se agruparian
  #    en una sola categoria llamda "otros". Tres opciones para este argumento:
  #
  #    - Numero entero que indica la cantidad de especies a mantener antes de agrupar. Si este numero
  #        es mayor o igual al numero total de especies, no se agrupa.
  #    - NULL, para no agrupar. Se muestran todas las especies en la tabla resultante.
  #    - "auto": Se busca un numero de manera automatica de tal forma que la abundancia de la 
  #     especie mas abundante sea mayor que la abundancia que queda para la categoria "otros".
  #
  # decreasing = Si TRUE, el ordenamiento se realiza descendente por la frecuencia.
  #
  # name_others = Nombre para la categoria en la cual se agruparan el resto de especies 
  #   de acuerdo al argumento 's_keep'.
  #
  # n_ind = Entero dando el numero de individuos por el cual multiplicar la frecuencia relativa
  #   en la tabla final. Por defecto 100 para que quede en porcentaje. Si la abundancia total es
  #   muy alta y existen especies muy raras, puede ser conveniente multiplicar por 1000 o un numero
  #   mayor para facilitar la lectura de esta frecuencia.
  
  # Calculos
  # Se validan el tipo de dato en 'x':
  if(class(x) %in% c("character", "factor") ){
    x <- table(x)
    class(x) <- NULL
  }
  if(is.null(names(x))) names(x) <- paste0('sp', 1:length(x))  # si no hay nombres que lo asigne
  x <- sort(x[x > 0], decreasing = decreasing)
  k <- 1:length(x)
  s <- length(x)    # riqueza
  n <- sum(x)       # abundancia total
  sp <- names(x)    # nombres de especies ya ordenadas
  names(x) <- NULL
  
  # Se organiza un data.frame con todas las especies:
  DF <- data.frame(sp, k, f = x, fr = x / n * n_ind)
  DF$f_acum <- cumsum(DF$f)
  DF$fr_acum <- cumsum(DF$fr)
  
  # En caso de que s_keep sea nulo o s_keep mayor o igual a s
  imprima_DF <- ifelse(is.null(s_keep), T, is.numeric(s_keep) & (s_keep >= s))
  if(imprima_DF){
    DF$sp <- factor(DF$sp, levels = sp)
    DF   # se imprime
  } else{
    # Busqueda autmoatica de s_keep
    u <-  s_keep %in% c("auto", "Auto", "A", "a", "Au", "au", "aut", "Aut")
    if(u){
      s_keep <- 1  # se define valor inicial para s_keep
      while(u){
        s_keep <- s_keep + 1     # se actualiza s_keep
        DF1 <- DF[1:s_keep, ]
        DF2 <- DF[(s_keep+1):s, ]
        DF3 <- data.frame(
          sp = name_others,
          k = paste(DF2[1, "k"], DF2[nrow(DF2), "k"], sep = "-"),
          f = sum(DF2$f),
          fr = sum(DF2$fr),
          f_acum = sum(DF$f),
          fr_acum = sum(DF$fr)
        )
      DFr <- rbind(DF1, DF3)
      DFr$sp <- factor(DFr$sp, levels = DFr$sp)
      u <- DFr$fr[nrow(DFr)] > DF$fr[1]   # se actualiza el valor de u
      }
      DFr  # se imprime
    } else{
      # s_keep debe ser numerico y menor que s desde su definicion como argumento
      DF1 <- DF[1:s_keep, ]
      DF2 <- DF[(s_keep+1):s, ]
      DF3 <- data.frame(
        sp = name_others,
        k = paste(DF2[1, "k"], DF2[nrow(DF2), "k"], sep = "-"),
        f = sum(DF2$f),
        fr = sum(DF2$fr),
        f_acum = sum(DF$f),
        fr_acum = sum(DF$fr)
      )
      DFr <- rbind(DF1, DF3)
      u <- DFr$fr[nrow(DFr)] < DF$f[1]
      DFr$sp <- factor(DFr$sp, levels = DFr$sp)
      DFr  # se imprime
    }
  }
  
}
