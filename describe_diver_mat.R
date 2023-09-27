describe_diver_mat <- function(x, species_in_rows = FALSE, 
                               pool_units = "all", show_total = TRUE){
  
  # paguzmang
  # 27-sep-2023
  
  # Funcion que hace lo mismo que describe_diver pero desde una matriz de 
  # abundancias de especies por unidades de muestreo. Permite 'pool' 
  # todas unidades, o no, o 'pool' por grupos de unidades.
  
  # Argumentos:
  # x = matriz o data.frame de abundancias de especies por unidades de muestreo
  # species_in_rows = Si TRUE, es porque x tiene las especies en las filas y las
  #     unidades de muestreo en las filas
  # pool_units = Puede ser "all" para realizar un pool de todas las unidades de muestreo
  #     y generar un solo resumen. Si NULL, genera un resumen por cada unidad de
  #     muestreo; y puede ser un factor de longitud igual a nrow(x) o ncol(x),
  #     dependiendo de si las unidades de muestreo estan las filas o columnas, para hacer
  #     un pool por grupos de unidades.
  # show_total = Logico, para mostrar o no el total en caso de que pool_units no sea
  #     'all'. Por defecto se muestra un resumen para el total de las unidades.
  
  
  # Funcion 1 ----
  # Funcion para calcular resumen de diversidad para una sola unidad de muestreo
  diver <- function(x){
    
    # Calculos:
    x <- if(class(x) == 'data.frame') unlist(x) else x
    x <- x[x > 0]          # solo abundancias mayores que cero
    s <- length(x)         # riqueza
    N <- sum(x)            # abundancia total
    p <- x / N             # proporcion por especie
    H <-  -sum(p*log(p))   # indice de Shannon
    q1 <- exp(H)           # numero efectivo de especies
    J <- log(s)            # Diversidad de Shannon maxima posible
    unif_pielou <- H / J   # Uniformidad de Pielou (proporcion)
    f1 <- sum(x == 1)      # frec. absoluta de 'singletons' (especies con frec=1)
    f2 <- sum(x == 2)      # frec. absoluta de 'doubletons' (especies con frec=2)
    
    # Impresion
    data.frame(
      n_units = 1, n_ind = N, Riq = s, f1, f2, H, q1, unif_pielou
    )
  }
  
  # Funcion 2 ----
  add_g_to_df_list <- function(x, g, res.df = F){
    
    # Funcion que recibe una lista de data.frame's y otro
    # data.frame de grupos y le adiciona a cada data.frame
    # dentro de la lista, las columnas correspondientes a los 
    # grupos. Pero ademas, puede juntar los data.frame en uno solo
    
    # x = lista de data.frames
    # g = data.frame de grupos. Observe que los grupos se pueden formar
    #     por una sola variable o una combinacion de niveles o categorias
    #     de dos o mas variables
    # res.df = ¿El resultado se quiere como un data.frame?
    
    nx <- length(x)
    ng <- nrow(g)
    ngc <- ncol(g)
    if(nx != ng) stop("Length of list components is diferent of number of groups")
    xr <- x
    for(i in 1:ng){
      dg <- as.data.frame(
        matrix(data = NA, ncol = ngc, nrow = nrow(x[[i]]) )
      )
      names(dg) <- names(g)
      for(j in 1:ngc) dg[, j] <- g[i,j]
      xr[[i]] <- cbind(dg, x[[i]])
    }
    
    # Impresion dependiendo de res.df
    if(res.df){
      xr2 <- xr[[1]]
      for(i in 2:ng){
        xr2 <- rbind(xr2, xr[[i]])
      }
      xr2
    } else{
      xr 
    }
  }
  
  # Codigo
  # Se calculan la cantidad de unidades de muestreo:
  n_units <- ifelse(species_in_rows, ncol(x), nrow(x))
  
  # Transformacion de la tabla para las unidades de muestreo
  # siempre queden en las filas y las especies en columnas
  if(species_in_rows){
    x <- as.data.frame(t(x))
  } else{
    x <- as.data.frame(x)
  }
  
  # Se valida el argumento pool_units para definir cursos de accion:
  pool1 <- if(length(pool_units) == 1) pool_units == "all" else FALSE
  pool2 <- is.null(pool_units)
  pool3 <- is.factor(pool_units) & length(pool_units) == n_units
  poolt <- c(pool1, pool2, pool3)
  
  # Solo uno de los tres anteriores pool debe dar TRUE
  if( sum( poolt ) != 1) stop('Más de una condición se cumple o ninguna se cumpla')
  
  # Cual de tres caminos se tomara:
  poolr <- which(poolt)
  
  # Se desarrollan los calculos para cada camino:
  switch(
    EXPR = poolr,
    
    # si pool1: se juntan todas las unidades
    {
      r <- diver(x = colSums(x))
      r$n_units <- n_units
    },
    
    # si pool2: se aplica el resumen a cada diversidad sin pool nada
    {
      r_all <- diver(x = colSums(x) ) 
      r_all$n_units <- n_units
      r1 <- apply(X = x, MARGIN = 1, FUN = diver) 
      r1 <- add_g_to_df_list(x = r1, g = data.frame(unit = 1:n_units), res.df = T)
      r <- rbind(r1, cbind(unit = "Total", r_all) )
    },
    
    # si pool3: se aplica resumen por grupos de unidades de pendiendo de un factor
    {
      r_all <- diver(x = colSums(x) ) 
      r_all$n_units <- n_units
      xs <- split(x, f = pool_units)
      r1_all <- sapply(xs, FUN = function(x) diver(x = colSums(x)), simplify = F)
      r1 <- add_g_to_df_list(x = r1_all, g = data.frame(group = levels(pool_units)), res.df = T)
      r1$n_units <- as.numeric(table(pool_units))
      r <- rbind(r1, cbind(group = "Total", r_all) )
    }
    
  )  # fin del switch
  
  # Se imprime el resultado dependiendo de show.total y pool_units
  if(pool1){
    r
  } else{
    if(show_total){
      r
    } else{
      r[-nrow(r), ]
    }
  }

  # Fin de la funcion 
}
