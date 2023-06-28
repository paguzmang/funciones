rad <- function(x, decreasing = T, n_ind = 100, nom_grupo = 'grupo'){
  
  # Abril 2023
  # paguzmang
  # En el contexto de distribucion de abundancia de especies en Ecologia de comunidades, 
  # esta funcion realiza un RAD = Rank Abundance Distribucion. Entrega los datos para 
  # hacer un grafico. Si hay grupos, entrega los datos organizados para ser graficados
  # con ggplot2.
  
  # Argumentos: ----
  # x = Abundancias por especie. Se permiten abundancias de cero. 
  #     Puede ser uno de los siguientes objetos:
  #        - Vector nombrado de abundancias por especie.
  #        - Matriz de dos o mas columnas. Filas = especies, Columnas  = Sitios.
  #        - Lista de dos o vectores. Cada vector es un vector de abundancias por especie
  #            para un sitio.
  # decreasing = Logico. Si TRUE, las especies se ordenan desde la mas
  #     abundante a la menos abundante. Si FALSE, se orden al contrario.
  # n_ind = cantidad de individuos para multiplicar por la frec. relativa
  #     Por defecto 100 para expresar la frec. relativa en porcentaje.
  #     Sin embargo, si la abundancia total es mayor a 1000, puede ser conveniente
  #     multiplicar por 1000 y no por 100 para expresar por cada 1000 individuos.
  # nom_grupo = Cadena de texto indicando el nombre que se quiere para la columna de grupos
  #    o sitios para el caso en el cual x sea una matriz de dos o mas columnas o una lista
  #    con dos o mas vectores.
  
  # Valor -----
  # En cualquier caso, la funcion entrega un data.frame con el rango (k) y la frecuencia
  # de cada especie, en orden descendente o ascendente de acuerdo al argumento 'decreasing'
  
  # Codigo -----
  # Validacion del argumento x ----
  v  <- is.vector(x)
  m1 <- is.matrix(x)
  if(m1) m1 <- ncol(x) == 1
  m2 <- is.matrix(x)
  if(m2) m2 <- ncol(x) >= 2
  l1 <- is.list(x) 
  if(l1) l1 <- length(x) == 1
  l2 <- is.list(x) 
  if(l2) l2 <- length(x) >= 2
  
  # Funciones internas:
  # Fun 1 ----
  rad_v_to_df <- function(x, decreasing = T, n_ind = 100){
    
    # 26-mar-2022
    # paguzmang
    
    # Genera info del rank por abundancia en orden de mayor abundancia
    # a menor abuandancia. Desde un vector, entrega un data.frame
    
    # Argumentos:
    # x = vector nombrado de abundancias por especie.
    #     Pueden haber abundancias de 0
    # decreasing = Logico. Si TRUE, las especies se ordenan desde la mas
    #     abundante a la menos abundante.
    # n_ind = cantidad de individuos para multiplicar por la frec. relativa
    #     Por defecto 100 para expresar la frec. relativa en porcentaje.
    #     Sin embargo, si sum(x) > 1000, puede ser conveniente
    #     multiplicar por 1000 no por 100 para expresar por cada 1000
    #     individuos.

    x <- sort(x[x > 0], decreasing = decreasing)
    k <- 1:length(x)
    n <- sum(x)
    sp <- names(x)
    names(x) <- NULL
    data.frame(sp, k, f = x, fr = x / n * n_ind)
    
  } # Fin de Fun 1
  
  # Fun 2 ----
  add_g_to_df_list <- function(x, g, res.df = F){
    
    # Funcion que recibe una lista de data.frame's y otro
    # data.frame de grupos y le adiciona a cada data.frame
    # dentro de la lista, las columnas correspondientes a los 
    # grupos. Pero ademas, puede juntar los data.frame en uno sol
    
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
  }   # Fin de Fun 2
  
  # Calculos ---- 
  if(v){
    if(is.null(names(x))) names(x) <- paste0('sp', 1:length(x))
    res <- rad_v_to_df(x, decreasing = decreasing, n_ind = n_ind)
  }
  
  if(m1){
    x <- x[, 1]
    if(is.null(names(x))) names(x) <- paste0('sp', 1:length(x))
    res <- rad_v_to_df(x, decreasing = decreasing, n_ind = n_ind)
  }
  
  if(l1){
    x <- x[[1]]
    if(is.null(names(x))) names(x) <- paste0('sp', 1:length(x))
    res <- rad_v_to_df(x, decreasing = decreasing, n_ind = n_ind)
  }
  
  if(m2){
    res0 <- apply(x, 2, rad_v_to_df, decreasing = decreasing, n_ind = n_ind)
    dfg  <- data.frame(g = colnames(x))
    names(dfg) <- nom_grupo
    res  <- add_g_to_df_list(x = res0, g = dfg, res.df = T)
  }
  
  if(l2){
    res0 <- sapply(x, rad_v_to_df, decreasing = decreasing, n_ind = n_ind)
    dfg  <- data.frame(g = names(x))
    names(dfg) <- nom_grupo
    res  <- add_g_to_df_list(x = res0, g = dfg, res.df = T)
  }
  
  # Se imprime el resultado -----
  res
  
}
