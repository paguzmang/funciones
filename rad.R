rad <- function(x, s_keep = NULL, max_fr_acum = NULL, decreasing = TRUE, 
                name_others = "Others", n_ind = 100, 
                species_in_columns = TRUE,
                col_name_sample = 'sample',
                collapse_by = NULL, col_name_group = "group",
                as_list = FALSE){
  
  # Oct 2024 / Actualizado julio 2025
  # paguzmang
  
  # DESCRIPCION:
  # En el contexto de distribucion de abundancia de especies en Ecologia de comunidades, 
  # esta funcion realiza un RAD = Rank Abundance Distribucion a partir de una sola muestra
  # o de múltiples muestras o sitios. También permite colapsar, primero, muestras o sitios 
  # de acuerdo a otra variable (un factor) para luego calcular el RAD.
  # Entrega los resultados en un data.frame conveniente para hacer un grafico de RAD desde ggplot2.
  
  # Opcionalmente, permite agrupar las especies de menor
  # frecuencia en una sola categoria al final. Esto es util si se quiere mostrar en un grafico
  # solo las especies mas abundantes debido a que sean muchas y, si se graficaran todas, 
  # ocuparian mucho espacio en un grafico. Esta ultima opcion fue inspirada en el comando
  # forcats::fct_lump que hace algo similar para un factor o vector de texto.
  
  # ARGUMENTOS: ----
  # x = Abundancias por especie. Se permiten abundancias de cero. 
  #     Puede ser uno de los siguientes objetos:
  #        - Vector nombrado de abundancias por especie.
  #        - Matriz de dos o mas columnas. Filas = sitios (o muestras), Columnas  = Especies.
  #        - Lista de dos o mas vectores. Cada vector es uno de abundancias por especie
  #            para un sitio o muestra. La lista debe ser nombrada, es decir, names(x) no puede ser NULL
  # decreasing = Logico. Si TRUE, las especies se ordenan desde la mas
  #     abundante a la menos abundante. Si FALSE, se orden al contrario.
  # n_ind = cantidad de individuos para multiplicar por la frec. relativa
  #     Por defecto 100 para expresar la frec. relativa en porcentaje.
  #     Sin embargo, si la abundancia total es mayor a 1000, puede ser conveniente
  #     multiplicar por 1000 y no por 100 para expresar por cada 1000 individuos y asi
  #     facilitar la lectura de esta columna.
  # col_name_sample = Cadena de texto indicando el nombre que se quiere para la columna de
  #    sitios o muestras, para el caso en el cual x sea una matriz o una lista
  #    con dos o mas vectores.
  # s_keep = Argumento para indicar el número de especies que se deben de manter de forma
  #    individual en la tabla resultante antes de agrupar en una categoria de "Otras". Es decir,
  #    la tabla mostraria las s_keep especies mas abundantes de primero. Las restantes se agruparian
  #    en una sola categoria llamda "Otros". Tres opciones para este argumento:
  #
  #    - NULL, para no agrupar. Se muestran todas las especies en la tabla resultante.
  #    - Numero entero que indica la cantidad de especies a mantener antes de agrupar. Si este numero
  #        es mayor o igual al numero total de especies, no se agrupa.
  #    - "auto": Automático. Aquí las especies más abundantes se seleccionan hasta el punto en el cual
  #      las especies restantes (`Otras`) tengan una frecuencia igual o inferior a la especie más abundante. Esto garantiza que el gráfico del RAD, la barra de la categoría `Otras` sea igual o inferior a la barra de la primera especie (la más abundante).
  #    - "fr_acum":  En este caso, las especies más abundantes serán aquellas que acumulen
  #      al menos cierto porcentaje, el cual debe ser indicado en el argumento 'max_fr_acum'.
  #
  # max_fr_acum = valor numérico entre 0 y n_ind que define el umbral 
  #     máximo de frecuencia relativa acumulada a conservar cuando
  #     s_keep = "fr_acum".
  #
  # name_others = Nombre para la categoria en la cual se agruparan el resto de especies 
  #   de acuerdo al argumento 's_keep'.
  #
  # species_in_columns = Logico. Si TRUE (por defecto) y 'x' es una matriz, es porque las especies están en las columnas
  #   y las muestras (o sitios) están en las filas. Si FALSE, sera lo contrario. 
  #
  # collapse_by = Para el caso en el cual se tienen varias muestras y 'x' es un vector o una matriz, 
  #   este argumento es un vector (tipiciamente un factor) de la misma longitud que el numero de muestras. 
  #   Permite colapsar (totalizando las abundancias por especie) las muestras con un mismo valor del vector
  #   y realizar el calculo del rad para cada grupo definido por este vector. Si NULL, no se realizara
  #   el colapso y se calculara la RAD para el vector, o para cada muestra o sitio dado en la matriz 'x'.
  #
  # col_name_group = Cadena de texto que serviria como titulo de la variable para el factor definido
  #   en 'collapse_by'
  #
  # as_list = Logica, si TRUE, cuando collapse_by no es NULL, se consulta si el resultado es una lista
  #    de data.frame o el resultado sera un solo data.frame con una respectiva columna que diferencia
  #    los grupos.
  
  # Valor -----
  # La funcion entrega, por defecto, un data.frame con el rango (k) y la frecuencia
  # de cada especie, en orden descendente o ascendente de acuerdo al argumento 'decreasing'.
  # Cuando ya varias muestras o grupos, si as_list = TRUE, se entregará una lista de data.frame
  
  # Codigo -----
  # Validacion del argumento x ----
  v  <- is.atomic(x) & (length(dim(x)) == 1 | is.null(dim(x)))
  m1 <- is.matrix(x) | is.data.frame(x)
  if(m1) if(species_in_columns) m1 <- nrow(x) == 1 else m1 <- ncol(x) == 1
  m2 <- is.matrix(x) | is.data.frame(x)
  if(m2) if(species_in_columns) m2 <- nrow(x) >= 2 else m2 <- ncol(x) >= 2
  l1 <- is.list(x) & is.null(dim(x))
  if(l1) l1 <- length(x) == 1
  l2 <- is.list(x) & is.null(dim(x)) 
  if(l2) l2 <- length(x) >= 2
  
  # Funciones internas:
  # Fun 1 ----
  rad_v_to_df <- function(x, s_keep = NULL, decreasing = TRUE, 
                          name_others = "Others", n_ind = 100,
                          max_fr_acum = NULL){
    
    # oct-2024
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
    # name_others = Cadena de texto
    # s_keep = puede ser un número, "auto", o "fr_acum". Esto para calcular
    #     el numero maximo de especies a mostrar, antes de juntar el resto
    #     en una categoria llamada como se indica en el argumento "name_others".
    #     Si un numero entero, indica el numero de especies a mantener directamente
    #     Si "auto" busca automaticamente el numero tratando de la fr. de la categoria
    #     "Otros" sea similar a la fr de la especie mas abundante.
    #     Si "fr_acum", se acumulan las especies que acumulen cierta fr_acum. En este caso
    #     se debe usar el max_fr_acum.
    # max_fr_acum = valor numérico entre 0 y n_ind que define el umbral 
    #     máximo de frecuencia relativa acumulada a conservar cuando
    #     s_keep = "fr_acum".
    
    # Calculos
    # Se validan el tipo de dato en 'x':
    if(class(x) %in% c("character", "factor") ){
      x <- table(as.character(x))
    }
    class(x) <- NULL
    if(is.null(names(x))) names(x) <- paste0('sp', 1:length(x))  # si no hay nombres que lo asigne
    x <- sort(x[x > 0], decreasing = T)
    k <- 1:length(x)
    s <- length(x)    # riqueza
    n <- sum(x)       # abundancia total
    sp <- names(x)    # nombres de especies ya ordenadas
    names(x) <- NULL
    
    # Se organiza un data.frame con todas las especies:
    DF <- data.frame(sp, k, f = x, fr = x / n * n_ind)
    DF$f_acum <- cumsum(DF$f)
    DF$fr_acum <- cumsum(DF$fr)
    
    # Funcion para ordenar el data.frame resultante
    ordena_df <- function(df, decreasing = F){
      if(isFALSE(decreasing)){
        df <- df[nrow(df):1, ]
        row.names(df) <- NULL
        df
      } else{
        df
      }
    }
    
    # En caso de que s_keep sea nulo o s_keep mayor o igual a s
    imprima_DF <- is.null(s_keep) || (is.numeric(s_keep) && s_keep >= s)
    if(imprima_DF){
      DF <- ordena_df(DF, decreasing = decreasing)
      DF$sp <- factor(DF$sp, levels = DF$sp)
      DF   # se imprime
    } else if(tolower(s_keep) %in% c("auto", "a", "au", "aut")){
      # Busqueda autmoatica de s_keep
      s_keep <- 1  # se define valor inicial para s_keep
      repeat {
        s_keep <- s_keep + 1     # se actualiza s_keep
        DF1 <- DF[1:s_keep, ]
        DF2 <- DF[(s_keep+1):s, ]
        DF1$n_sp <- 1  # <--- NUEVO: asignar columna n_sp = 1
        DF3 <- data.frame(
          sp = name_others,
          k = paste(DF2[1, "k"], DF2[nrow(DF2), "k"], sep = "-"),
          f = sum(DF2$f),
          fr = sum(DF2$fr),
          f_acum = sum(DF$f),
          fr_acum = sum(DF$fr),
          n_sp = nrow(DF2)
        )
        DFr <- rbind(DF1, DF3)
        if(DFr$fr[nrow(DFr)] <= DF$fr[1]) break
      }
      DFr <- ordena_df(DFr, decreasing = decreasing)
      DFr$sp <- factor(DFr$sp, levels = DFr$sp)
      DFr  # se imprime
    } else if(tolower(s_keep) %in% c("fr_acum", "fr", "fracum", "fr_a", "fr_ac", "fr_acu")) {
      # Determinar cuántas especies acumulan al menos max_fr_acum
      if(is.null(max_fr_acum)){
        stop("Debe definir el argumento 'max_fr_acum' cuando s_keep = 'fr_acum'")
      }
      if(!is.numeric(max_fr_acum) || max_fr_acum <= 0 || max_fr_acum > n_ind){
        stop("'max_fr_acum' debe ser un valor numérico entre 0 y 100")
      }
      
      pos <- which(DF$fr_acum >= max_fr_acum)[1]
      if(is.na(pos)) pos <- s  # en caso de que ninguna fr_acum lo alcance, usar todas
      DF1 <- DF[1:pos, ]
      DF2 <- DF[(pos+1):s, , drop = FALSE]
      
      if(nrow(DF2) == 0){
        DF1 <- ordena_df(DF1, decreasing = decreasing)
        DF1$sp <- factor(DF1$sp, levels = DF1$sp)
        DF1
      } else {
        DF1$n_sp <- 1  # <--- NUEVO: asignar columna n_sp = 1
        DF3 <- data.frame(
          sp = name_others,
          k = paste(DF2[1, "k"], DF2[nrow(DF2), "k"], sep = "-"),
          f = sum(DF2$f),
          fr = sum(DF2$fr),
          f_acum = sum(DF$f),
          fr_acum = sum(DF$fr),
          n_sp = nrow(DF2)
        )
        DFr <- rbind(DF1, DF3)
        DFr <- ordena_df(DFr, decreasing = decreasing)
        DFr$sp <- factor(DFr$sp, levels = DFr$sp)
        DFr  # se imprime
      }
      
    } else{
      # s_keep debe ser numerico y menor que s desde su definicion como argumento
      DF1 <- DF[1:s_keep, ]
      DF2 <- DF[(s_keep+1):s, ]
      DF1$n_sp <- 1  # <--- NUEVO: asignar columna n_sp = 1
      DF3 <- data.frame(
        sp = name_others,
        k = paste(DF2[1, "k"], DF2[nrow(DF2), "k"], sep = "-"),
        f = sum(DF2$f),
        fr = sum(DF2$fr),
        f_acum = sum(DF$f),
        fr_acum = sum(DF$fr),
        n_sp = nrow(DF2)
      )
      DFr <- rbind(DF1, DF3)
      DFr <- ordena_df(DFr, decreasing = decreasing)
      DFr$sp <- factor(DFr$sp, levels = DFr$sp)
      DFr  # se imprime
    }
  } # Fin de la Fun 1
  
  # Fun 2 ----
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
  }   # Fin de Fun 2
  
  # Fun 3 ----
  list_to_df <- function(lst, add_id = TRUE) {
    # 20-jul-2025
    # IA ChatGPT
    # convierte lista de df a un solo df
    
    # Verifica que todos los elementos de la lista sean data.frame
    if (!all(sapply(lst, is.data.frame))) {
      stop("Todos los elementos de la lista deben ser data.frame")
    }
    
    # Verifica que todas las columnas sean iguales
    colnames_list <- lapply(lst, names)
    if (!all(sapply(colnames_list, function(x) identical(x, colnames_list[[1]])))) {
      stop("Todos los data.frame deben tener las mismas columnas y en el mismo orden")
    }
    
    # Asigna nombres si la lista no los tiene
    if (is.null(names(lst))) {
      names(lst) <- as.character(seq_along(lst))
    } else {
      names(lst)[names(lst) == ""] <- as.character(which(names(lst) == ""))
    }
    
    # Añade columna .id si se solicita
    if (add_id) {
      lst <- mapply(function(df, id) {
        df$.id <- id
        df
      }, lst, names(lst), SIMPLIFY = FALSE)
    }
    
    # Combina todos los data.frame en uno solo
    result <- do.call(rbind, lst)
    
    # Reordena columnas para que .id sea la primera
    if (add_id) {
      result <- result[, c(".id", setdiff(names(result), ".id"))]
    }
    
    # Elimina nombres de fila
    rownames(result) <- NULL
    
    return(result)
  }
  
  # Calculos ---- 
  if (v) {
    if (is.null(collapse_by)) {
      res <- rad_v_to_df(x,
                        decreasing = decreasing,
                        n_ind = n_ind,
                        s_keep = s_keep,
                        name_others = name_others, max_fr_acum = max_fr_acum)
      
    } else {
      # Validaciones
      if (length(collapse_by) != length(x)) {
        stop("El vector 'collapse_by' debe tener la misma longitud que 'x'")
      }
      if (is.null(col_name_group)) {
        stop("Debes definir un nombre para la columna de grupo con el argumento 'col_name_group'")
      }
      
      # Separar índices por grupo
      grupos <- split(seq_along(x), collapse_by)
      x_lista <- lapply(grupos, function(i) {
        x_sub <- x[i]
        tapply(x_sub, names(x_sub), sum)
      })
      
      # Aplicar rad_v_to_df a cada grupo y añadir columna con nombre del grupo
      res <- mapply(function(x_sub, grupo_nombre) {
        df_sub <- rad_v_to_df(x_sub,
                              decreasing = decreasing,
                              n_ind = n_ind,
                              s_keep = s_keep,
                              name_others = name_others, max_fr_acum = max_fr_acum)
        df_sub[[col_name_group]] <- grupo_nombre
        # Reordenar columnas para que grupo quede al inicio
        df_sub <- df_sub[, c(col_name_group, setdiff(names(df_sub), col_name_group))]
        return(df_sub)
      },
      x_sub = x_lista,
      grupo_nombre = names(x_lista),
      SIMPLIFY = FALSE)
      
      # ¿El resultado sera un solo DF con todas las tablas juntas o una lista de DF?
      res <- if(as_list) res else list_to_df(res, add_id = F)
    }
  }
  
  if(m1){
    if(species_in_columns) x <- x[1, ] else x <- x[, 1]
    if(is.null(names(x))) names(x) <- paste0('sp', 1:length(x))
    res <- rad_v_to_df(x, decreasing = decreasing, n_ind = n_ind, s_keep = s_keep,  
                       name_others = name_others, max_fr_acum = max_fr_acum)
  }
  
  if(l1){
    x <- x[[1]]
    if(is.null(names(x))) names(x) <- paste0('sp', 1:length(x))
    res <- rad_v_to_df(x, decreasing = decreasing, n_ind = n_ind, max_fr_acum = max_fr_acum)
  }
  
  if(m2){
    if(is.null(collapse_by)){
      res0 <- apply(x, MARGIN = ifelse(species_in_columns, 1, 2),
                    rad_v_to_df, decreasing = decreasing, n_ind = n_ind, 
                    s_keep = s_keep, name_others = name_others, max_fr_acum = max_fr_acum)
      dfg  <- data.frame(g = if(species_in_columns) row.names(x) else colnames(x))
      names(dfg) <- col_name_sample
      res  <- add_g_to_df_list(x = res0, g = dfg, res.df = !as_list) 
    } else{
      n_sample <- if(species_in_columns) nrow(x) else ncol(x)
      n_unique_g <- length(unique(collapse_by))
      eval_g <- (length(collapse_by) == n_sample) & (n_unique_g < n_sample)
      eval_g_txt <- "'collapse_by' debe ser un vector de la misma longitud que el número de muestras en 'x' y
            el número de valores únicos en 'collapse_by' debe ser menor que el número de muestras en 'x'"
      if(!eval_g) stop(eval_g_txt)
      xs <- split(x, collapse_by)
      g <- names(xs)
      xs <- if(species_in_columns) sapply(xs, function(x) colSums(x), simplify = F) else sapply(xs, function(x) rowSums(x), simplify = F)
      res0 <- sapply(xs, rad_v_to_df, decreasing = decreasing, n_ind = n_ind, 
                     s_keep = s_keep, name_others = name_others, max_fr_acum = max_fr_acum,
                     simplify = FALSE)
      dfg  <- data.frame(g = g)
      names(dfg) <- col_name_group
      res  <- add_g_to_df_list(x = res0, g = dfg, res.df = !as_list)
    }
  }
  
  if(l2){
    res0 <- sapply(x, rad_v_to_df, decreasing = decreasing, n_ind = n_ind, 
                   s_keep = s_keep, name_others = name_others, max_fr_acum = max_fr_acum,
                   simplify = FALSE)
    dfg  <- data.frame(g = names(x))
    names(dfg) <- col_name_sample
    res  <- add_g_to_df_list(x = res0, g = dfg, res.df = !as_list)
  }
  
  # Se imprime el resultado -----
  res
  
  # # Ejemplos
  # # Simulacion de un vector de abundancias por especie:
  # x <- sample(paste0("sp",  1:40))  # vector de nombres de especies
  # d <- dexp(x = 1:40, rate = 0.08)  # vector de frecuencias relativas
  # # simulacion de muestra de 500 individuos
  # x <- sample(x, size = 500, replace = T, prob = d)
  # ab <- table(x)  # abundancias por especie
  # sum(ab)  # debe dar 500
  # length(ab)  # riqueza
  # 
  # # Se aplica la funcion rad_lump
  # res <- rad(x = ab, s_keep = NULL)                    # no se agrupa
  # str(res)  # revise la estructura del objeto
  # res       # imprima el objeto
  # rad(x = ab, s_keep = NULL, decreasing = F)    # no se agrupa
  # rad(x = ab, s_keep = "auto")  # con determinacion automatica de s_keep
  # rad(x = ab, s_keep = 50)      # no se agrupa porque s_keep > riqueza
  # rad(x = ab, s_keep = 10)      # se agrupan a partir de la especie 11 mas abundante
  # rad(x = ab, s_keep = 10, decreasing = F)      # se agrupan a partir de la especie 11 mas abundante
  # 
  # 
  # # Grafico
  # library(ggplot2)
  # df <- rad(x = ab, s_keep = 30, decreasing = F)  # con determinacion automatica de s_keep
  # str(df)
  # ggplot(df, aes(x = fr, y = sp)) +
  #   geom_col(fill = "tomato1") +
  #   scale_x_continuous(expand = c(0,0)) +
  #   labs(x = "Abundancia (%)", y = "Especies")
  # 
  # # RAD:
  # df <- rad(x = ab)  
  # ggplot(df, aes(x = k, y = fr)) +
  #   geom_path() +
  #   labs(x = "rank de Especies", y = "Abundancia (%)")
  # 
  # # Grafico de abundancia acumulada
  # ggplot(df, aes(x = k, y = fr_acum)) +
  #   geom_line() +
  #   labs(x = "rank de Especies", y = "Abundancia acumulada (%)")
  # 
  # # RAD por muestras
  # require(vegan)
  # data(dune)
  # data(dune.env)
  # dim(dune)   # 20 sitios (filas) x 30 especies (columnas)
  # rad_dune <- rad(x = dune)  # rad para cada sitio
  # str(rad_dune)
  # ggplot(rad_dune, aes(x = k, y = fr, group = sample)) +
  #   geom_line() +
  #   labs(x = "rank de Especies", y = "Abundancia (%)")
  # 
  # # RAD colapsando por el 'Use' de cada sitio
  # rad_dune <- rad(x = dune, collapse_by = dune.env$Use, 
  #                 col_name_group = "Use")  # rad para cada sitio
  # str(rad_dune)
  # ggplot(rad_dune, aes(x = k, y = fr, color = Use)) +
  #   geom_line() +
  #   labs(x = "rank de Especies", y = "Abundancia (%)")
  
  
}
