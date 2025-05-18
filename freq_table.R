freq_table <- function(data, variable = 1,
                       breaks = c('auto', 'no')[1], 
                       variable_name = "variable",
                       group_by = NULL, order_by_freq = FALSE,
                       dec_f = 4){
  
  # 21-ago-2024
  # pguzmang
  # Descripcion: funcion que realiza una tabla de frecuencias para
  #   una sola variable, categorica o numerica. Dependiendo del tipo de
  #   variable la impresion de la tabla sera distinta.
  
  # Argumentos:
  # data: vector o data.frame de datos
  # variable: indicador de la variable a analizar si 'data' es un data.frame. 
  #    Puede ser un entero positivo o una cadena texto
  # breaks = indicador para definir limites de los intervalos en caso de que
  #    la variable sea numérica. Si breaks = 'auto' y el número de 
  #    valores unicos es 21 o mas, el programa determina los intervalos de 
  #    forma automática con el comando hist. Si breaks = "no", no se generan
  #    intervalos a pesar de que la variable sea numérica. 'breaks' tambien puede
  #    ser un vector de tres o más números para definir de manera personalizada
  #    los intervalos.
  # variable_name = Cadena de texto indicando el nombre de la variable en caso
  #    de que "data" sea un vector.
  # group_by = Indicador para entregar las frecuencias separadas por las categorias
  #    de una variable categoria. Si "data" es un vector, "group_by" deberia ser
  #    otro vector de la misma longitud que "data" con los valores de dicha variable.
  #    Si "data" es un data.frame, "group_by" debería ser un indicador de la variable
  #    de "data" que actuará como variable de grupo. Este indicador puede ser un número
  #    indicando la posición, o una cadena de texto indicando el nombre.
  # dec_f = numero de decimales para imprimir la frec. relativa como fracción. 
  #    La frec. relativa en porcentaje se reporta con dos decimales siempre.
  
    require(tibble)
  is.df <- is.data.frame(data) | is_tibble(data)
  is.v <- is.atomic(data) || is.factor(data)  # para que recibo factor o vector de texto
  
  # CASO CON AGRUPACIÓN
  if (!is.null(group_by)) {
    
    # Si 'data' es un vector
    if (is.v && !is.df) {
      if (length(data) != length(group_by)) stop("group_by debe tener la misma longitud que data.")
      df <- data.frame(.var = data, .grp = group_by)
      out <- lapply(split(df, df$.grp), function(subdf){
        freq_table(subdf$.var, variable_name = variable_name,
                   breaks = breaks, dec_f = dec_f,
                   order_by_freq = order_by_freq)
      })
      names(out) <- names(split(df, df$.grp))
      return(out)
    }
    
    # Si 'data' es un data.frame
    if (is.df) {
      if (is.numeric(group_by)) group_by <- names(data)[group_by]
      if (!(group_by %in% names(data))) stop("group_by no es una columna válida del data.frame.")
      groups <- unique(data[[group_by]])
      out <- lapply(groups, function(gr){
        subdata <- data[data[[group_by]] == gr, , drop = FALSE]
        freq_table(subdata, variable = variable, 
                   breaks = breaks, dec_f = dec_f,
                   variable_name = variable_name,
                   order_by_freq = order_by_freq)
      })
      names(out) <- as.character(groups)
      return(out)
    }
  }
  
  # CASO SIN AGRUPACIÓN
  total <- TRUE
  
  # Extracción
  if( is.df ) {
    n <- nrow(data)
    if(is.numeric(variable)) variable <- names(data)[variable] 
    y <- data[[variable]]
  } else{
    if(is.v){
      y <- data
      n <- length(y)
      variable <- variable_name
    } else{
      stop('data debe ser un data.frame o un vector')
    }
  }
  
  y0 <- y  # Original para validaciones
  if(is.numeric(y)){
    lu <- length(unique(y))
    lb <- length(breaks)
    if(lb == 1){
      if(breaks == 'auto'){
        if(lu > 20) {
          h <- hist(y, plot = F)
          y <- cut(y, breaks = h$breaks)
        }
      }
    } else{
      h <- hist(y, plot = F, breaks = breaks)
      y <- cut(y, breaks = breaks)
    }
  }
  
  if(total) {
    tab <- as.data.frame(addmargins(xtabs( ~ y)))
    tab[, 1] <- as.character(tab[, 1])
    tab[nrow(tab), 1] <- 'Total'
  } else{
    tab <- as.data.frame(xtabs( ~ y))
  }
  names(tab) <- c(variable, 'n')
  tab$f <- round(tab$n / n, dec_f)
  tab$f_pct <- round(tab$n / n *100, 2)
  if('h' %in% ls()){
    tab <- cbind(tab[, 1], mids = c(h$mids, " "), tab[, -1])
    names(tab)[1] <- variable
  }
  if( (is.numeric(y0) | is.ordered(y0)) & length(unique(y0)) >= 3 ){
    fa <- cumsum(xtabs(~ y))
    tab$n_acum <- as.character(c(fa, " "))
    tab$f_acum <- as.character(c(round(fa / n, dec_f), " "))
    tab$f_acum_pct <- as.character(c(round(fa / n *100, 2), " "))
  }
  
  # Aplicar ordenamiento si es válido
  if (order_by_freq) {
    if (is.character(y0) || (is.factor(y0) && !is.ordered(y0))) {
      total_row <- tab[tab[[variable]] == "Total", , drop = FALSE]
      main_rows <- tab[tab[[variable]] != "Total", , drop = FALSE]
      tab <- rbind(main_rows[order(-main_rows$n), ], total_row)
      rownames(tab) <- NULL
    } else {
      warning("La opción 'order_by_freq = TRUE' fue ignorada porque la variable no es character ni factor no ordenado.")
    }
  }

 # Se imprime
 tab  
}




                            
