freq_table <- function(data, variable = 1,
                       breaks = c('auto', 'no')[1], 
                       variable_name = "variable",
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
  # dec_f = numero de decimales para imprimir la frec. relativa como fracción. 
  #    La frec. relativa en porcentaje se reporta con dos decimales siempre.
  
  total <- TRUE
  require(tibble)
  # Tipo de datos
  is.df <- is.data.frame(data) | is_tibble(data)
  is.v <- is.vector(data)
  
  # Extraccion de los datos
  if( is.df ) {
    n <- nrow(data)
    if(is.numeric(variable)) variable <- names(data)[variable] 
    y <- data[, variable]
  } else{
    if(is.v){
      y <- data
      n <- length(y)
      variable <- variable_name
    } else{
      stop('data debe ser un data.frame o un vector')
    }
  }
  
  # Cuando los datos son numericos
  y0 <- y
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
  
  # Tabla de frecuencia
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
  
  # se imprime
  tab
  
}




                            
