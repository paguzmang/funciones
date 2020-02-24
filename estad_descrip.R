estad.descrip <- function(x, r = NULL, var.en.filas = T, est = c('n.m.s', 'todo')){
  
  # Fecha: 24-feb-2020
  # Autor: paguzmang
  
  # Descripcion:
  # Funcion que calcula estadisticos descriptivos sobre un vector, o sobre una o 
  # mas columnas de un data.frame. Permite pasarse por la funcion ddply (del paquete plyr).
  # para hacer calculos por grupos. Ver ejemplos.
  
  # Argumentos:
  # x = vector numerico o data.frame con columnas numericas
  # r = Si x es un data.frame permite identificar con texto o numeros cuales columnas se
  #     analizaran. Si no se pone nada, se asume r = 1 (1era. columna del data.frame)
  # var.en.filas = Logico. Si length(r) > 1 (dos o mas columnas), indica si las variables se ubicaran
  #     en las filas (por defecto) y los estadisticos en las columnas o viceversa.
  # est = Indica si quiere solo tres estadisticos (n, media y de) (por defecto) o se 
  #     quiere 'todo' (lo anterior mas cuantiles y cv). El n excluye 
  #     valores perdidos.
  
  # CODIGO:
  # Mensajes de parada por ingreso incorrecto de argumentos:
  if(is.null(r)) r <- 1
  if( !(is.vector(x) | is.data.frame(x)) ) stop('x debe ser un vector o un data.frame')
  tipo.x <- if(is.vector(x)) is.numeric(x) else{
    w <- apply(as.data.frame(x[, r]), 2, is.numeric )
    sum(w) == length(r)
  }
  if( !tipo.x ) stop('x debe ser un vector numerico o un data.frame indicando columnas numericas con r')
  if( ! est[1] %in% c( 'n.m.s', 'todo') ) stop("El argumento 'est' debe ser uno de 'n.m.s' o 'todo'")
  
  # Funcion que calcula estadisticos y opera sobre un vector y controla
  # el tipo del resultado:
  fun1 <- function(x, tipo.res = c('v', 'd'), est = c('n.m.s', 'todo'), ...){
    n.total <- length(x)
    n.na <- sum(is.na(x))
    n <- n.total - n.na
    m <- mean(x, ...)
    s <- sd(x, ...)
    cv <- round(s/m*100,2)
    q <- fivenum(x, ...)
    names(q) <- c('min', 'p25', 'p50', 'p75', 'max')
    res <- switch(est[1],
                  todo = c(n.total = n.total, n.na = n.na, n = n, q, 
                           media = m, de = s, cv = cv),
                  n.m.s  = c(n = n, media = m, de = s)
    )
    switch(tipo.res[1],
           v = res,
           d = {
             resd <- as.data.frame(as.list(res))
             names(resd) <- names(res)
             resd
           })
  }
  
  # Se ejecuta la funcion anterior dependiendo del tipo de x:
  if(is.vector(x) | length(r) == 1 ){
    # Si x es un vector o un data.frame y una sola columna
    if(is.vector(x))  {
      res <- fun1(x = x, tipo.res = 'd', na.rm = T, est = est[1])
    } else{
      res <- fun1(x = unlist(x[, r]), tipo.res = 'd', na.rm = T, est = est[1])
    }
   # Si x es un data.frame y r indica 2 o mas columnas 
  } else{
    y <- as.data.frame(x[, r])
    if(var.en.filas){
      res <- t(apply(y, 2, fun1, tipo.res = 'v', na.rm = T, est = est[1]))
      rownames(res) <- NULL
      res0 <- data.frame( var = names(x[, r]))
      res <- cbind(res0, res)
    } else{
      res <- apply(y, 2, fun1, tipo.res = 'v', na.rm = T, est = est[1])
      nom.est <- rownames(res)
      rownames(res) <- NULL
      res0 <- data.frame( est = nom.est )
      res <- cbind(res0, res)
    }
  }
  res
  
  # VALOR:
  # Entrega un data.frame donde las filas representan variables o estadisticos
  # y las columnas lo mismo, dependiendo de lo marcado en el argumento var.en.filas.
  
  # EJEMPLOS:
  # library(MASS)
  # data(birthwt)
  # Sobre un vector:
  # estad.descrip(x = birthwt$bwt)
  # estad.descrip(x = birthwt$bwt,  est = 'todo')
  # estad.descrip(x = c(birthwt$bwt, NA,NA, NA),  est = 'todo')  # con datos perdidos
  # estad.descrip(x = c(birthwt$bwt, NA,NA, NA),  est = 'otra.cosa')  # valiando argumento 'est'
  # estad.descrip(x = as.character(birthwt$bwt),  est = 'todo')  # valiando argumento 'x'
  
  # Sobre un data.frame:
  # estad.descrip(x = birthwt)   # por defecto r = 1 (columna 'low')
  # estad.descrip(x = birthwt, r = 'bwt')
  # estad.descrip(x = birthwt, r = c('bwt', 'age') )   # dos columnas
  # estad.descrip(x = birthwt, r = c('bwt', 'age'), var.en.filas = F )   # dos columnas y var en columnas
  # estad.descrip(x = birthwt, r = c('bwt', 'age'), var.en.filas = F, est = 'todo' )
  # estad.descrip(x = birthwt, r = c('bwt', 'age'), var.en.filas = T, est = 'todo' )
  
  # Usando ddply (de plyr):
  # require(plyr)
  # plyr::ddply(birthwt,  ~ smoke + race, estad.descrip, r =  c('bwt', 'age'), est = 'n.m.s', var.en.filas = T)
  # plyr::ddply(birthwt,  ~ smoke + race, estad.descrip, r =  c('bwt', 'age'), est = 'n.m.s', var.en.filas = F)
}