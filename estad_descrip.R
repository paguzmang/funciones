estad_descrip <- function(x, r = NULL, var.en.filas = TRUE, estad_forma = FALSE, 
                          n.auto = TRUE, n.dist = FALSE, print_tibble = FALSE){
  
  # Fecha: 27-nov-2024
  # Autor: paguzmang
  
  # DESCRIPCION:
  # Funcion que calcula estadisticos descriptivos sobre un vector, o sobre una o 
  # mas columnas de un data.frame. Calcula el numero de datos, el min, q25, q50, q75, max,
  # media, desv. estandar, coef. de variacion el sesgo y la kurtosis.
  # Permite pasarse por la funcion ddply (del paquete plyr).
  # para hacer calculos por grupos (split - apply - combine). Ver ejemplos.
  
  # ARGUMENTOS:
  # x = vector numerico o data.frame (o tibble) con columnas numericas. Si el vector no es 
  #     numerico o el data.frame no tiene al menos una columna numerica, la funcion se 
  #     detiene y genera un mensaje de error.
  #
  # r = Si x es un data.frame, permite identificar con un vector de texto o numeros cuales columnas se
  #     analizaran. Si no se usa (se deja NULL), se seleccionan automaticamente todas las columnas 
  #     numericas del data.frame. Si no hay ninguna, la funcion se detiene y genera un mensaje de error.
  #
  # var.en.filas = Logico. Si length(r) > 1 (dos o mas columnas), indica si, en el resultado, las variables se ubicaran
  #     en las filas (por defecto) y los estadisticos en las columnas o viceversa.
  #
  # estad_forma = Si TRUE, se imprimen en los estadisticos, el sesgo y la kurtosis, calculados desde
  #     las funciones e1071::skewness y e1071::kurtosis. Consulte su ayuda ?e1071::skewness para
  #     interpretacion.
  #
  # n.auto = Logico. Si TRUE (por defecto), la propia funcion detectara si hay valores perdidos.
  #       Si los encuentra, entregara tres cantidades: n.total, n.na y n. Si no los encuentra solo
  #       entregara n. Si FALSE, siempre entregara n.total, n.na y n (existan o no datos perdidos)
  #
  # n.dist = Logico. FALSE por defecto. Si TRUE, se entregan las siguientes cantidades:
  #       n.dist = nro. de valores distintos
  #       n.ceros = nro. de ceros
  #       moda = Valor con la frecuencia mas alta. Si existen dos o mas valores con la misma frecuencia
  #              mas alta, se promedian para entregar uno solo. Si n.dist = n, moda = media.
  #       n.moda = frecuencia absoluta correspondiente a la moda.
  # 
  # print_tibble = Si TRUE, el resultado se imprime como un tibble y no como un data.frame (por defecto)
  
  # CODIGO:
  # Funcion para verificar si un paquete(s) ya esta instalado, y si no, que lo instale(s)
  check.packages <- function(pkg){
    # pkg = vector de character con los nombres de los paquetes a verficarse e instalarse
    new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
    if(length(new.pkg)) 
      install.packages(new.pkg, dependencies = TRUE)
  }
  
  # CODIGO:
  # Aqui se establece un valor por defecto para r (todas las columnas numericas)
  # Si no hay ninguna numerica (lo que deberia ser un error), se fija r = 1 y 
  # en el bloque de codigo siguiente, la funcion se detiene puesto que dicha columna 
  # no es numerica.
  if(is.null(r) & is.data.frame(x)) {
    temp <- sapply(as.data.frame(x), is.numeric)
    r <- if(any(temp))  which(temp) else  1
  }
  
  # Mensajes de parada por ingreso incorrecto de argumentos:
  if( !(is.vector(x) | is.data.frame(x)) ) stop('x debe ser un vector o un data.frame')
  tipo.x <- if(is.vector(x)) is.numeric(x) else{
    w <- sapply(as.data.frame(x[, r]), is.numeric )
    sum(w) == length(r)
  }
  if( !tipo.x ) stop('x debe ser un vector numerico, o un data.frame con columnas numericas indicadas con el argumento r')
  
  # Funcion que calcula estadisticos y opera sobre un vector y controla
  # el tipo del resultado:
  fun1 <- function(x, tipo.res = c('v', 'd'), auto.n = T, n.dist = F, estad_forma = F, ...){
    
    # Calculo de estadisticos:
    n.total <- length(x)
    n.na <- sum(is.na(x))
    n <- n.total - n.na
    res.n <- c(n.total = n.total, n.na = n.na, n = n)
    if(auto.n){
      res.n <- if(n.na == 0)  c(n = n) else c(n.total = n.total, n.na = n.na, n = n)
    }
    if(n > 0){
      m <- mean(x, ...)
      s <- sd(x, ...)
      cv <- ifelse(n >= 2, round(s/m*100,2), NA)
      q <- fivenum(x, ...)
      names(q) <- c('min', 'q25', 'q50', 'q75', 'max')
      if(estad_forma){
        check.packages(pkg = 'e1071')
        sesgo <- ifelse(n >= 4,  e1071::skewness(x, type = 2, ...), NA)
        kurt  <- ifelse(n >= 4,  e1071::kurtosis(x, type = 2, ...), NA)
      }
    } else{
      m <- s <- cv <- mg <- mad <- cvr <- sesgo <- kurt <- NA
      q <- rep(NA, 5)
      names(q) <- c('min', 'q25', 'q50', 'q75', 'max')
    }
    
    
    # Organizacion de resultados en un vector de acuerdo al valor del argumento 'est':
    res <- if(estad_forma){
      c(q, media = m, de = s, cv = cv, sesgo = sesgo, kurt = kurt)
    } else{
      c(q, media = m, de = s, cv = cv)
    }
    
    # Funcion para calcular moda y n.moda:
    moda <- function(x){
      n.na <- sum(is.na(x))
      n <- length(x) - n.na
      x1 <- x[!is.na(x)]
      if(n > 0){
        tab <- table(x1)                 # frec absoluta de cada valor
        n_mas <-  as.numeric(max(tab))   # frec absoluta mas grande
        moda  <-  mean(as.numeric(names(tab)[tab == n_mas]))   # moda.
        res <- c(moda = moda, n.moda = n_mas)
      } else{
        res <- c(moda = NA, n.moda = NA)
      }
      res
    }
    
    # Condicional para actualizar el res generado arriba agregandole informacion
    # del tamano de muestra contenida en res.n o incluso adicionando tambien
    # informacion de frecuencias relevantes y moda dependiendo del argumento logico n.dist
    res <- if(n.dist){
      x1 <- x[!is.na(x)]
      n_dist  <- ifelse(n > 0, length(unique(x1)), 0)    # nro. de valores distintos
      n_ceros <- ifelse(n > 0, sum(x1 == 0), 0)          # nro. de ceros
      r.moda <- moda(x)
      ns <- c(n.dist = n_dist, n.ceros = n_ceros, r.moda)  # se guarda todo
      c(res.n, ns, res)   # se entrega vector actualizado
    } else{
      c(res.n, res)       # se entrega vector actualizado
    } 
    
    # Entrega el resultado en un vector o data.frame:
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
      res <- fun1(x = x, tipo.res = 'd', na.rm = T, auto.n = n.auto, n.dist = n.dist, estad_forma = estad_forma)
    } else{
      res <- fun1(x = as.numeric(unlist(x[, r])), tipo.res = 'd', na.rm = T,  
                  auto.n = n.auto, n.dist = n.dist, estad_forma = estad_forma)
    }
    # Si x es un data.frame y r indica 2 o mas columnas 
  } else{
    y <- as.data.frame(x[, r])
    col.na <- any(apply(y, 2, is.na))
    if(var.en.filas){
      res <- t(apply(y, 2, fun1, tipo.res = 'v', na.rm = T, auto.n = ifelse(n.auto, !col.na, F), 
                     n.dist = n.dist, estad_forma = estad_forma) )
      rownames(res) <- NULL
      res0 <- data.frame( variable = names(x[, r]))
      res <- cbind(res0, res)
    } else{
      res <- apply(y, 2, fun1, tipo.res = 'v', na.rm = T,  auto.n = ifelse(n.auto, !col.na, F), 
                   n.dist = n.dist, estad_forma = estad_forma )
      nom.est <- rownames(res)
      rownames(res) <- NULL
      res0 <- data.frame( est = nom.est )
      res <- cbind(res0, res)
    }
  }
  
  # Se entrega el resultado dependiendo del argumento 'print':
  if(print_tibble){
    check.packages(pkg = 'tibble')
    tibble::as_tibble(res)
  } else{
    as.data.frame(res)
  }
  
  # VALOR:
  # Entrega un data.frame donde las filas representan variables o estadisticos
  # y las columnas lo mismo, dependiendo de lo marcado en el argumento var.en.filas.
  
  # EJEMPLOS:
  # library(MASS)
  # data(birthwt)
  # Sobre un vector:
  # estad_descrip(x = birthwt$bwt)
  # estad_descrip(x = birthwt$bwt, estad_forma = T)
  # estad_descrip(x = c(birthwt$bwt, NA, NA, NA))   # con datos perdidos
  # estad_descrip(x = birthwt$bwt, n.auto = F)
  # estad_descrip(x = as.character(birthwt$bwt))  # valiando argumento 'x'
  
  # Sobre un data.frame (sin datos perdidos):
  # estad_descrip(x = birthwt)   # por defecto r = NULL, que indica todas las columnas
  # estad_descrip(x = birthwt, n.auto = F)   # por defecto r = NULL, que indica todas las columnas
  # estad_descrip(x = birthwt, r = 'bwt')
  # estad_descrip(x = birthwt, r = c('bwt', 'age') )   # dos columnas
  # estad_descrip(x = birthwt, r = c('bwt', 'age'), var.en.filas = F )   # dos columnas y var en columnas
  
  # Sobre un data.frame con datos perdidos:
  # datos1 <- data.frame(x = c(35.5, NA, 10.9, 25.8, NA), y = c(5.67, 8.91, NA, 6.34, 2.89))
  # datos2 <- data.frame(x = c(35.5, NA, 10.9, 25.8, NA), y = c(5.67, 8.91, 7.34, 6.34, 2.89))
  # estad_descrip(x = datos1) 
  # estad_descrip(x = datos1, estad_forma = T) 
  # estad_descrip(x = datos1,  var.en.filas = F)   # no muy conveniente por el tema de decimales
  # estad_descrip(x = datos1,  var.en.filas = F, estad_forma = T)   # no muy conveniente por el tema de decimales
  # estad_descrip(x = datos1, n.dist = T)
  # estad_descrip(x = datos1, n.dist = T, print_tibble = T)
  
  # Usando ddply (de plyr):
  # require(plyr)
  # plyr::ddply(birthwt,  ~ smoke + race, estad_descrip, r =  'bwt')
  # plyr::ddply(birthwt,  ~ smoke + race, estad_descrip, r =  c('bwt', 'age'))
  # plyr::ddply(birthwt,  ~ smoke + race, estad_descrip, r =  c('bwt', 'age'), var.en.filas = F)
  # plyr::ddply(birthwt,  ~ smoke + race, estad_descrip, r =  c('bwt', 'age'), var.en.filas = F, estad_forma = T)
  # plyr::ddply(birthwt,  ~ smoke + race, estad_descrip, r =  c('bwt', 'age'), 
  #             var.en.filas = F, estad_forma = T, print_tibble = T)     # no funciona print_tibble dado que 'ddply' entrega un data.frame 
}
