estad.descrip <- function(x, r = NULL, var.en.filas = T, 
                          est = c('n.m.s', 'n.m.s.cv', 'n.r.m.s', 'n.r.m.s.cv', 'todo', 'rob', 'quant'), 
                          n.auto = T, n.dist = F, print = c('data.frame', 'tibble')){
  
  # Fecha: 24-feb-2020
  # Autor: paguzmang
  
  # DESCRIPCION:
  # Funcion que calcula combos de estadisticos descriptivos sobre un vector, o sobre una o 
  # mas columnas de un data.frame. Permite pasarse por la funcion ddply (del paquete plyr).
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
  # var.en.filas = Logico. Si length(r) > 1 (dos o mas columnas), indica si las variables se ubicaran
  #     en las filas (por defecto) y los estadisticos en las columnas o viceversa.
  #
  # est = Indica que combinacion ('combo') de estadisticos se desea:
  #       n.m.s = tamano de muestra (n), media (m) y desviacion estandar (s) (opcion por defecto)
  #       n.m.s.cv = n, m, s y coef. de variacion (cv, en porcentaje)
  #       n.r.m.s = n, rango (r: min y max), m, s
  #       n.r.m.s.cv = n, r, m, s y cv.
  #       todo = n, m, s, r, cuartiles (quant), media geometrica (mg), desviacion mediana absoluta (mad), 
  #              sesgo, kurtosis (kurt), y coef. de variacion robusto (cvr = mad / q50 *100)
  #       rob = n y estadisticos robustos (quant, mg, mad, cvr)
  #       quant = n, r, cuartiles.
  #       En todos los casos, si hay valores perdidos, la funcion los saca antes del calculo. Ademas
  #       el n reportado depende del argumento n.auto.
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
  posibles.est <- c('n.m.s', 'n.m.s.cv', 'n.r.m.s', 'n.r.m.s.cv', 'todo', 'rob', 'quant')
  if( !(is.vector(x) | is.data.frame(x)) ) stop('x debe ser un vector o un data.frame')
  tipo.x <- if(is.vector(x)) is.numeric(x) else{
    w <- sapply(as.data.frame(x[, r]), is.numeric )
    sum(w) == length(r)
  }
  if( !tipo.x ) stop('x debe ser un vector numerico, o un data.frame con columnas numericas indicadas con el argumento r')
  if( ! est[1] %in% posibles.est ) {
    stop(paste0("El argumento 'est' debe ser uno de: ", posibles.est) )
  }
  
  # Funcion que calcula estadisticos y opera sobre un vector y controla
  # el tipo del resultado:
  fun1 <- function(x, tipo.res = c('v', 'd'), 
                   est = c('n.m.s', 'n.m.s.cv', 'n.r.m.s', 'n.r.m.s.cv', 'todo', 'rob', 'quant'), 
                   auto.n = T, n.dist = F, ...){
    
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
      x1 <- x[!is.na(x)]
      # mg <- prod(x1)^(1/n)      # media geometrica
      mg <- ifelse(min(x1) <= 0, NA, exp( mean(log(x1)) )  )     # media geometrica
      mad <- median( abs(as.numeric(q['q50']) - x1) )   # desv. mediana absoluta
      cvr <- round(mad / as.numeric(q['q50']) *100, 2)  # coef. de variacion robusto
      if(est == 'todo') {
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
    res <- switch(est[1],
                  todo = c(q, media = m, de = s, cv = cv, sesgo = sesgo, kurt = kurt,
                           media.geom = mg, mad = mad, cvr = cvr),
                  rob = c(q, media.geom = mg, mad = mad, cvr = cvr),
                  n.m.s  = c(media = m, de = s),
                  n.r.m.s = c(q['min'], q['max'], media = m, de = s),
                  n.m.s.cv = c(media = m, de = s, cv = cv),
                  n.r.m.s.cv = c(q['min'], q['max'], media = m, de = s, cv = cv),
                  quant  = q
    )
    
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
      res <- fun1(x = x, tipo.res = 'd', na.rm = T, est = est[1], auto.n = n.auto, n.dist = n.dist)
    } else{
      res <- fun1(x = as.numeric(unlist(x[, r])), tipo.res = 'd', na.rm = T, 
                  est = est[1], auto.n = n.auto, n.dist = n.dist)
    }
   # Si x es un data.frame y r indica 2 o mas columnas 
  } else{
    y <- as.data.frame(x[, r])
    col.na <- any(apply(y, 2, is.na))
    if(var.en.filas){
      res <- t(apply(y, 2, fun1, tipo.res = 'v', na.rm = T, est = est[1], auto.n = ifelse(n.auto, !col.na, F), n.dist = n.dist) )
      rownames(res) <- NULL
      res0 <- data.frame( var = names(x[, r]))
      res <- cbind(res0, res)
    } else{
      res <- apply(y, 2, fun1, tipo.res = 'v', na.rm = T, est = est[1],  auto.n = ifelse(n.auto, !col.na, F), n.dist = n.dist )
      nom.est <- rownames(res)
      rownames(res) <- NULL
      res0 <- data.frame( est = nom.est )
      res <- cbind(res0, res)
    }
  }
  
  # Se entrega el resultado dependiendo del argumento 'print':
  switch(print[1],
         data.frame = res,
         tibble = {
           check.packages(pkg = 'tibble')
           tibble::as_tibble(res)
         }
  )
  
  # VALOR:
  # Entrega un data.frame donde las filas representan variables o estadisticos
  # y las columnas lo mismo, dependiendo de lo marcado en el argumento var.en.filas.
  
  # EJEMPLOS:
  # library(MASS)
  # data(birthwt)
  # Sobre un vector:
  # estad.descrip(x = birthwt$bwt)
  # estad.descrip(x = c(birthwt$bwt, NA, NA, NA))   # con datos perdidos
  # estad.descrip(x = birthwt$bwt,  est = 'todo')
  # estad.descrip(x = birthwt$bwt,  est = 'todo', n.auto = F)
  # estad.descrip(x = c(birthwt$bwt, NA,NA, NA),  est = 'todo')  # con datos perdidos
  # estad.descrip(x = c(birthwt$bwt, NA,NA, NA),  est = 'otra.cosa')  # valiando argumento 'est'
  # estad.descrip(x = as.character(birthwt$bwt),  est = 'todo')  # valiando argumento 'x'
  # estad.descrip(x = birthwt$bwt,  est = 'quant')  # solo cuartiles y n
  # estad.descrip(x = c(birthwt$bwt, NA,NA, NA),  est = 'quant')  # solo cuartiles y n, con datos perdidos
  # estad.descrip(x = c(birthwt$bwt, NA,NA, NA),  est = 'rob')  # solo robustos y n, con datos perdidos
  # estad.descrip(x = birthwt$bwt,  est = 'rob')  # solo robustos y n, sin datos perdidos
  
  # Sobre un data.frame (sin datos perdidos):
  # estad.descrip(x = birthwt)   # por defecto r = NULL, que indica todas las columnas
  # estad.descrip(x = birthwt, n.auto = F)   # por defecto r = NULL, que indica todas las columnas
  # estad.descrip(x = birthwt, r = 'bwt')
  # estad.descrip(x = birthwt, r = c('bwt', 'age') )   # dos columnas
  # estad.descrip(x = birthwt, r = c('bwt', 'age'), var.en.filas = F )   # dos columnas y var en columnas
  # estad.descrip(x = birthwt, r = c('bwt', 'age'), var.en.filas = F, est = 'todo' )
  # estad.descrip(x = birthwt, r = c('bwt', 'age'), var.en.filas = F, est = 'todo', n.auto = F )
  # estad.descrip(x = birthwt, r = c('bwt', 'age'), var.en.filas = T, est = 'todo' )
  # estad.descrip(x = birthwt, r = c('bwt', 'age'), var.en.filas = T, est = 'rob' )  # solo estad. robustos
  # estad.descrip(x = birthwt, r = c('bwt', 'age'), var.en.filas = F, est = 'rob' )  # solo estad. robustos
  # estad.descrip(x = birthwt, r = c('bwt', 'age'), var.en.filas = T, est = 'quant' )  # solo cuartiles
  # estad.descrip(x = birthwt, r = c('bwt', 'age', 'ftv'), var.en.filas = F, est = 'quant' )  # solo cuartiles, tres variables
  
  # Sobre un data.frame con datos perdidos:
  # datos1 <- data.frame(x = c(35.5, NA, 10.9, 25.8, NA), y = c(5.67, 8.91, NA, 6.34, 2.89))
  # datos2 <- data.frame(x = c(35.5, NA, 10.9, 25.8, NA), y = c(5.67, 8.91, 7.34, 6.34, 2.89))
  # estad.descrip(x = datos1, var.en.filas = T)  
  # estad.descrip(x = datos1,  var.en.filas = F)   # no muy conveniente por el tema de decimales
  # estad.descrip(x = datos1,  var.en.filas = T, est = 'rob')  
  # estad.descrip(x = datos2,  var.en.filas = T, est = 'rob')  # una columna con datos perdidos y otra sin datos perdidos
  # estad.descrip(x = datos2,  r = 1, var.en.filas = T, est = 'rob')
  # estad.descrip(x = datos2,  r = 2, var.en.filas = T, est = 'rob')
  # estad.descrip(x = datos2,  r = 2, var.en.filas = T, est = 'rob', n.auto = F)
  # estad.descrip(x = datos2,  r = 2, var.en.filas = T, est = 'todo', n.auto = F)
  
  # Usando ddply (de plyr):
  # require(plyr)
  # plyr::ddply(birthwt,  ~ smoke + race, estad.descrip, r =  'bwt', est = 'n.m.s', var.en.filas = T)
  # plyr::ddply(birthwt,  ~ smoke + race, estad.descrip, r =  'bwt', est = 'rob', var.en.filas = T)   # solo robustos
  # plyr::ddply(birthwt,  ~ smoke + race, estad.descrip, r =  'bwt', est = 'todo', var.en.filas = T)   # todo
  # plyr::ddply(birthwt,  ~ smoke + race, estad.descrip, r =  c('bwt', 'age'), est = 'n.m.s', var.en.filas = T)
  # plyr::ddply(birthwt,  ~ smoke + race, estad.descrip, r =  c('bwt', 'age'), est = 'n.m.s', var.en.filas = F)
}
