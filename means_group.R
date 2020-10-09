means_group <- function(data, response, group, 
                        alternative = c("two.sided", "less", "greater"),
                        conf = 0.95, var.equal = FALSE, null.dif = 0){
  
  # 9-oct-2020
  # Version 0.1
  # Autor: pguzmang
  
  # DESCRIPCION ----
  # En el contexto de comparar una respuesta numerica entre dos o mas grupos
  # bajo un diseno de muestras independientes, este comando
  # entrega un data.frame con las medias de la respuesta numerica
  # por cada nivel o categoria de la variable de grupo, para minimo
  # dos grupos. El data.frame entrega ademas el tamano de muestra, desv. estandar, 
  # error estandar, e intervalos de confianza por grupo. 
  # Adicionalmente se entrega, en otro data.frame, una prueba de hipotesis basada 
  # en t.test si los grupos comparados son dos, o basada en un analisis de varianza 
  # (comando aov) si el nro. de grupos comparados es tres o mas.
  # El data.frame de medias es util para luego hacer un grafico de medias con barras
  # de error y escoger entre diferentes opciones para dichas barras de error.
  # El data.frame de la prueba de hipotesis es util puesto que muestra resultados que
  # no entrega el comando t.test tales como la diferencia de medias y el error estandar
  # de la dif. de medias. Ver ejemplos.
  
  # ARGUMENTOS ----
  # data = data.frame con minimo dos columnas, una de ellas numerica. Puede ser tambien
  #     un tibble (del tidyverse)
  # response = cadena de texto o numero entero indicando la variable (columna)
  #     respuesta en data. Esta variable debe ser de clase numerica. 
  # group = cadena de texto o numero entero indicando la variable de grupo en data. Debe 
  #     tener minimo dos categorias o niveles. Si no es un factor, se convierte
  #     en uno internamente, y la diferencia de medias se calcula como el 1er. nivel
  #     menos el 2do. nivel, igual que en el comando t.test. Si quiere controlar el 
  #     orden en el cual se realiza la diferencia debe asegurar el orden deseado de
  #     los niveles del factor desde el mismo data.
  # alternative = Cadena de texto indicando el tipo de hipotesis alternativa 
  #     cuando el nro. de grupos comparados es dos. Por defecto "two.sided".
  #     Igual que en t.test. Revisar la ayuda ?t.test. 
  #     No aplica si el nro. de grupos es 3 o mas.
  # conf = nivel de confianza para los intervalos de confianza para la media de 
  #     cada grupo.
  # var.equal = Logico. Indica se quiere considerar varianzas iguales para el calculo
  #     de los intervalos de confianza de las medias de cada grupo y para realizar la
  #     prueba T en caso de que el nro. de grupos sea dos. Si TRUE, se obtiene la
  #     varianza ponderada y en el resultado se muestra la desviacion estandar ponderada
  #     Ademas, los errores estandar e IC de cada media se obtienen con esta varianza ponderada.
  #     Cuando hay tres o mas grupos, su valor (T o F) no afecta la ejecucion de 
  #     la prueba de hipotesis, ya que se realiza un analisis de varianza y una prueba F
  #     que asume homogeneidad de varianzas.
  # null.dif = La diferencia de medias nula (en H0) cuando los grupos comparados son dos.
  
  # INICIO DEL CODIGO ---
  # Validacion sobre argumento data:
  if(!is.data.frame(data)) stop("'data' argument should be a 'data.frame' or 'tibble'")
  if(ncol(data) == 1) stop("'data' should have two or more columns")
  
  # Se convierte a data.frame el valor en data:
  data <- as.data.frame(data)  # por si data es un tibble
  
  # Validacion sobre nombres de las columnas:
  if(is.character(response)){
    if(!response %in% names(data))  stop("'response' variable should be in 'data'")
  }
  if(is.character(group)){
    if(!group %in% names(data))  stop("'group' variable should be in 'data'")
  }
  if(is.numeric(response)){
    if(response > ncol(data))  stop(paste0("'data' has ", ncol(data), " columns"))
  }
  if(is.numeric(group)){
    if(group > ncol(data))  stop(paste0("data has ", ncol(data), " columns"))
  }
  
  # Se extraen cantidades relevantes:
  y <- data[, response]
  g0 <- data[, group]
  g <- if(is.factor(g0)) g0 else factor(g0)
  lev.g <- levels(g)
  k <- length(lev.g)
  alpha <- 1 - conf
  
  # Otras validaciones:
  if(k < 2)             stop("the number of groups should greater or equal than 2")
  if(any(table(g) < 2)) stop("sample size should greater or equal than 2 in all groups")
  if(!is.numeric(y))    stop("'response' variable should numeric class")
  
  # Calculando cantidades basicas:
  n <- as.numeric(tapply(X = y, INDEX = g, FUN = length))
  m <- as.numeric(tapply(X = y, INDEX = g, FUN = mean))
  v <- as.numeric(tapply(X = y, INDEX = g, FUN = var))
  s <- sqrt(v)
  tc <- qt(p = 1 - alpha/2, df = n -1)
  
  if(var.equal){
      vp  <- weighted.mean(x = v, w = n - 1)   # var weighted
      vp  <- rep(vp, k)                         
      sp  <- sqrt(vp)                          # sd weighted
      eemp <- sp / sqrt(n)
      L1 <- m - tc*eemp
      L2 <- m + tc*eemp
  } else{
    eem <- s / sqrt(n)
    L1 <- m - tc*eem
    L2 <- m + tc*eem
  }

  # Resultados:
  # Tabla de medias:
  means = data.frame(
      group = lev.g,
      n = n,
      mean = m,
      sd = s
    )
  if(var.equal){
    means$sdw     <- sp
    means$se      <- eemp
    means$ic.low  <- L1
    means$ic.up   <- L2
    means$ic.conf <- conf
  } else {
    means$se      <- eem
    means$ic.low  <- L1
    means$ic.up   <- L2
    means$ic.conf <- conf
  }
  
  # Resultado de la prueba de hipotesis:
  if(k == 2){
  
  # Validacion de la escritura de H1:
  h1s <- c('two.sided', 'less', 'greater')
  if(!alternative[1] %in% h1s)  {
    stop("'alternative' argument should be one of 'two.sided', 'less', 'greater'")
  }
  
  # Prueba T:
  rtest <- t.test(y ~ g, alternative = alternative[1],
                  var.equal = var.equal, mu = null.dif)
  dif.means <- m[1] - m[2]  # diferencia de medias
  eedm <- as.numeric(dif.means / rtest$stat)   # error estandar de la dif. de medias
  
  # Texto para H1:
  h1 <- paste0(
    'mu_1 - mu_2 ', 
    switch(rtest$alter, two.sided = '=! ', less = '< ', greater = '> '),
    null.dif
    )
  
  # Se guarda los resultados en un data.frame:
  test = data.frame(
      means.dif = dif.means,
      se.dm = eedm,
      null.dif = null.dif,
      t.stat = as.numeric(rtest$stat),
      df = as.numeric(rtest$par),
      p.value = as.numeric(rtest$p.value),
      side.H1 = rtest$alter,
      H1 = h1,
      method = ifelse(var.equal, 'Classic_t_test', 'Welch_t_test' )
    )
  }
  
  if(k > 2){
    r.aov <- aov(y ~ g)
    sst <- sum(summary(r.aov)[[1]][, 2])
    ssg <- summary(r.aov)[[1]][1, 2]
    f.stat    <- summary(r.aov)[[1]][1, 4]
    f.p.value <- summary(r.aov)[[1]][1, 5]
    f.df1     <- summary(r.aov)[[1]][1, 1]
    f.df2     <- summary(r.aov)[[1]][2, 1]
    test = data.frame(
      R2     = ssg / sst,
      F.stat = f.stat,
      df1 = f.df1,
      df2 = f.df2,
      p.value = f.p.value
    )
  }
  
  # Impresion del resultado:
  if(k == 2){
    list(
      means = means, t_test = test
    )
  } else{
    list(
      means = means, F_test = test
    )    
  }
  
  # FIN DEL CODIGO -----
  
  # VALOR: ¿QUE ENTREGA LA FUNCION? -----
  # Se entrega una lista con dos data.frame: 
  #
  # - means y t_test si el nro. de grupos comparados es dos
  # - means y F_test si el nro. de grupos comparados es tres o mas
  
  # Enseguida una descripcion del contenido de cada data.frame:
  #
  # means: data.frame con informacion de medias por grupo.
  #   n = tamano de muestra; mean = media; sd = desv. estandar
  #   sdw = desv. estandar ponderada en caso de que var.equal = T
  #   se  = error estandar de la media. Si var.equal = T, calculado usando
  #       la varianza ponderada.
  #   ic.low = limite inferior de un IC de conf*100% para media pob
  #   ic.up  = limite superior de un IC de conf*100% para media pob
  #   conf = confianza con la cual el IC fue calculado.
  
  # t_test: data.frame con resultado de prueba T en caso de que el nro.
  #   de grupos sea dos.
  #   means.dif = diferencia de medias en el sentido 1ero. menos 2do. nivel.
  #   se.dm = error estandar de la dif. de medias
  #   null.dif = valor nulo usado para la diferencia de medias (en H0)
  #   t.stat = valor del estadistico de la prueba
  #   df = grados de libertad. Depende de si var.equal = T o F.
  #   p.value = valor p de la prueba de acuerdo a la alternative solicitada
  #   side.H1 = Palabra clave que indica si la prueba es a dos colas, cola izq o 
  #      cola der.
  #   H1 = Hipotesis alterantiva solicitada
  #   method = tipo de preuba T utilizada. Depeden de var.equal.
  
  # F_test: data.frame con resultado de prueba F (anova) en caso de que el nro.
  #   de grupos sea mayor a dos.
  #   R2 = Coef. de Determinacion. Suma de cuadrados entre grupos dividido la 
  #       suma de cuadrados del total.
  #   F.stat = Valor del estadistico de la prueba. Razon de la varianza entre
  #      grupos y la varianza dentro de grupos.
  #   df1 = grados de libertad del numerador (entre grupos)
  #   df2 = grados de libertad del denominador (dentro grupos)
  #   p.value = valor p de la prueba F.
  
  # EJEMPLOS -----
  # Para mayor a dos grupos:
  # str(PlantGrowth) # observe la estructura del data.frame
  # mean_group(PlantGrowth, response = 'weight', group = 'group',
  #           var.equal = TRUE, conf = 0.90)
  
  # Para dos grupos
  # Se simulan datos:
  # x <- rep(0:1, each = 5)
  # d <- data.frame(
  #  x = x,
  #  y = rnorm(10, mean = 10 + 3*x, sd = 2)
  # )
  # 
  # Se aplica la funcion:
  # means_group(d, response = 'y', group = 'x', 
  #             var.equal = T, alternative = 'less')
  #
  # Usando enteros para seleccionar las columnas:
  # means_group(d, response = 2, group = 1,
  #             var.equal = F, alternative = 'less')

}