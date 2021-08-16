simul_ab_sp <- function(sp = 12, ab = NULL, N = 100, m = 3, Nm = NULL, 
                        alfa = 0.5, res.ab0 = F,  
                        res.formato = c("ancho", "largo.ind", "largo.ab")){
  
  # paguzmang
  # 11-mar-2021
  # Funcion que simula una muestra de individuos con los nombres de especies 
  # y abundancias por puntos de muestreo. Existen varias opciones de
  # personalizacion de la simulacion en cuanto a la probabilidad de seleccion
  # cantidad de individuos total y por sitio y formato de entrega de los
  # resultados. No tiene ningun argumento obligatorio. Con los valores
  # por defecto se simula una muestra de 100 individuos en total repartidas
  # en tres puntos de muestreo y una riqueza maxima de 12 especies. Los resultados
  # seran entregados en formato ancho y aquellas especies con abundancia de 0
  # no apareceran en la tabla resultante.
  
  # Argumentos:
  # sp = vector de tipo "character" con los nombres de las especies (de forma unica)
  #      Tambien puede ser un solo numero entero indicando la riqueza (=numero de especies) 
  #      maxima deseada. Si se usa esta 2da. opcion (por defecto para 12 especies) 
  #      los nombres de las especies se conforman como: paste0("sp", 1:sp)
  # ab = vector con abundancias por especie (en el mismo orden que sp).
  #      Las abundancias pueden ser absolutas o relativas. Ver detalles
  #      para mas informacion. Si se deja como NULL (por defecto), estas abundancias
  #      seran simuladas. Ver argumento "alfa".
  # N  = Un solo numero entero representando la cantidad total de individuos 
  #      (abundancia total) a simular
  # m  = Un solo numero entero representado el numero de puntos de muestreo 
  #      o de muestras. Podria ser incluso m = 1 para simular un solo punto
  #      muestreo o una sola muestra.
  # Nm = Vector de numeros enteros representando cada numero
  #      la cantidad total de individuos por punto de muestreo. 
  #      P.e., si se provee un vector con tres numeros (30, 20, 26), se asumen
  #      tres puntos de muestreo en los cuales se colectaron respectivamente
  #      30, 20 y 26 individuos. Ver detalles para mas informacion.
  # alfa = Numero entreo 0 y 1 indicando el parametro "prob" del comando
  #      vcdExtra::rlogseries para simular el vector de abundancias ("ab") por especie
  #       Esto aplica en el caso de que "ab" sea NULL.
  # res.ab0 = Logico. Si TRUE (por defecto) se incluyen especies con 
  #      abundancia de cero al considerar la cantidad total (N) de individuso seleccionados
  # res.formato = Una sola cadena de texto indicando el tipo de formato para la
  #     tabla resultante. Las opciones son las siguientes:
  #         "ancho": (por defecto) se entrengan los datos en formato ancho
  #                  donde cada columna es una punto de muestreo y cada fila
  #                  es una especie. Los numeros indican la abundancia (cantidad de individuos)
  #                  de cada especie en cada punto de muestreo.
  #         "largo.ind": se entregan los datos en formato largo, donde
  #                  cada fila es un registro de la ocurrencia de un individuo con su especie
  #                  y el punto de muestreo donde fue observado. 
  #                  Bajo este formato, se requiere CONTAR las filas por especie para calcular
  #                  la abundancia por especie y por punto de muestreo si se quiere.
  #                  Si res.ab0 = TRUE, la columna de especie sera un factor incluyendo los
  #                  niveles (especies) que no fueron seleccionados. Al realizar una tabla de
  #                  frecuencias por especie, saldran estos niveles (especies) con conteos de 0.
  #         "largo.ab": datos en formato largo, donde cada fila es una combinacion de 
  #                  especie y punto de muestreo, y se agrega una columna adiciona que indica
  #                  la abudancia de la especie en dicho punto.
  #                  Si res.ab0 = TRUE, la columna de especie sera un factor incluyendo los
  #                  niveles (especies) que no fueron seleccionados. Estos niveles (especies)
  #                  tendran 0 en la columna de abundancia.
  
  # Detalles:
  
  # El vector "sp" tiene los nombres de las especies cuyas abundancias
  # queremos simular. La longitud de este vector sera la riqueza maxima
  # que podremos obtener en la simulacion. Tener presente que en la
  # simulacion algunas de estas especies (sobre todo aquellas con baja
  # probabilidad) pueden no salir nunca y estar del todo ausentes en
  # la muestra total de individuos generada. Entre mayor se N, mas opcion
  # tendran estas especies "raras" de salir al menos con un solo individuo.
  
  # El vector "ab" es utilizado para conocer la probabilidad 
  # que tiene cada especie de ser seleccionada y no afecta las
  # cantidades "N", "m" o "Nm". Este vector puede ser tomado desde
  # un estudio ya realizado, o puede simularse usando una distribucion
  # tal como la logseries. El comando vcdExtra::rlogseries genera
  # un vector de abundancias por especie bajo el modelo logseries.
  # El paquete SAD tiene comandos para este mismo y otros modelos.
  # Si "ab" se deja NULL, el vector sera simulado usando
  # vcdExtra::rlogseries con prob = alfa. Ver ayuda de este comando
  # para mas informacion.
  
  # El argumento "m" (numero de puntos de muestreo) refleja la idea
  # de que para explorar una zona o area de estudio, usualmente
  # seleccionamos mas de un punto de muestreo para tomar una muestra
  # de individuos. Esta funcion asume que la probabilidad de
  # seleccionar cada especie (dada por vector "ab") es la misma para
  # todos los "m" puntos de muestreo.
  
  # Si "Nm" es NULL (por defecto), los argumentos "N" y "m" son 
  # obligatorios. En este caso, el "N" se reparte por "igual"
  # entre los "m" puntos de muestreo usando una distribucion
  # multinomial.
  
  # Si "Nm" se utiliza (diferente de NULL), cualquier valor asignado 
  # a "N" y "m" no aplicara. Todo sera tomado desde "Nm", tanto la 
  # cantidad total de individuos colectados como el numero de puntos
  # muestreo.
  
  # Librerias
  require(dplyr)
  require(tidyr)
  require(tibble)
  
  # Codigo:
  # Parametrizacion de Nm y m:
  if(is.null(Nm)){
    Nm <- rmultinom(n = 1, size = N, prob = rep(1/m,m) )[,1]
  } else{
    m <- length(Nm)
  }

  # Validacion de algunos argumentos:
  # res.formato:
  if(!res.formato[1] %in% c("ancho", "largo.ind", "largo.ab")) 
    stop("El argumento 'res.formato' debe ser uno de 'ancho', 'largo.ind' o 'largo.ab'")
  
  # sp cuando es numero:
  sp.num <- is.numeric(sp) | is.integer(sp)
  if(sp.num)   sp.num <- length(sp) == 1
  if(sp.num)   sp.num <- sp > 1

  # sp cuando es texto
  sp.tex <- is.character(sp) | is.factor(sp)
  if(sp.tex) sp.tex <- length(sp) > 1
  
  if(sp.num + sp.tex != 1)
    stop("El argumento 'sp' debe ser un solo entero mayor que 1 o un vector\n  de dos o mas nombres de especies")
  
  # Simulacion y organizacion de datos en formato "largo.ind":
  if(sp.num) sp <- paste0("sp", 1:sp)
  if(is.null(ab)) {
    r <- length(sp)
    require(vcdExtra)
    ab <- rlogseries(n = r, prob = alfa)
  }
  if(length(sp) != length(ab)) stop("La cantidad de especies ('sp') debe ser igual la longitud del vector 'ab'")
  fm <- function(x) sample(x = sp, size = x, replace = T, prob = ab)
  m.puntos <- sapply(Nm, fm)
  datos <- data.frame(
    m  = rep(1:m, times = Nm),
    esp = unlist(m.puntos)
  )
  
  # Se imprimen los resultados de acuerdo a los argumentos res.ab0 y res.formato
  switch(res.formato[1],
         
    # Para formato "ancho":
    ancho = {
      d1 <- datos %>%
        mutate(
          esp = factor(esp, levels = sp)
        ) %>% 
        xtabs(~ esp + m, data = .) %>%
        as.data.frame() %>%
        pivot_wider(names_from = "m", values_from = "Freq", 
                    names_prefix = "m", values_fill = 0)
      if(!res.ab0){
        i <- rowSums(d1[, -1]) > 0   # cuales filas tienen abundancia > 0
        d1 <- d1[i, ]
      }
      d1
    },
    
    # Para formato "largo.ind":
    largo.ind = {
      if(res.ab0) datos <- mutate(datos, esp = factor(esp, levels = sp))
      datos
    },
    
    # Para formato "largo.ab":
    largo.ab = {
      if(res.ab0) datos <- mutate(datos, esp = factor(esp, levels = sp))
      d1 <- xtabs(~ esp + m, data = datos) %>%
        as.data.frame() %>%
        rename(abund = Freq)
      d1
    })
  
  # Ejemplos
  # Valor por defecto:
  # simul_ab_sp()

  # Simulan las abundancias de cuatro especies en dos puntos de muestreo
  # con una abundancia total de 30 individuos
  # simul_ab_sp(sp = 4, ab = c(30, 2, 2, 1), N = 30, m = 2, res.ab0 = T)
  # simul_ab_sp(sp = 4, ab = c(30, 2, 2, 1), N = 30, m = 2, res.ab0 = F)
  # simul_ab_sp(sp = 4, ab = c(30, 2, 2, 1), N = 30, m = 2, res.ab0 = F, res.formato = "largo.ind")
  # simul_ab_sp(sp = 4, ab = c(30, 2, 2, 1), N = 30, m = 2, res.ab0 = F, res.formato = "largo.ab")
  # simul_ab_sp(sp = 4, ab = c(30, 2, 2, 1), N = 30, m = 2, res.ab0 = T, res.formato = "largo.ab")
  
  # Se controla la cantidad de individuos totales por punto de muestreo
  # d <- simul_ab_sp(sp = 4, ab = c(30, 2,2,1), Nm = c(10, 20))
  # d
  # colSums(d[, -1])  # verifique la cantidad de individuos por punto de muestreo
  
  # Ejemplo tomando prestado datos de un estudio:
  # Supongo un estudio donde se observaron N = 42 individuos de murcielagos 
  # repartidos en cuatro especies:
  # sp <- c("Artibeus lituratus", "Dermanura tolteca", 
  #         "Chiroderma salvini", "Sturnira lilium")
  #
  # Con las siguientes abundancias relativas:
  # ab <- c(0.321, 0.143, 0.179, 0.357)
  #
  # Se desea simular una muestra de esa comunidad repartida
  # en tres puntos de muestreo
  # d <- simul_ab_sp(sp = sp, ab = ab, N = 42, m = 3, 
  #                  res.ab0 = T, res.formato = "ancho")
  #
  # Revision de la simulacion:
  # d
  # colSums(d[, -1])        # abundancia total por punto de muestreo
  # sum(colSums(d[, -1]))   # abundancia total
  # rowSums(d[, -1]) / 42   # abudancia total por especie
  # ab                      # compare con las prob. teoricas ingresadas
}

