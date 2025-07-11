fechas_curso_semestre <- function(f1, d, n_sem = 18, k = 4, 
                                  inicio_eval = 4, dur_clase = 2,
                                  res_excel = FALSE, 
                                  nombre_archivo_excel = 'fechas_curso.xlsx'){
  
  # 11-jul-2025
  # pguzmang
  # Funcion que entrega las fechas de clases de un curso que tendra dos 
  # encuentros en dos dias de la semana, separados d dias. 
  
  # La funcion entrega dos tablas, una en formato ancho, una fila por semna
  # y otra en formato largo, una fila por dia de clase.
  
  # Argumentos:
  # f1 = Fecha de inicio o de la primera clase, como una cadena de texto
  #     en formato 'yyyy-mm-dd'. La funcion internamente convierte esto en clase fecha
  # d = numero de dias hasta la proxima clase dentro de la misma semana.
  #     ejemplo, si las clases son martes, jueves, entoces d = 2
  # n_sem = numero de semanas totales al semestre.
  # k = numero de evaluaciones a realizarse durante el semestre
  # inicio_eval = numero de la semana para la primera evaluacion. La funcion
  #    reparte las k evaluaciones entre las n_sem semanas, mas o menos igualmente
  #    espaciadas a partir de inicio_eval y seguramente, finalizando el ultimo
  #    dia de clase para la ultimo eval.
  # dur_clase = horas que dura cada clase. Esto aparecera como una columna en la 
  #    tabla. Se supone que cada clase tiene la misma duracion.
  # res_excel = Si TRUE, se genera archivo de excel con las tablas resultantes
  # nombre_archivo_excel = Cadena de texto con el nombre del archivo de excel
  #    en caso de que res_excel sea TRUE
  
  # Librerias
  require(tidyr)
  require(dplyr)
  require(openxlsx)
  require(lubridate)
  
  # Funcion que reparte k eventos en n semanas, controlando
  # la semana para el primer evento (chatGPT)
  repartir_eventos <- function(n, k, inicio = 1) {
    if (k > (n - inicio + 1)) stop("No hay suficientes semanas desde 'inicio' para ubicar los eventos.")
    if (inicio < 1 || inicio > n) stop("'inicio' debe estar entre 1 y n.")
    
    # Calcular las posiciones igualmente espaciadas desde 'inicio' hasta 'n'
    posiciones <- round(seq(inicio, n, length.out = k))
    
    # Crear vector con "a"
    semanas <- rep("a", n)
    
    # Asignar eventos en las posiciones calculadas
    for (i in seq_along(posiciones)) {
      semanas[posiciones[i]] <- paste0("e", i)
    }
    
    return(semanas)
  }
  
  # Calculos:
  semana <- 1:n_sem
  f1t <- seq(as.Date(f1), by = 7, length.out = n_sem)
  f2t <- f1t + d
  tab <- data.frame(semana, d.1 = f1t, d.2 = f2t, 
                    eval = repartir_eventos(
                      n = n_sem, k = k, inicio = inicio_eval)
  )
  tab.e <- tab |> select(d.2, eval) |> rename(fecha = d.2)
  tabf <- pivot_longer(
    tab[, -c(4,5)], cols = c("d.1", "d.2"), names_to = 'dia', 
    names_prefix = 'd.', values_to = 'fecha'
  )
  tabf <- dplyr::left_join(tabf, tab.e, by = "fecha")
  tabf$eval[is.na(tabf$eval)] <- "a"
  tabf <- mutate(tabf,
    clase = 1:n(),
    dur   = dur_clase,
    mes   = month(fecha),
    dia_cal   = day(fecha)
    )
  
  # Se guardan las dos tablas principales:
  res <- list(tab.ancho = tab, tab.largo = tabf)
  
  # Impresion:
  if(res_excel){
    write.xlsx(res, file = nombre_archivo_excel)
  } else{
    res
  }
  
  
}

