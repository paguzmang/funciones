arreglaVarCen <- function(x, texto.cen = '<', res.x = F, nom.x = 'x', reemplazo = c('mitad',  'na')){
  #
  # Descripcion de la funcion
  # Toma un vector de texto en el cual algunas entradas estan
  # indicadas como censuradas a izquierda, por ejemplo con '<0.5',
  # y crea un vector numerico donde las entradas censuradas
  # se reemplazan con la mitad entre 0 y el limite. Entrega
  # informacion adicional.
  # Autor: Pablo Andres Guzman | U.CES.
  # Fecha: Sep 2019
  
  # Argumentos:
  # x = vector character con los datos a evaluar
  # texto.cen = character que indica la censura
  # res.x = Logico, ¿desea incluir en el data.frame resultante
  #         una columna con vector original?
  # nom.x = Nombre de la variable analizada para que
  #         salga etiquetada en el data.frame resultante
  # reemplazo = Un valor de texto indicando por que se quiere hacer el reemplazo, si por
  #   la 'mitad' del limite de deteccion (por defecto) o por 'na'.
  
  # Codigo:
  pos.cen       <- grep(pattern = texto.cen,  x = x)
  pos.cen.logic <- grepl(pattern = texto.cen, x = x)
  n <- length(pos.cen)
  y <- gsub(pattern = texto.cen, replacement = '', x = x)
  y <- gsub(pattern = ',', replacement = '.', x = y)
  x.num <- as.numeric(y)
  x.lim <- unique(x.num[pos.cen])
  tab.res <- as.data.frame(table(x.num[pos.cen]))
  if(!reemplazo[1] %in% c('mitad',  'na') ) stop("remplazo debe ser uno entre 'mitad' o 'na'")
  if(reemplazo[1] == 'mitad') x.num[pos.cen] <- x.num[pos.cen]/2 else  x.num[pos.cen] <- NA  # reemplazo por la media
  names(tab.res) <- c('lim', 'n_cen')
  tab.res$p_cen <- round(tab.res$n_cen/length(x)*100,2)
  tab.res$texto.cen = texto.cen
  
  # Se prepara el vector o data.frame resultante
  datos <- data.frame(
    x     = x,
    x.num = x.num,
    cen   = pos.cen.logic
  )
  names(datos) <- c(paste0(nom.x, '_orig'), nom.x, 
                    paste0(nom.x, '_cen'))
  if(!res.x) datos <- datos[, -1]
  
  # Impresion del resultado en una lista:
  list(
    datos   = datos,
    res.cen = tab.res
  )
  
  # Valor
  # Entrega una lista con dos data.frame (datos y res.cen).
  # datos = contiene la nueva columna numerica y una columna logica que indica si
  #   el dato estuvo por debajo del limite o no. Opcionalmente si res.x = T, entrega
  #   tambien el vector original.
  # res.cen = data.frame con resumen de lo encontrado con respecto a datos censurados
  #   o por debajo del limite.
  #   lim = limites de deteccion encontrados
  #   n_cen = nro. de entradas encontradas con el formato '<LD'
  #   p_cen = porcentaje de entradas encontradas con el formato '<LD'
  #   texto.cen = El texto o signo encontrado que marca si un dato esta por debajo del limite.
  
  # Ejemplo de uso:
  # w <- c(45.5, 34.5, '<5.5', '<  5.5', '<3.4', 32, 24)
  # arreglaVarCen(x = w)
  # arreglaVarCen(x = w, res.x = T)
  # arreglaVarCen(x = w, res.x = T, reemplazo = 'na')
  # arreglaVarCen(x = w, res.x = T, reemplazo = 'nas')   # detiene y saca mensaje informativo
  # arreglaVarCen(x = w, res.x = T, reemplazo = 'na', nom.x = 'NO2')   
}