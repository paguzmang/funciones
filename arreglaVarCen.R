arreglaVarCen <- function(x, texto.cen = '<', res.x = F, nom.x = 'x'){
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
  
  # Codigo:
  pos.cen       <- grep(pattern = texto.cen,  x = x)
  pos.cen.logic <- grepl(pattern = texto.cen, x = x)
  n <- length(pos.cen)
  y <- gsub(pattern = texto.cen, replacement = '', x = x)
  y <- gsub(pattern = ',', replacement = '.', x = y)
  x.num <- as.numeric(y)
  x.lim <- unique(x.num[pos.cen])
  tab.res <- as.data.frame(table(x.num[pos.cen]))
  x.num[pos.cen] <- x.num[pos.cen]/2   # reemplazo por la media
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
  # Entrega una lista con dos data.frame (datos y res.cen)
  
  # Ejemplo de uso:
  # x <- c(45.5, 34.5, '<5.5', '<  5.5', '<3.4', 32, 24)
  # arreglaVarCen(x = x)
  # arreglaVarCen(x = x, res.x = T)
}
