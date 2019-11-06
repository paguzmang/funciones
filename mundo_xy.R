mundo_xy <- function(x, col.pt = 'black', cex.pt = 0.6, pch.pt = 1, 
                     ylim = c(-80,80), xlim = c(-200,200),
                     br.long = seq(-150,150,50),
                     br.lat = seq(-50,50,25), col.mundo = 'brown'){
  # paguzmang | nov 2019
  # Descripcion:
  # Funcion que dibuja puntos sobre un mapa del mundo de baja resolucion tomado
  # de la libreria maps. Se debe tener instalado maps.
  
  # Argumentos:
  # x = data.frame con minimo dos vectores (columnas), el primero: longitud y el
  #     segundo: latitud. El data.frame puede tener otras columnas tipo factor o 
  #     o character para diferenciar los puntos por grupo.
  # col.pt, cex.pt, pch.pt = color, tamano y tipo del punto para agregarse. 
  # ylim, xlim = limites del eje X (longitud) y Y (latitud)
  # br.long, br.lat = valores de longitud y latitud donde se trazaran lineas de 
  # de referencia en el mapa construyendo una cuadricula
  # col.mundo = color para el borde del mapa del mundo.
  # 
  require(maps)
  # se dibuja un mapa de baja resolucion del mundo
  m <- map('world', interior = F, plot = F)  
  op <- par(no.readonly = TRUE)
  par(mar = c(3.5, 3.5, 1,1), mgp = c(2,1,0))
  plot(x = mean(xlim), y = mean(ylim), 
       xlim = xlim, ylim = ylim, type = 'n',
       xlab = 'Longitud', ylab = 'Latitud')
  abline(h = br.lat, v = br.long, lty = 3, col = 'grey60')
  lines(m, col = col.mundo)
  points(x, pch = pch.pt, col = col.pt, cex = cex.pt)
  par(op)
}