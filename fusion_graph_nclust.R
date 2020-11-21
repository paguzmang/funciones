fusion_graph_nclust <- function(hc, par = T){
  
  # paguzmang
  # 21-nov-2020
  
  # Descripcion:
  # Funcion para graficar el numero de grupos vs. la distancia 
  # fusion o formacion de cluster en el dendrograma. El grafico
  # util para identificar pasos largos en la distancia y decidir
  # una distancia de corte para definir nro. de grupos
  # Idea de Rencher & Christensen (2013) o Borcard et. al (2011)
  
  # Argumentos:
  # hc = objeto de clase hclust
  # use.par = Logico. Si TRUE, se usa par antes del grafico
  #      para ajustar margenes y tamano de letra en el grafico
  #      El par se resetea al final
  
  # Codigo:
  n <- length(hc$order)
  h <- hc$height
  r <- diff(range(h))
  x <- c(h[1]-r*0.1, h)
  y <- c(n, n:2)
  if(par) op <- par(mar = c(3.5, 3.5, 2, 1), mgp = c(2,1,0), cex = 0.9)
  plot(x = x, y = y, type = "S", col = "grey", 
       main = "Fusion levels", xlab = "Distance",
       ylab = "Number of groups",  yaxt = "n")
  axis(side = 2, at = 2:n, las = 1)
  text(x = x-r*0.02, y = y, labels = y, col = "red", 
       cex = 0.8)
  if(par) par(op)
}