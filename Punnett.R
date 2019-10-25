frec.punnett<-function(gen.p1,gen.p2){
  # Creacion: 24-oct-2019
  # Version: 1.0
  # Daniel Alejandro Rodriguez; Mateo Gonzalez Jimenez
  # Curso: Programacion
  #
  # Esta funcion entrega una tabla con los genotipos 
  # de F1 y sus frecuencias.
  #
  # Argumentos:
  # gen.p1 = genotipo del primer parental
  # gen.p2 = genotipo del segundo parental
  #
  # Codigo:
  kp1 <- nchar(gen.p1)
  kp2 <- nchar(gen.p2)
  p1v <- rep(NA, kp1)
  p2v <- rep(NA, kp2)
  for(i in 1:kp1) p1v[i] <- substr(gen.p1, start = i, stop = i)
  for(i in 1:kp2) p2v[i] <- substr(gen.p2, start = i, stop = i) 
  p1.gam <- expand.grid(x = p1v[1:2], y = p1v[3:4])
  p1.gam$xy <- paste0(p1.gam$x, p1.gam$y) 
  #Formacion de los gametos del parental 1
  p2.gam <- expand.grid(x = p2v[1:2], y = p2v[3:4])
  p2.gam$xy <- paste0(p2.gam$x, p2.gam$y)
  #Formacion de los gametos del parental 2
  f1 <- expand.grid(x = p1.gam$xy, y = p2.gam$xy)
  f1$xy <- paste0(f1$x, f1$y)
  #Formacion de los genotipos de la F1
  
  
  frec.Alelo <- function(x, letras = c('A', 'a', 'B', 'b')){
    k <- nchar(x)
    res <- rep(NA,k)
    for(i in 1:k) res[i] <- substr(x, i, i)
    y <- factor(res, levels = letras)
    tab <- table(y)
    res2 <- tab[1]
    for(i in 2:k) res2 <- paste0(res2, tab[i])
    res2
  }#Permite verificar cu?ntos de un mismo alelo hay en los genotipos de la F1
  
  
  f1$xyp <- sapply(f1$xy, FUN = frec.Alelo)
  f1$xyp  <- factor(f1$xyp)
  
  frec.unicas <- unique(f1$xyp)
  res <- as.data.frame(table(f1$xyp))
  res$gen <- NA
  for(i in 1:length(frec.unicas)){
    res$gen[i] <- subset(f1, xyp == res[i, 1])$xy[1]
  }#Serivira para contar los genotipos que sean iguales y agruparlos 
  # en los que comprendan la misma frecuencia.
  
  frec.genotipos <- res[ , c(3,2)]
  frec.genotipos 
  
  # Entrega los resultados como un
  # data.frame
  
  # Formas de uso:
  # con unos genotipos dados por el usuario siendo
  # los genotipos de un parental AaBb y del otro #parental AaBB
  # frec.punnett(gen.p1 = "AaBb",gen.p2 = "AaBB")
}