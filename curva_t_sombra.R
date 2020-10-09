curva_t_sombra <- function(v, x0 = NULL, x1 = NULL, 
                           l1 = NULL, l2 = NULL, k = 3, nom.x = T,
                           nom.y = T, col = 'salmon', lwd = 2, 
                           use.par = T, reset.par = T, ...){
  
  # 1-oct-2020
  # pguzmang.
  # Funcion que realiza un grafico de la curva t con sombra para cierto
  # rango del eje X.
  
  # Argumentos:
  # v = grados de libertad
  # x0 = limite inferior de la sombra
  # x1 = limite superior de la sombra
  #   Nota: x0, x1 pueden ser un vector de uno o dos numeros si se quiere
  #         una sola sombra o dos sombras. Ver ejemplos. En x0 puede
  #         usar -Inf y en x1 puede ser Inf. Ver ejemplos.
  # l1 = limite inferior del eje x.
  # l2 = limite superior del eje x.
  #   Nota: Si NULL (por defecto) se ajustan automaticamente usando el factor k.
  # col = color para la sombra
  # lwd = grosor de la linea de la curva
  # k = multiplicador de la desv. estandar para encontrar los extremos
  #     del rango del eje x (l1 y l2). Por defecto k = 3.
  # nom.x = Logico. ¿quiere que se ponga de forma automatica un 
  #         nombre en el eje X?
  # use.par = Logico. Indica se quiere usar 'par' para ajustar margenes.
  # reset.par = Logico. Si quiere que el 'par' usado se 'resetee'
  # ... = Otros argumentos para curve diferentes a xlab, ylab, from, to y lwd.
  #     Util p.e. si quiere quitar el eje x (xaxt = 'n') y poner su propio eje 
  #     con el comando axis.
  
  # funcion para dibujar sombra de la T:
  sombra.t <- function(x0,x1,df = 10){
    x <- c(x0,seq(x0,x1,0.01),x1) 
    y <- c(0,dt(seq(x0,x1,0.01), df = df),0)
    list(x = x, y = y)
  }
  
  # Ajuste de los valores x0, x1, l1, l2 si son NULL (por defecto)
  de <- sqrt(v / (v-2))    # var[T_v] = v / (v -2)
  if(is.null(x0)) x0 <- 0 - de*k
  if(is.null(x1)) x1 <- 0 + de*k
  if(is.null(l1)) l1 <- 0 - de*k  # para min del rango del eje x
  if(is.null(l2)) l2 <- 0 + de*k  # para max del rango del eje x
  
  # Ajustes de x0 y x1 en caso de que haya un -Inf o un Inf:
  x0 <- ifelse(is.infinite(x0) & x0 < 0, -de*k, x0 ) 
  x1 <- ifelse(is.infinite(x1) & x1 > 0,  de*k, x1 )
  
  # Calculo de la sombra:
  una.sombra <- length(x0) == 1 & length(x1) == 1
  if(una.sombra) {
    som <- sombra.t(x0 = x0, x1 = x1, df = v)
  } else{
    som1 <- sombra.t(x0 = x0[1], x1 = x1[1], df = v)
    som2 <- sombra.t(x0 = x0[2], x1 = x1[2], df = v)
  }
  
  # Grafico:
  if(use.par){
  op <- par(mar = c(3.5, 3.5, 1,1), mgp = c(1.9,0.7,0), cex = 0.9)
  }
  # Se agrega curva sin linea (con amplitud 0)
  curve(dt(x, df = v), from = l1, to = l2, lwd = 0, 
        ylab = if(nom.y) "Densidad" else NA, 
        xlab = if(nom.x) bquote(T[.(v)]) else NA, ...)
  
  # Se agrega la sombra:
  if(una.sombra){
    polygon(som, border = FALSE, col = col)
  } else{
    polygon(som1, border = FALSE, col = col)
    polygon(som2, border = FALSE, col = col)
  }
  
  # Se agrega una curva encima
  curve(dt(x, df = v), from = l1, to = l2, lwd = lwd, add = T)
  if(use.par) { if(reset.par) par(op) }
  
  # Ejemplos:
  # curva_t_sombra(v = 4, x0 = c(2,-3),  x1 = c(3, -2) )
  # curva_t_sombra(v = 4, x0 = -3,  x1 = -2 )
  # curva_t_sombra(v = 4, x0 = -3*sqrt(4/2),  x1 = -2 )
  # curva_t_sombra(v = 4, x0 = -Inf,  x1 = -2 )  # lo mismo que la anterior
  # curva_t_sombra(v = 4, x0 = 2,  x1 = Inf )
  # curva_t_sombra(v = 4, x0 = c(2, -Inf),  x1 = c(Inf, -2) )
  # curva_t_sombra(v = 4, x0 = -2,  x1 = 2 )
  # curva_t_sombra(v = 4, x0 = -2,  x1 = 2, xaxt = 'n' )
  # curva_t_sombra(v = 4, x0 = -2,  x1 = 2, xaxt = 'n', nom.x = F )
  # curva_t_sombra(v = 4, x0 = -2,  x1 = 2, xaxt = 'n', nom.x = F, nom.y = F )
}


