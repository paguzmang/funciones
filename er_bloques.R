er_bloques <- function(m1, m2){
  
  # Funcion para calcular al Eficiencia Relativa de
  # usar bloques.
  
  # Argumentos:
  # m1 = modelo con el DCA
  # m2 = modelo con el DBCA
  
  # Calculos:
  ssr1 <- deviance(m1)
  dfr1 <- df.residual(m1)
  msr1 <- ssr1 / dfr1
  
  ssr2 <- deviance(m2)
  dfr2 <- df.residual(m2)
  msr2 <- ssr2 / dfr2
  
  # ER:
  num <- (dfr2 + 1)/((dfr2+3)*msr2)
  denom <- (dfr1 + 1)/((dfr1+3)*msr1)
  er <- num/denom
  
  # Media general:
  xbar <- mean(m1$model[, 1])
  
  # Impresion:
  data.frame(
    modelo = c('m1_DCA', 'm2_DBCA'),
    CM_e = c(msr1, msr2),
    CV_e = round(c(sqrt(msr1)/xbar*100, sqrt(msr2)/xbar*100),2),
    Efic_rel  = round(c(NA, er), 3)
  )
}