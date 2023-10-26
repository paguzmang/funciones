RtoS <- function(R, s){
  # 5-mar-2020
  # Funcion para convertir una matriz de correlacion en una de covarianza
  # a partir de un vector de varianzas
  # paguzmang
  # R = matriz de correlacion
  # s = vector de desviaciones estandar. Se debe cumplir que nrow(R) == length(s)
  
  # Codigo
  Dsigma <- diag(s)
  Sigma  <- Dsigma %*% R %*% Dsigma
  Sigma
}
