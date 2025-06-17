create_R_mat <- function(n_var = 2, r = 0, sigma = NULL){

  # 17-jun-2025
  # pguzman
  # Crea una matriz de correlaciones a partir del numero
  # de variables y de un valor de correlación que sería
  # único para toda la matriz. Opcionalmente, crea también
  # matriz de covarianzas si se entrega un valor de desviación 
  # estándar o un vector de valores de la misma longitud que
  # el numero de variables solicitado.
  
  
  # Codigo:
  U <- diag(x = 1, nrow = n_var, names = TRUE)
  U[lower.tri(U)] <- r
  U[upper.tri(U)] <- r
  if(!is.null(sigma)){
    if(length(sigma) == 1) sigma <- rep(sigma, n_var)
    if(length(sigma) != n_var) stop('length(sigma) debe ser igual a n_var') 
    Dsigma <- diag(sigma)
    Sigma  <- Dsigma %*% U %*% Dsigma
    list(R = U, S = Sigma)   # impresion
  } else{
    U   # impresion
  }
  
} 


