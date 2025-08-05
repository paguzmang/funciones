chisq_test_monteCarlo <- function(x, fixed_total = c(1,2,3)[1], 
                                  B = 2000, return_simul = TRUE, 
                                  plot = FALSE, seed = rpois(n = 1, lambda = 2500) ){
  
  # 4-ago-2025
  # pguzmang
  # Funcion que realiza una prueba chi-cuadrado Monte Carlo desde una tabla de
  # contingencia de dos dimensiones. Esta version Monte Carlo simula B tablas de
  # frecuencia suponiendo que H0 es cierta y para cada ocasión calcula el X2.
  
  # ARGUMENTOS:
  # x = matriz de frec. absolutas nr filas x nc columnas
  # fixed_total = Cual es el margen de la tabla que quiere que se considere como
  #     totales para realizar la simulacion. 1 = total de filas; 2 = total de columnas
  #     3 = gran total
  # B = numero de réplicas generadas 
  # return_simul = Logico. Si TRUE en el objeto resultante quedan guarados los X2 simulados
  # plot = Logico, si TRUE, se realiza igualmente un histograma.
  
  # CALCULOS:
  nr <- nrow(x)
  nc <- ncol(x)
  res.test <- suppressWarnings(chisq.test(x = x, correct = F))
  switch (fixed_total,
          
          # Si el total es el de las filas
          {
            total <- rowSums(x)
            pH0 <- prop.table(res.test$expected, margin = fixed_total)
            set.seed(seed)
            X2.simul <- replicate(
              n = B,
              expr = {
                x.simul <- matrix(data = NA, nrow = nr, ncol = nc)
                for(i in 1:nr){
                  x.simul[i, ] <- t(rmultinom(n = 1, size = total[i], prob = pH0[i,]))
                }
                unname(suppressWarnings(chisq.test(x = x.simul))$stat)
              }
            )
          },
          
          # Si el total es el de las columnas
          {
            total <- colSums(x)
            pH0 <- prop.table(res.test$expected, margin = fixed_total)
            set.seed(seed)
            X2.simul <- replicate(
              n = B,
              expr = {
                x.simul <- matrix(data = NA, nrow = nr, ncol = nc)
                for(i in 1:nc){
                  x.simul[, i] <- t(rmultinom(n = 1, size = total[i], prob = pH0[,i]))
                }
                unname(suppressWarnings(chisq.test(x = x.simul))$stat)
              }
            )
          },
          
          # Si el total es el gran total
          {
            total <- sum(x)
            pH0 <- prop.table(res.test$expected)
            set.seed(seed)
            X2.simul <- replicate(
              n = B,
              expr = {
                x.simul <- rmultinom(n = 1, size = total, prob = pH0)
                x.simul <- matrix(x.simul, nrow = nr, ncol = nc)
                unname(suppressWarnings(chisq.test(x = x.simul))$stat)
              }
            )
          }
  )
  
  # Resultado
  p.value <- mean(X2.simul >= res.test$statistic)
  
  # GRAFICO:
  if(plot){
    tit <- paste0('X2.simul\n', ifelse(p.value < 0.001, 'p value < 0.001', paste0('p value = ', p.value)))
    hist(X2.simul, main = tit, xlim = c(0, res.test$statistic*1.05))
    abline(v = res.test$statistic, col = 'red', lty = 2)
  }
  
  # IMPRESION:
  r <- list(stat = data.frame(
    X2.obs = unname(res.test$stat), 
    df = unname(res.test$par),
    p_value_chisq.test = res.test$p.value, 
    B = B,
    p_value_simul = p.value),
    X2.simul = X2.simul
  )
  if(return_simul) r else r[[1]]
}

