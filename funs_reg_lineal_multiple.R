# 30-Sep-2018
# Archivo con cinco funciones para aplicar en el contexto de Reg Lineal multiple
# r2, pred.org y graficos.diag sirve para reg lineal simple
#
anova_global <- function(x){
  # anova global desde un objeto lm
  # x = objeto lm
  
  # Calculos requeridos:
  y <- x$model[,1]        # var respuesta
  n <- length(y)          # nro. de obs
  df.tot <- n - 1
  df.error <- df.residual(x)
  df.reg <- df.tot - df.error  #  = p = nro. de terminos (o parametros) en el modelo (menos el intercepto)  
  SST <- var(y)*(n-1)
  SSE <- deviance(x)
  SSReg <- SST - SSE
  MSReg <- SSReg/df.reg
  MSE <- SSE / df.error
  Fcal <- MSReg / MSE
  valor.p <- pf(q = Fcal, df1 = df.reg, df2 = df.error, lower.tail = F)
  
  # Se genera la tabla:
  d <- data.frame(
    SS = round(c(SSReg, SSE, SST),4),
    df = c(df.reg, df.error, df.tot),
    MS = round(c(MSReg, MSE, NA),4),
    Fcal = c(round(Fcal,2), NA, NA),
    P = c(ifelse(valor.p < 0.001, '< 0.001', round(valor.p,4)), NA, NA),
    stringsAsFactors = F
  )
  rownames(d) <- c('Regresion', 'Residual', 'Total')  # Se asignan nombres de fila
  
  # Impresion del resultado
  d
}

# 
coef.imp <- function(x, d = 3, b.est2 = F, colin = F){
  # Funcion que calcula diferentes estadisticos que evaluan
  # la importancia de los predictores o terminos en un objeto lm y ademas
  # calcula dos medidas para evaluar la colinealidad: tolerancia
  # y factor de inflacion de varianza (vif)
  
  # Argumentos:
  # x = objeto lm con minimo dos predictores. Se permiten terminos de interaccion. Se debe
  #     incluir el intercepto.
  # d = nro. de decimales para la impresion de resultados
  # colin = Valor logico indicando si se imprimen medidas de colinealidad (tolerancia)
  #         y factor de inflacion de varianza (vif). Por defecto: no (FALSE).
  # b.est2 = Valor logico indicando si se imprime una 2da. medida de coef. estandarizado
  #          con base en la formula de Quinn y Keough ec: 6.16 que modifica la formula
  #          para calcular la desv. estandar de las X's. Por defecto: no (FALSE).
  
  # Codigo:
  X <- model.matrix(formula(x), data = x$model)
  p <- ncol(X) - 1   # nro. de terminos = nro. predictores (sin intercepto)
  int <- colnames(X)[1] == '(Intercept)'  # debe existir un intercepto
  
  if(p >= 2 & int){  # minino 2 predictores e intercepto
    Sx <- apply(X[,-1], 2, sd)
    Sy <- sd(x$model[, 1])
    b <- coef(x)[-1]
    n <- nrow(X)
    vif <- diag(solve(cor(X[,-1])))  # tomado de Bocard, Gillet & Legendre (2018) Numerical ecology with R pag: 233
    tol <- 1/vif   #  tomado de Quinn y Keough, texto, pag: 128. vif es inverso de tolerancia
    names(b) <- names(Sx) <- names(vif) <- names(tol) <- NULL
    Sx.ajust <- Sx/sqrt(vif)*sqrt((n-1)/(n-p)) # tomado de Quinn y Keough ec: 6.16
    s <- summary(x)$coef[, ]
    nom <- rownames(s)
    rownames(s) <- NULL
    colnames(s) <- NULL
    
    # Calculo de r2.parcial (de acuerdo a formula 6.14 de Quinn, pag: 123)
    dX <- as.data.frame(X[,-1])   # data.frame con predictores incluyendo interacciones
    SSR.f <- deviance(x)
    r2.parcial <- rep(NA, ncol(dX))
    for(i in 1:p){
      SSR.r <- deviance( lm(x$model[,1] ~  .  , data = as.data.frame(dX[, -i])) )
      SSR.drop <- SSR.r - SSR.f
      r2.parcial[i] <- SSR.drop/SSR.r
    }
    
    # Se guardan calculos:
    res <- data.frame(
      X = nom,
      b = round(c(s[1,1], b), d),               # coef. de regresion en su escala original
      ee_b = round(s[, 2],d+1),                 # error estandar de coef. de reg
      t = round(s[, 3],3),                      # estadistico t para prueba parcial tipo II o III
      valor.p = round(s[, 4] , 4),              # valor p de la prueba t
      tol = round(c(NA,tol), d),                # Tolerancia (para colinealidad)
      vif = round(c(NA,vif), d),                # VIF (para colinealidad)
      b.est = round(c(0,b*Sx/Sy), d),           # coef. estandarizado, tomado de Quinn y Keough ec: 6.15
      b.est2 = round(c(0,b*Sx.ajust/Sy), d),    # coef. estandarizado, tomado de Quinn y Keough ec: 6.15 y 6.16
      r2.parcial = round(c(NA, r2.parcial), d)  # R2 parcial, tomado de Quinn y Keough ec: 6.14
      )
    
    # Se imprime el resultado, de acuerdo a la solicitud en colin y b.est2
    i <- c(rep(T,5), colin, colin, T, b.est2, T)
    res[, i]
  } else{
    'Se requieren minimo dos predictores y el intercepto'
  }
}

# 
r2 <- function(x){
  # Funcion que calcula el r2 (coef. de determinacion) desde
  # un objeto lm.
  
  # Argumentos:
  # x = objeto lm
  
  # Codigo e impresion del resultado
  n <- nrow(x$model)
  SSE <- deviance(x)
  SST <- var(x$model[,1])*(n-1)
  1- SSE/SST
}

mse <- function(x){
  # Funcion que calcula el mse (error cuadrado medio) desde
  # un objeto lm.
  
  # Argumentos:
  # x = objeto lm
  
  # Codigo e impresion del resultado
  n <- nrow(x$model)
  SSE <- deviance(x)
  dfe <- df.residual(x)
  SSE/dfe
}

# 
add_pred <- function(data, model, se = T, conf = 0.95, name_pred = "pred"){
  # Funcion que devuelve predicciones desde un objeto lm
  # organizadas en un data.frame, junto con error estandar
  # e IC, y ademas, el resultado tambien devuleve los valores de las
  # X's. Entrega salida conveniente por ejemplo para usar en comandos
  # de graficos.
  
  # Argumentos:
  # data = data.frame con nuevos datos. Las columnas del data.frame
  #      deben tener los mismos nombres que las variables predictoras
  #      en el objeto lm. Pueden haber otras columnas.
  # model = objeto lm
  # se = Logico. Indica si se imprime error estandar de las predicciones. 
  #      Por defecto si (TRUE).
  # conf = confianza (entre 0 y 1) para el IC de la prediccion. Por defecto: 0.95. 
  #      Si NULL, no se entrega IC
  # name_pred = Cadena de texto con el nombre de la columna de las predicciones
  
  # Calculos
  w.conf <- is.null(conf) 
  yhat <- predict(
    object = model, newdata = data, se.fit = se, 
    interval = 'confidence', 
    level = ifelse(w.conf, 0.95, conf)
    )
  if(se){
    res <- cbind(data, pred = yhat$fit[,1], se = yhat$se.fit, 
                 L1 = yhat$fit[,2], 
                 L2 = yhat$fit[,3])
  } else{
    res <- cbind(data, pred = yhat[,1],  
                 L1 = yhat[,2], 
                 L2 = yhat[,3])
  }
  p <- ncol(data)
  k <- ncol(res)
  colnames(res)[p+1] <- name_pred
  
  # Se imprime
  if( w.conf ){
    res[, -((k-1):k)]
    } else{
    conf <- conf*100
    colnames(res)[(k-1):k] <- paste0("IC", conf, "_", c("L1", "L2")) 
    res  
    } 
  
}

#
graficos.diag <- function(x){
  # Funcion que realiza tres graficos de diagnostico desde un
  # objeto lm. Los graficos son:
  
  # Grafico 1:
  # residuales (estandarizados) vs. predicciones.
  # En este grafico tambien se marcan los dos residuales correspondientes
  # a las dos filas con mayor distancia de cook.
  
  # Grafico 2:
  # QQ Norm para residuales
  
  # Grafico 3:
  # Distancia de Cook vs. las filas. Se identifican las filas con las 
  # dos distancias de cook mas grandes.
  
  # Argumentos
  # x = objeto lm
  
  # Codigo:
  n <- nrow(x$model)
  yhat <- x$fit
  res <- rstandard(x)
  rango.r <- range(res)
  mr <- max(abs(rango.r))
  if(mr < 3) rango.r <- c(-3,3) else rango.r <- c(-ceiling(mr), ceiling(mr))
  dc <- cooks.distance(x)
  mdc <- max(dc)
  if(mdc < 1) rdc <- c(0,1.2) else rdc <- c(0, mdc*1.1)
  expre1 <- expression(hat(Y)[i])
  expre2 <- expression(paste(epsilon[i], '   vs   ', hat(Y)[i]))
  expre3 <- expression(paste(epsilon[i], '  ordenados'))
  orden.dc <- order(dc)[c(n-1, n)]

  # Graficos
  op <- par(no.readonly = TRUE)  # para luego reestablecer los parametros graficos que se modifican 
  # en la siguiente llamada a par
  par(mfrow = c(1,3), mar = c(3.5,3.5,3,0.5), mgp = c(1.7,0.6,0), cex = 0.8)
  
  # Grafico 1: Residuales vs. predicciones
  plot(x = yhat, y = res, ylim = rango.r, main = expre2,
       ylab = expression(epsilon[i]), xlab = expre1,
       xlim = range(yhat))
  abline(h = c(-2,0,2), lty = 2)
  text(y = res[orden.dc], x = yhat[orden.dc], labels = orden.dc, pos = 1,
       col = 'red')
  
  # Grafico 2: QQ norm para residuales
  qqnorm(y = res, ylim = rango.r, xlim = rango.r,
         main = 'Q-Q Normal para\nResiduales',
         xlab = 'Cuantiles teóricos', ylab = expre3)
  abline(a = 0, b = 1, lty = 2)
  
  # Grafico 3: Distancia de cook
  plot(x = 1:n, y = dc, type = 'h', lwd = 2, lend = 2,
       xlab = 'Observaciones', ylim = rdc,
       ylab = 'Distancia de Cook', 
       main = 'Distancia de Cook')
  abline(h = 1, lty = 2)
  text(x = orden.dc, y = dc[orden.dc], labels = orden.dc, pos = 3,
       col = 'red')
  
  # Se reestablecen los parametros graficos
  par(op)
}

estima_coef_lm <- function(m, conf.level = 0.95){
  
  # 27-sep-2024
  # Genera tabla con medidas de estimacion de los coeficientes de un modelo
  # Argumentos:
  # m = objeto lm
  # conf.level = Nivel de confianza para los intervalos de confianza
  
  # Codigo:
  tab <- as.data.frame(
    cbind(summary(m)$coef[, 1:2], 
          confint(m) )
  )
  tab <- cbind(coef = rownames(tab), tab)
  row.names(tab) <- NULL
  conf <- conf.level*100
  colnames(tab)[4:5] <- paste0("IC", conf, "_", c("L1", "L2")) 
  
  # Impresion:
  tab
  
}
