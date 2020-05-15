# funcion covid_19

covid_19<- function(H, A, D = 1, nuevo = T
                    , grafico = F ){
  
  # Descripcion: Predice los nuevos casos de contagio del virus covid_19 y 
  # su tendencia
  
  
  # Argumentos:
  
  # H: Los casos del dia de hoy
  
  # A: Los casos del dia de ayer
  
  # D: El numero de dias en los
  #    que se desea ver la prediccion
  #    de contagiados
  
  # nuevo: Logico. Si nuevo es T se mostraran 
  #        los nuevos casos de contagio, si 
  #        nuevo es F se mostraran los casos 
  #        nuevos + los anteriores.
  
  # grafico: Logico. Si se desea un grafico 
  #          de la funcion se le asigna T al 
  #          argumento
  
  rD <- data.frame (Dia = rep(NA, D) ,FDR = rep(NA, D), 
                    APROX_Contagiados = rep(NA, D)) 
  
  if(nuevo){
    for(i in 1 : D){ 
      f<- H/A
      m<- H*f
      t<- m-H
      A<- H
      H<- m
      rD[i, ]<- c(i,f,t)
    }
  } 
  else{
    for(i in 1 : D){ 
      f<- H/A
      m<- H*f
      A<- H
      H<- m
      rD[i, ]<- c(i,f,m)
    }
  }
  if(grafico){ 
    
    plot(x = rD$Dia, y = rD$APROX_Contagiados, type = "l", 
         xlab = "Dias", ylab = "Contagiados", 
         main = "Contagiados COVID-19")
    
    
    points(x= rD$Dia, y = rD$APROX_Contagiados,
           col = "red", pch = 19 )
  }
  rD
}