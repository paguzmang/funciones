# Version: 1.0 | Autor: Valeria Arce
# Fecha: 14/5/2020
# Descripción de la función:

# La función MM calcula los valores para Vmax
# y Km en una reacción enzimatica con y sin inhibidor 
# a partir de los datos dados.

MM<- function(without, with, S, plot=F) {
  # Argumentos:
  
  # without = vector con distintas velocidades de la reaccion sin el inhibidor
  
  # with= vector con distintas velocidades de la reaccion con el inhibidor
  
  # S= vector con concentraciones de sustrato en distintos momentos
  
  # plot= se indica T o F si se quiere o no graficar los resultados
  
  # Codigo:
  
  datos<-data.frame(S,without,with) # se hace un dataframe con los datos dados
  
  # Se utiliza el comando drm para adecuar los datos a la ecuación de Michaelis-Menten:
  sin<-drm(without~ S, fct = MM.2())  
  con <-drm(with~S,fct=MM.2())
  
  # Se crean dos dataframes para organizar los valores de Km y Vmax con y sin inhibidor:
  calculocon<-data.frame(coef(con))
  calculosin<-data.frame(coef(sin))
  
  # Se reunen todos los datos obtenidos en un solo dataframe:
  Results<-data.frame(Vmax=c(calculocon[1,1],calculosin[1,1]),
                      Km=c(calculocon[2,1],calculosin[2,1]),
                      row.names = c("With","Without"))
  Results
  
  
  if(plot){ 
    # Si plot es igual a T:
    
    ecuacion<-function(Km,Vmax,x) (Vmax*x)/(Km+x)   # Se crea la función para calcular la ecuación de Michaelis-Menten 
    
    # Argumentos:
    
    # Km= concentracion del sustrato cuando Vmax/2
    # Vmax= velocidad maxima
    # x= concentracion de sustrato
    
    # Grafica:
    
    par(mar=c(3.5,3.5,2,1),mgp= c(2,1,0), cex=0.9)  # Ajustes para la gráfica
    
    plot(without~ S, main= "Curva de Michaelis-Menten",
         xlab="[S]", ylab="V0", type="n")  # Se establece la gráfica
    
    curve(ecuacion(x, Vmax =Results[2,1], Km = Results[2,2]), 
          add = T, col = "violet")  # Curva sin inhibidor
    
    curve(ecuacion(x, Vmax =Results[1,1] , Km = Results[1,2]), 
          add = T, col = "turquoise")  # Curva con inhibidor
    
    legend( x = mean(datos$S), y = mean(datos$with), col = c("turquoise", 
                                                             "violet"), legend = c("Con inhibidor", "Sin inhibidor"), 
            lty = 1, merge = T) # Leyenda  
  }
  Results
  
}
