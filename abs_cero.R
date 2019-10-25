abs_cero<- function(file, p_atm =101325, p_abs = T, exel = T){
  
  # Funcion abs_cero
  # Autores:Juan Pablo Sierra Aristizabal; Mariana Ossa Yepes
  # Fecha: 24 - Oct - 2019
  # Curso Programacion
  
  ## Descripcion de la Funcion
  
  # La funcion se va a encargar de realizar la estimacion experimental 
  #del cero absoluto, donde este se calcula para esta funcion con dos 
  #datos: temperatura (C) y presion absoluta (Pa).  Para esta funcion 
  #el usuario debe ingresar los datos obtenidos bajo el experimento y
  #estos pueden ser entregado a la funci?n por medio de un  Excel o un
  #Data.frame.
  
  ## Argumentos
  # - File: es donde se entrega la base de datos que contenga 
  # (Excel o Data.frame), los valores de temperatura como primera 
  # columna y presi?n como segunda columna, es importate resaltar 
  #que en caso de Excel es importante que la primera linea tenga las
  #respectivas etiquetas.
  #- p_atm: la presion atmosferica del lugar donde se presento el experimento
  #en (Pa), por defecto esta $101325Pa$ que es la presion a nivel del
  #mar (m s. n. m).
  #- p_abs: argumentos logico donde se establece si la presi?n de
  # los datos importados son presiones absolutas (T) o presiones manometricas
  #(F).
  #- Excel: argumento logico que se pregunta si el argumento 
  # File es un documento Excel o no, por defecto este argumento es 
  # (T), ya que es la entrada de datos mas comun.
  
  # codigo
  require(openxlsx)
  #library(openxlsx)
  
  data <- if(exel){
    
    data <- read.xlsx(xlsxFile = file, sheet = 1, startRow = 1,
                      colNames = T)}
  else{
    data <- file
  }
  
  presion_abs<- if(p_abs){
    presion_abs<- data[[2]]}
  else {
    presion_abs<- data[[2]]+p_atm
  }
  
  
  
  rl<- lm(presion_abs ~  data[[1]])
  
  ca <- -rl$coef[1]/rl$coef[2]
  
  j <- abs((-273.15 - ca)/(-273.15))*100
  
  
  me <- if(j <= 5) {
    me <- "Exelente"
  }
    else{ 
      if(j > 5 & j <=10){
        me <- "Regular, puede existir algun sesgo en la toma de sus datos"
      }
      else {
        me <- "Muy regular, recomendamos volver a tomar los datos"
      }
      }
  
  print(x = list("Estimacion de 0 absoluto:" = paste0("El valor cero absoluto experimental es  de  : ", round(ca, 2)),
                 "Funcion :" = paste0("y", " = ",
                                  round(rl$coef[2],2),"x", " + ",
                                  round(rl$coef[1],2)),
                 "Error: " = paste0("El porcentaje de error es: ", 
                                    round(j,2),  "%" , " La toma datos fue ", me)))

  par(mar = c(4.5, 5.5, 1, 1),
      mgp = c(3.4, 1, 0),cex= 0.9,las = 1)
  plot(data[[1]], presion_abs, 
       xlim = c(ca + ca*(30/100), max(data[[1]]) + max(data[[1]])*(20/100)), 
       ylim = c(10, max(presion_abs) + max(presion_abs)*(2/100)),
       ylab = 'Presion (Pa)',
       xlab = 'Temperatura (?C)', col = 'blue',
       main = "Gr?fica del cero absoluto")
  abline(rl)
  abline(h= 0, v = 0, lty = 2)
  points(x= ca, y= 0, pch = 19, col = "red", bg = "red")
  

## Que entrega la funcion abs_cero
#La funcion  $abs_cero$ le entregara al usuario una lista con tres
#elementos y una grafica. La lista va imprimir al usuario la estimacion
#aproximada del cero absoluto obtenido en el experimento, tambien la
#funci?n que los datos generan que va a permitir la extrapolacion de
#los datos en la estimcacion del cero absoluto y por ultimo el porcentje
#de error que la estimacion con respecto al valor descrito estimado.
#Posteriormente la grafica mostrara el comportamiento de la funcion y 
#la estimacion del valor, dicho valor se mostrara en el eje X de la 
#grafica con un punto rojo.

## Ejemplo de uso de la funcion
#1 hay que tener la libreria "openxlsx" instalada previamente 
#se instala con el siguiente comando 
# install.packages("openxlsx")
#utilizando un file de exel que usa una presion absoluto 
# abs_cero(file = "ejemplo2.xlsx", p_atm = 82700, p_abs = F)

## Resultado
# $`Estimacion de 0 absoluto:`
# [1] "El cero absoluto experimental es  de  : -280.79"

# $`Funcion :`
# [1] "y = 281.85x + 79138.77"

# $`Error: `
# [1] "El porcentaje de error es: 2.8% La toma datos fue Exelente"

}

