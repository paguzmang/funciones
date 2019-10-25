# Funcion fatburn

fatburn <- function(edad, peso, f.r, sexo = c("H", "M")) {
  #
  # Oct 2019
  # Manuela Cano; Santiago Quintero; Josue David Garces Soto
  # Curso: Programacion
  
  #Descripcion:
  #Calcula el intervalo de frecuencia cardiaca necesaria para quemar grasa
  #eficientemente.
  
  #Argumentos:
  # edad: edad del usuario, en años.
  
  # peso: peso del usuario, en Kilogramos.
  
  # f.r:  frecuencia cardiaca en reposo, en pulsaciones por minuto (ppm). 
  # (Para saber la frecuencia en cardiaca en reposo, se recomienda medirla
  # justo despues de levantarse; asi, el pulso se encuentra en
  # total reposo). 
  
  # sexo: genero del usuario, dentro de comillas. "M" para Mujer y "H" para Hombre.
  
  #Codigo:
  
  #mensaje:
  mensaje <- "Ojo: argumento sexo solo recibe M (mujer) o H (hombre)"
  
  #condicional para arrojar el mensaje:
  if(!sexo %in% c("H", "M")) stop(mensaje)
  
  #condicional en relacion a lo que introduzca el usuario en el argumento "sexo":
  f_maxima <- if(sexo == "H"){((210-(0.5*edad)))-(0.2*peso)} else {((210-(0.5*edad)))-(0.2*peso)+4}
  
  #frecuencia cardiaca en 60% y 70%
  fc1 <- (f_maxima-f.r)*0.6+f.r
  fc2 <- (f_maxima-f.r)*0.7+f.r
  
  #salida:
  print(paste0('Para quemar grasa debes de mantener tu frecuencia cardiaca entre ',  fc1,' ppm y ',  fc2,' ppm')) 
  
  #Ejemplo de uso 1

  #Si tenemos un usuario hombre, con 23 a?os, que tenga un peso de de 68 Kg, con una frecuencia en 
  #reposo de 67 ppm ,?cual es el intervalo de frecuencia cardiaca necesaria para quemar grasa adecuadamente?
  #Comando:
  
  #fatburn(edad = 23, peso = 68, f.r = 67, sexo = "H")
  
  #Ejemplo de uso 1

  #Se tiene una mujer de 54 a?os , con un peso de 47 kilogramos, con un ritmo
  #cardiaco normal de 84 ppm,?cual es el intervalo de frecuencia cardiaca para quemar grasa adecuadamente?
  
  #fatburn(edad=54, peso =47, f.r=84, sexo="M")
  
  # ejemplo de uso 3
  
  #se tiene una mujer de 19 a?os , con un peso de 50 kg, y una f.r de 100ppm,
  #?cual seria el rango de frecuencia cardiaca ideal para quemar grasa adecuadamente?
  
  #fatburn(edad= 19 , peso= 50, f.r= 100 , sexo="M")
  }

