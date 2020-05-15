e.orden <- function ( a , b , c = NA, tipo = c(1 , 2), mas = TRUE){ 

  # 25-Mayo,2020
  # Torres J. Alejandro & Silva P. Valentina
  # Funcion: "Ecuaciones de Primer y Segundo Orden"
  
# Descripcion: `e.orden` dá al usuario la posibilidad de encontrar la 
  # respuesta a una ecuación de primer y/o segundo grado.

# Argumentos:
  
  #a = N° que acompaña el "x^2" en ecuaciones de segundo orden y 
       #a "x" en primer orden.
  
  #b = N° que acompaña a la "x" en ecuaciones de segundo grado y 
       #en ecuaciones de primer orden, será un numero conocido que no 
       #tiene como acompañante, una variable . 
  
  #c = N° que no acompaña a ninguna X en ecuaciones de segundo orden.
       #si se tiene una ecuacion de primer orden, no será necesario su
       #usó   además la funcion se dará cuenta y lo asumira como un "NA".
  
  #tipo = Es un vector, con dos valores: 1 y 2, en donde 1 representa 
       #una ecuacion de primer orden o 2 una de segundo orden.
  
  #mas = Argumento SOLO para ecuaciones de segundo grado, por defecto, 
       #en las ecuaciones de primer orden, se asume como "NA". Aquí, el 
       #usuario elige, según su necesidad, entre las dos respuestas que  
       #puede presentar la funcion, para ello utiliza "TRUE" si desea  
       #ver el resultado sumando el discriminante o FALSE para ver el 
       #resultado restando el discriminante.
  
# Codigo:
  
  switch(tipo[1], 
         
         primero= { -b/a }, 
         
         segundo={  
           
           dis <- b^2-4*a*c
           
           if (dis < 0) stop("El discriminante es negativo, la solucion esta en los N° complejos")
           
           if(mas){ (-b+sqrt(dis))/(2*a)
             
           } else { (-b-sqrt(dis))/(2*a)
             
           } })
  
# Ejemplo de uso: 
  # e.orden(a = 2, b = -2)                                 # 1er orden
  # e.orden( a=2 , b=-2 , c = -3, tipo = c(2), mas = TRUE) # Segundo orden, suma el discriminante.
  # e.orden( a=2 , b=-2 , c = -3, tipo = c(2), mas = FALSE)# Segundo orden, resta el discriminante.
}