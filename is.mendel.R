is.mendel <- function(obs, alpha = 0.05){

# Octubre 2019
# Juanita Agudelo Posada
# Diana Gomez Alvarez
# Curso: Programacion
  
# DESCRIPCION:
  
#  La funcion predice si un modelo de herencia obedece a la genetica mendeliana, 
#  a partir de la cantidad de individuos observados con cada fenotipo y el nivel 
#  de significancia deseado (0.05 por defecto). La conclusion e si obedece o no a 
#  un modelo mendeliano se genera utilizando una prueba de chi cuadrado.

  
# ARGUMENTOS:

# obs: vector numerico con la cantidad de individuos  observados por
#      cada fenotipo, ordenados de mayor a menor numero.
# alpha: nivel de significancia con que se quiere obtener el resultado.
#        Por defecto p = 0.05
  
# CODIGO:

# Se define la cantidad total de individuos
n <- sum(obs)
  
# Se define la cantidad de fenotipos diferentes observados
cruce <-length(obs)

# Se define el tipo de cruce que se va a realizar
if(cruce == 2){esp <- c(n* 3/4, n* 1/4)}
if(cruce == 4){esp <- c(n* 9/16, n* 3/16, n* 3/16, n* 1/16)}

# Se calculan los grados de libertad y 1-alpha
gl <- cruce-1
p <- 1-alpha

# Se calculan el chi caudrado calculado (x2c) y el tabulado (x2t)
x2c <- sum( (obs-esp)^2/esp )
x2t <- qchisq(p = p, df = gl, ncp = 0, lower.tail = TRUE)

if(x2c >= x2t){mensaje1 <- 'Rechazar H0'} else 
  {mensaje1 <- 'Retener H0'}
if(x2c >= x2t){mensaje2 <- '  No cumple el modelo mendeliano'} else 
  {mensaje2 <- '  Cumple el modelo mendeliano'}

# Resultado final
data.frame(x2Cal = x2c, x2Tab = x2t, Resultado = mensaje1, 
           Conclusion = mensaje2)

# SALIDA:

# La funcion entrega un data.frame con el chi cuadrado calculado, 
# el chi cuadrado tabulado, el resultado de si se acepta o se 
# rechaza H0 y lo que esto significa respecto al modo de herencia.


# EJEMPLO DE USO:

# Para una poblacion de 1000 frijoles se tienen 740 individuos con 
# semilla rugosa y 260 con semillas lisas. Determinar si el modo de 
# herencia obedece a la genetica mendeliana.
# is.mendel(obs = c(740, 260), alpha = 0.01)
}
