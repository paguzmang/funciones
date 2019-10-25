multi_eva <- function(ans, stu) {
  #
  # Creada el 21-oct-2019
  # Miguel Angel Valencia Osorno; Karen Gisselle Aguilar; Valentina Ramirez Mora
  # Curso: Programacion
  #
  # Esta funcion entrega un data.frame que contiene el total de respuestas
  # correctas y la nota que los estudiantes obtienen en una evaluacion
  # tipo opcion multiple, a partir de un vector con las respuestas correctas
  # y un data.frame con las respuestas de los estudiantes.
  #
  # Argumentos:
  # ans = un vector (puede ser numerico o de caracteres) que contenga las
  # respuestas correctas de la evaluacion.
  # stu = un data.frame que contenga las respuestas de los estudiantes.
  #
  if(!is.data.frame(stu)){
    stop("Las respuestas de los estudiantes deben estar en un data.frame")
  }
  if(!is.vector(ans)){
    stop("El argumento debe ser un vector")
  }
    resul <- data.frame(NA)
    for(i in 1:length(stu)){
      if(length(ans)!=length(stu[[i]])){
        stop("Las longitudes de los vectores deben ser las mismas")
      }
      x <- 0
      for(k in 1:length(ans)){
        x <- x + sum(ans[k]==stu[[k,i]])
        resul[1,i] <- data.frame(x)
      }
      resul[2,i] <- round(resul[1,i]/length(ans)*5,2)
    }
    colnames(resul) <- colnames(stu)
    rownames(resul) <- c("Respuestas correctas", "Nota")
  resul
  # Salida:
  # La funcion entrega un data.frame que contiene el total de respuestas
  # correctas y la nota de cada estudiante (0 < nota < 5)
  #
  # Ejemplo de uso:
  # Se define un data.frame con las respuestas de los estudiantes
  # m <- data.frame(sara=c("a","b","c","b","d","a"),
  #                mario=c("a","d","b","c","d","a"),
  #                vanessa=c("b","a","e","a","c","a"),
  #                julio=rep("a",6))
  # Se entrega directamente un vector con las respuestas correctas y el
  # data.frame creado anteriormente
  # multi_eva(ans=c("a","b","c","b","d","e"), stu = m)
}