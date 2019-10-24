promsem<-function(notas,cred,asig=NA, nes=3.0, tab=F){
  # Oct 2019
  # Nicole Lewis Moncada | Curso: Programacion
  
  # Descripcion
  # Esta es una funcion para calcular la nota promedio del semestre y mostrarla
  # sola con un mensaje o en un data.frame
  
  # Argumentos:
  # tab: Con este argumento se decide si la informacion aparece en un data.frame 
  # con todas las notas y la buscada,o aparece solo la nota con un mensaje, si se 
  # busca un data.frame, se deja pone tab=T, sino, no se pone el argumento, 
  # ya que esta hecho por defecto para que aparezca la nota con el mensaje
  # nes: Es la nota que se necesita, esta por defecto en 3.0, si se necesita una 
  # nota diferente usa este argumento
  # notas: En este argumento va un vector numerico con las notas cada materia
  # cred: en este argumento va un vector numerico con los creditos de cada
  # materia, debe coincidir con las notas puestas en notas
  # asig: En esta argumento va un vector de caracteres con el nombre de las 
  # materias vistas en el semeste, solo es necesario ponerlo cuando tab=T
  sum(notas*cred)/sum(cred)
  fin<-sum(notas*cred)/sum(cred)
  fin<-round(fin,2)
  mp <- c(" Lo lograste!", "Muy bien!", "Si se pudo!")
  mp <- sample(x=mp, size = 1)
  mn <- c("lo lamento", "Es una lastima",
          "Ya sera el proximo semestre")
  mn <- sample(x=mn, size = 1)
  
  if(tab){data.frame(asignatura=c(asig,"Necesaria","Total semestre"),
                        notas=c(notas, nes,fin),
                        creditos= c(cred, sum(cred),sum(cred)))} else {ifelse(fin<nes,
                        paste(fin, mn, sep = " "),paste(fin, mp, sep = " "))}
  # Ejemplo de uso
  # promsem(asig=c("matematicas 3","Bioquimica","Biologia Molecular"," Biofisica" ),
  # notas=c(4,3.2,4,2),cred=c(3,2,3,4), nes=3.4, tab=T)
  #          asignatura notas creditos
  #1      matematicas 3   4.0        3
  #2         Bioquimica   3.2        2
  #3 Biologia Molecular   4.0        3
  #4          Biofisica   2.0        4
  #5          Necesaria   3.4       12
  #6     Total semestre   3.2       12
}