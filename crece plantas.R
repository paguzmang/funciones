crec_plan <-function(s=0,tasa,sobj,d,tamano=F,grafico=F ) {
  # Oct 2019
  # Cesar Augusto Sabogal | Curso Programacion
  
  #argumentos:
  #tasa: crecimiento diario
  #s: tamano inicial
  #sobj: tamano objetivo (opcion 1)
  #d:dia a evaluar (opcion 2)
  if(tamano ) {w<-s+(d*tasa)
  x <- seq(from=0,to=d,by=1)
  y <- s+(x*tasa) }
  else {d<-(sobj-s)/tasa
  y <- seq(from=0,to=sobj,by=1)
  x <-(y-s)/tasa }

  if(grafico) {print(data.frame(tiempo=x,crecimiento=y))
    par(mar=c(3.5,3.5,1,1),mgp=c(2,1,0))
  plot(x,y,xlab='tiempo (dias)',ylab='crecimiento (cm)',type='l',main='crecimiento plantas')
  }
  else {print(data.frame(tiempo=x,crecimiento=y))}
}




