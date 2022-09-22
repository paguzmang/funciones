img_circle_crop <- function(img, file = "img.png", f = 0, col = "white"){
  
  # 25-ago-2022
  # Funcion que hace un crop de una imagen en forma de circulo. Tomado
  # de stackoverflow. El crop circular 1ero. forma un recorte cuadrado usando
  # la dimension (ancho o alto) menor de la imagen suministrada.
  
  # codigo original tomado de:
  # https://stackoverflow.com/questions/64597525/r-magick-square-crop-and-circular-mask
  
  # img = objeto imagen generado con los comandos de magick
  # file = Nombre de archivo para guardar la imagen en un nuevo archivo.
  #        Si NULL, devuelve un objeto imagen de magick sin guardar
  # f = fraccion entre 0 y 1 para seleccionar la ubicacion relativa
  #     desde donde iniciar el crop en la dimension (ancho o alto) mayor
  #     Pe., si img tiene un ancho > largo, entonces f = 0.3 indicaria
  #     que inicie el crop un 30% desde izq a derecha a lo ancho de la 
  #     imagen. En caso de que el ancho > largo, un valor de f < 0.5 
  #     realizaria un crop antes de la mitad de la imagen y un 
  #     f > 0.5 realizaria un crop despues de la mitad de la imagen.
  #     Si f tiene un valor que hace superar el ancho o alto de la imagen al
  #     momento de realizar el crop, el programa automaticamente ajusta
  #     el valor para que se respete la dimension menor de la imagen.
  #     Este ajuste ocasiona que para varios valores de f, la ubicacion del recorte
  #     no cambie. 
  #     Si alto > ancho, el valor de f cuenta de arriba a abajo. Si alto < ancho,
  #     el valor de f cuenta de izquierda a derecha.
  # 
  
  require(magick)
  ii <- image_info(img)
  w <- ii$width
  h <- ii$height
  z <- min(w,h)
  if(w > h) {
    a <- w*f
    while(a + z >= w) a <- a - 0.01*a
    b <- 0
  } else{
    a <- 0
    b <- h*f
    while(b + z >= h) b <- b - 0.01*b
  }
  
  geom <- paste0(z, "x", z, "+", a, "+", b)
  img1 <- image_crop(img, geometry=geom, repage=TRUE)
  
  # create a new image with white background and black circle
  fig <- image_draw(image_blank(z, z))
  symbols(z/2, z/2, circles=(z/2)-3, 
          bg="black", inches=FALSE, add=TRUE)
  dev.off()
  
  # create an image composite using both images
  img2 <- image_composite(img1, fig, operator='copyopacity')
  
  # set background as white
  img3 <- image_background(img2, col)
  
  # se entrega el resultado
  if(is.null(file)){
    img3
  } else{
    image_write(img3, path = file)
  }
}
