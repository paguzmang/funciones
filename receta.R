receta <- function(gr,porcentaje = NULL,ingredientes = NULL, 
                   receta = c('e.a','m.l')){
  
  # 21 - 10 - 2019
  # Aura B. Rios Becerra ; Andrea Marulanda Osorio
  # Curso: Programacion
  
  # Descripcion:
  # Calcula los gramos de los ingredientes que requiere una receta, el recetario
  # incluye empanadas argentinas y maria luisas
  # Version 1.0 / 21-10-2019
  
  #Argumentos:
  # gr: cantidad de comida que se desea preparar (en gramos)
  # porcentaje: vector numerico que contiene los porcentajes (proporciones) de los
  # ingredientes que lleva la receta.
  # ingrediente: vector de caracteres que contiene el nombre de los ingredientes de la 
  # receta.
  # receta: nombre de la receta incluida en la funci?n.'e.a' = empada argentina,
  # 'm.l' = maria luisa.
  
# Codigo:
mensaje <- "si porcentaje es NULL,
                   'receta' debe ser e.a = empanadas argentinas o m.l = maria 
                   luisa"
  if(!receta[1] %in% c('e.a','m.l')) stop(mensaje)
  if(is.null(porcentaje)){
    switch(receta[1],
           'e.a' = data.frame('Ingrediente'= c('Harina','Margarina','Leche','Sal',
                                               'Azucar'),
                              'Porcentaje' = c(100,35,50,2,30),
                              'Cantidad_gr' = c(gr,gr*0.35,gr*0.5,gr*0.02,gr*0.3)),
           'm.l' = data.frame('Ingrediente'= c('Harina','Polvo de hornear','nuez o 
                                               canela'
                                               ,'Leche p','Leche l','Sal','Azucar',
                                               'vainilla','huevos',
                                               'grasa'),
                              'Porcentaje' = c(100,2,0.1,5,50,0.3,80,0.5,90,90),
                              'Cantidad_gr' = c(gr,gr*0.02,gr*0.001,gr*0.05,gr*0.5,
                                                gr*0.003,gr*0.8,
                                                gr*0.005,gr*0.9,gr*0.9))
    )}
  else {
    if(is.null(ingredientes))
    {data.frame('Ingredientes'=1:length(porcentaje)
                ,'Porcentaje'= porcentaje,'Cantidad_gr'= (porcentaje*gr)/100)}
    else {data.frame('Ingredientes'=ingredientes
                     ,'Porcentaje'= porcentaje,'Cantidad_gr'= 
                       (porcentaje*gr)/100)}}


# La funcion entrega un data.frame con tres filas: Ingredientes, Porcentaje, 
# Cantidad_gr

# Probando la funcion
# receta(gr = 666,  receta = "m.l")
# receta(gr = 250, ingredientes = c("Pasta","Agua", "Aceite","Sal","Maicitos",
#                                  "Atun","Salsa"),
#       porcentaje = c(100, 200, 2, 1, 20, 10, 5 ))
# receta(gr = 250,porcentaje = c(100, 200, 2, 1, 20, 10, 5 ))
}






