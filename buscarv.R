# 19-Ene-2012.
#
buscarv <- function(x, tabla, colb, colr, un.reg = FALSE) {
 #
 # Funcion que hace lo mismo que buscarv(...) de Excel:
 # solo se permiten coincidencias exactas. No importa el orden.
 # 
 # Argumentos:
 # x = valor buscado (un solo elemento)
 # tabla = un data.frame donde se buscara el valor buscado "x"
 # colb = el numero o nombre de la columna en "tabla" donde se
 #        buscara el valor "x"
 # colr = el numero o nombre de la columna en "tabla" desde la
 #        la cual se desea obtener el resultado que coincida.
 
 # un.reg = Logico. Si TRUE se devuelve el 1er. valor que conincida

 #        Este opcion se incluyo por si x aparece mas de una vez
 
 #        en colb. Lo ideal es que aparezca una sola vez. Por esto
 
 #        esta FALSE por defecto.
 #
 fila <- tabla[,colb] %in% x ## x debe estar una sola vez en tabla[, colb]
 if(sum(fila) == 0) NA else {if(un.reg) tabla[fila , colr][1] else tabla[fila , colr]}
 #
 # Uso:
 # sapply(X = ???, FUN = buscarv, USE.NAMES = F,  
 #                                tabla = ???, colb = ???, colr = ???)
 #
 # Con sapply, se entrega un vector de la misma longitud del vector colocado en X.
 # Donde aparezca NA, es porque la buscado no arrajo ninguna conicidencia.
 #
 # En particular, cuando "x" es una columna de fecha, el resultado de sapply(...)
 # es un vector de numeros. Para convertir nuavemente este vector a "fecha" seguir
 # las instrucciones del siguiente bloque.
 #
 # --------------------------------
 #
 # CONVERSION DE NUMERO A FECHA EN R:
 # Cuando el propio R convierte una "fecha" en "numero", parece que el "numero" queda 
 # con origin = "1970-01-01". Luego, si usted quiere volver a convertir este "numero"
 # a "fecha", lo puede hacer con:
 #
 # as.Date("numero", origin = "1970-01-01" )
 #
}
