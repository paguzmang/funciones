descripcion_por_var <- function(x, tib = F, View = F){
  # Nov 2019
  # paguzmang
  # Funcion que entrega informacion sobre las columnas de un data.frame.
  # 
  # Argumentos:
  # x = data.frame
  # tib = Logico. ¿El resultado se desea en formato tibble? Por defecto: FALSE.
  #        requiere el paquete 'tibble'
  # View = Logico. Si TRUE la tabla resultante se visualiza en el visor de Rstudio
  #        Por defecto FALSE. Esto es, la tabla se imprime en la consola
  #
  f <- function(y){
    tab <- table(y)
    nom.tab <- names(tab)
    d <- data.frame(
      tipo = class(y),
      n.na = sum(is.na(y)),
      n.ceros = sum(identical(y,0)),
      n.dist = length(unique(y)),
      valor.mas.frec   =  ifelse(is.null(nom.tab), NA, nom.tab[which.max(tab)]),
      n.valor.mas   =  ifelse(is.null(nom.tab), NA, tab[which.max(tab)]), 
      stringsAsFactors = F
    )
    rownames(d) <- NULL
    d
  }
  dd <- lapply(X = x, FUN = f)
  r <- dd[[1]]
  k <- length(dd)
  for(i in 2:k) r <- rbind(r, dd[[i]]) 
  r <- cbind(
    variable = names(dd),
    r
  )
  r$variable <- as.character(r$variable)
  if(tib) {
    require(tibble)
    r <- as_tibble(r)
  }
  if(View) View(r) else r
  
  # Valor
  # data.frame con informacion asociada a cada columna del data.frame
}
