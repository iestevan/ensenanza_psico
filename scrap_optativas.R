
#----
#librerías y directorio
#----

library(rvest)
library(dplyr)
library(tidyr)
library(reshape2)
library(httr)
library(stringr)
library(splitstackshape)

#-----
#OPTATIVAS
#-----
#levanto las web de cada optativa----
year = c("2018")
ciclo = c("7","8")
rm(optativas_listado)

for (i in year){
  for (j in ciclo){
    optativas <- read_html(paste0("https://sifp.psico.edu.uy/guias-seminarios-optativos-publicas?field_nombre_guia_value=&field_nombre_pdocente_value=&field_apellido_pdocente_value=&field_modulo_guia_value=All&field_ciclo_guia_value=",j,"&field_nombre_pinstituto_value=&field_cod_materia_guia_value=&field_codigo_grupo_rel_guia_value=&field_anio_value%5Bvalue%5D%5Byear%5D=",i,""))
    nodo_op <- html_node(optativas, xpath = '/html/body/div[1]/div/div[1]/div[3]/div/section/div/div/div/div/div[2]/table')
    optativas <- html_table(nodo_op)
    # optativas$Ano = i
    optativas$ciclo = j
    
    enlaces <- data.frame(html_attr(html_nodes(nodo_op, "a"), "href"))
    names(enlaces) = c("enlace")
    optativas$enlace = enlaces$enlace
    
    if(exists("optativas_listado")){
      optativas_listado <- rbind(optativas_listado, optativas)
    } else {optativas_listado <- optativas}
  }
}
rm(year, ciclo, i, j, optativas, nodo_op, enlaces)
optativas_listado = optativas_listado %>% 
  select (Nid, Ano, Cupos, `Codigo de la materia`, ciclo, enlace) 


# Obtengo informacion de docentes y creditos
rm(optativas_docentes)

for(i in optativas_listado$enlace){
  web=tryCatch({read_html(i)},
               error=function(e) NA)
  
  texto = as.character(web)
  texto_ok = str_replace_all(texto, "[\r\n]" , "")
  
  
  # # Obtengo creditos por optativa
  get_creditos = 'CrÃ©ditos:.</div><div class=\\\"field-items\\\"><div class=\\\"field-item even\\\">([^</div]*)'
  creditos = str_match(texto_ok, get_creditos)
  
  creditos = creditos[2]
  
  if(exists("creditos_optativas")){
    creditos_optativas <- rbind(creditos_optativas, creditos)
  } else {creditos_optativas<- creditos}
  
  
  # Obtengo docentes que participan de la optativa
  largo_pre_nombre = 'Nombre:.</div><div class=\\\"field-items\\\"><div class=\\\"field-item even\\\">'
  largo_pre_apellido = 'Apellido:.</div><div class=\\\"field-items\\\"><div class=\\\"field-item even\\\">'
  
  get_nombre = 'Nombre:.</div><div class=\\\"field-items\\\"><div class=\\\"field-item even\\\">([^</div]*)'
  get_apellido = 'Apellido:.</div><div class=\\\"field-items\\\"><div class=\\\"field-item even\\\">([^</div]*)'
  
  #Chequeo cuantos docentes participan de la optativa
  cantidad_docentes = str_count(texto_ok, pattern = get_nombre) 
  
  keep = data.frame(matrix (ncol=2,nrow=cantidad_docentes))
  n <- c("Nombre", "Apellido")
  colnames(keep) <- n
  
  for (j in 1:cantidad_docentes){
    
    pre_nombre = as.data.frame(str_locate_all(texto_ok, pattern = largo_pre_nombre)) #identifico donde empiezan y terminan las expresiones regular hasta el nombre
    nombre = as.data.frame(str_locate_all(texto_ok, pattern = get_nombre)) #identifico donde empiezan y terminan las expresiones regular incluyendo el nombre
    nombre$start=(pre_nombre$end +1) # encuentro la posici??n donde empieza el nombre
    
    keep$Nombre[j] = substr(texto_ok,nombre$start[j],nombre$end[j]) #levanto el nombre
    
    pre_apellido =as.data.frame( str_locate_all(texto_ok, pattern = largo_pre_apellido))# idem para el nombre
    apellido = as.data.frame(str_locate_all(texto_ok, pattern = get_apellido))
    apellido$start=(pre_apellido$end + 1)
    
    keep$Apellido[j] = substr(texto_ok,apellido$start[j],apellido$end[j])}  
  
  a = paste(keep$Nombre, keep$Apellido, sep=" ")
  b = paste(a,collapse = ',')
  
  docentes = b
  
  if(exists("optativas_docentes")){
    optativas_docentes <- rbind(optativas_docentes, docentes)
  } else {optativas_docentes <- docentes}
}

optativas_docentes =as.data.frame(optativas_docentes)
names(optativas_docentes) <- c("Docente")
optativas_docentes=cSplit(optativas_docentes, "Docente", ",")

creditos_optativas = as.data.frame(creditos_optativas)
names(creditos_optativas) <- c("Creditos")

optativas_listado <- bind_cols(optativas_listado, optativas_docentes)
optativas_listado <- bind_cols(optativas_listado, creditos_optativas)

save(optativas_listado,file="optativas_listad.RData")