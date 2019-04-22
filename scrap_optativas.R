
#----
#librerias y directorio
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
    names(enlaces) = c("enlace.SIFP.optativas")
    optativas$enlace.SIFP.optativas = enlaces$enlace.SIFP.optativas
    
    if(exists("optativas_listado")){
      optativas_listado <- rbind(optativas_listado, optativas)
    } else {optativas_listado <- optativas}
  }
}
rm(year, ciclo, i, j, optativas, nodo_op, enlaces)
optativas_listado = optativas_listado %>%
  select (Año, Cupos, 'Docente Responsable', 'Código de la materia', 'Código de horario', ciclo, enlace.SIFP.optativas) %>% 
  mutate(`Docente Responsable` = str_remove (`Docente Responsable`, regex(",", ignore_case = TRUE)))
  
rep = data.frame(origen = c("á", "é", "í", "ó", "ú", "ñ"), destino = c("a", "e", "i", "o", "u", "n"))
for (i in 1:nrow(rep)){
  origen = rep[i, "origen"]
  destino = rep[i, "destino"]
  names(optativas_listado) <- gsub(x = names(optativas_listado), pattern = paste(origen), replacement = paste(destino))
}
rm(rep, i, origen, destino)


# Obtengo informacion de docentes y creditos----
rm(optativas_docentes)
for(i in optativas_listado$enlace.SIFP.optativas){
  web=read_html(i)
  
  lista = as.data.frame(i)
  
  # # Obtengo creditos por optativa
  texto = as.character(web)
  texto_ok = str_replace_all(texto, "[\r\n]" , "")
  get_creditos = 'Créditos:.</div><div class=\\\"field-items\\\"><div class=\\\"field-item even\\\">([^</div]*)'
  creditos = str_match(texto_ok, get_creditos)
  
  lista$creditos = creditos[2]

  #Obtengo docentes que participan de la optativa
  enlace.SIFP.docente = web %>% 
    html_nodes('.profile2-perfil-docente') %>% 
    html_nodes("a")%>% 
    html_attr("href") %>% 
    
  if (length(enlace.SIFP.docente)>0) {
    lista = bind_rows(replicate(length(enlace.SIFP.docente), lista, simplify = FALSE))
    lista$enlace.SIFP.docente = enlace.SIFP.docente
  } else{lista$enlace.SIFP.docente = NA
    }
  
  if(exists("optativas_docentes")){
    optativas_docentes <- rbind(optativas_docentes, lista)
  } else {optativas_docentes <- lista
    }
}
rm(i, web, lista, get_creditos, creditos, texto, texto_ok, enlace.SIFP.docente)
names(optativas_docentes) = c("enlace.SIFP.optativas", "creditos", "enlace.SIFP.docente")

#porque en las fichas contienen dos horarios pero que están ingresados más de una vez en el listado y genera duplicacion
#al contar la cantidad de clases hay que buscar por docente, y de enlace.SIFP.optativas contar la cantidad de veces que aparece en optativas y la cantidad de docentes cada vez
optativas_docentes = optativas_docentes %>% 
  distinct(enlace.SIFP.optativas, enlace.SIFP.docente, .keep_all = TRUE)
  
#agrego numero de docentes al listado----

optativas = optativas_docentes %>% 
  group_by (enlace.SIFP.optativas, creditos) %>% 
  summarise (Cantidad.docentes = n()) %>% 
  left_join(optativas_listado, ., by="enlace.SIFP.optativas")
rm (optativas_listado)

#guardo las salidas----
save(optativas,file="optativas.RData")
save(optativas_docentes,file="optativas_docentes.RData")
