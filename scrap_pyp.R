
#----
#librerias y directorio
#----

library(rvest)
library(dplyr)
library(tidyr)
library(tibble)
library(reshape2)
library(httr)
library(stringr)

#----
#PyPs
#----

#levanto la lista----
year = c("2018")
ciclo = c("5", "6", "5y6", "7", "8", "7y8")
rm(pyp_listado)
for (i in year){
  for (j in ciclo){
    pyp <- read_html(paste0("https://sifp.psico.edu.uy/guias-practicas-o-proyectos-publicas?field_tipo_guia_pp_value=All&field_nombre_guia_pp_value=&field_nombre_pdocente_value=&field_apellido_pdocente_value=&field_ciclo_guia_pp_value=",j,"&field_nombre_pinstituto_value=&field_creditos_guia_pp_value=&field_anio_guia_pp_value%5Bvalue%5D%5Byear%5D=",i,"&field_cod_materia_guia_pp_value="))
    nodo_pyp <-html_node(pyp, xpath = '/html/body/div[1]/div/div[1]/div[3]/div/section/div/div/div/div/div[2]/table')
    pyp <- html_table(nodo_pyp)
  
    pyp$ciclo = j
    
    enlaces <- data.frame(html_attr(html_nodes(nodo_pyp, "a"), "href"))
    names(enlaces) = c("enlace.SIFP.pyp")
    pyp$enlace.SIFP.pyp = enlaces$enlace.SIFP.pyp
    
    if(exists("pyp_listado")){
      pyp_listado <- rbind(pyp_listado, pyp)
    } else {pyp_listado <- pyp}
  }
}
rm(year, ciclo, i, j, pyp, nodo_pyp, enlaces)
pyp_listado = pyp_listado %>%
  select (-Instituto) %>%
  distinct() %>%
  select (ciclo, Año, Código,`Código de horario`, `Docente Responsable`, Cupos, ciclo, enlace.SIFP.pyp) %>%
  separate (`Docente Responsable`, c("Apellido", "Nombre"), sep = ",") %>%
  unite ("nombre.SIFP", c(Nombre, Apellido), sep = " ") %>%
  mutate(nombre.SIFP = str_trim(nombre.SIFP, side = "both"))
  
  
rep = data.frame(origen = c("á", "é", "í", "ó", "ú", "ñ"), destino = c("a", "e", "i", "o", "u", "n"))
for (i in 1:nrow(rep)){
  origen = rep[i, "origen"]
  destino = rep[i, "destino"]
  names(pyp_listado) <- gsub(x = names(pyp_listado), pattern = paste(origen), replacement = paste(destino))
}
rm(rep, i, origen, destino)

#levanto cada ficha de pyp----
rm(pyp_docentes)
for(i in pyp_listado$enlace.SIFP.pyp){
  web=tryCatch({read_html(i)},
               error=function(e) NA)
  
  lista = as.data.frame(i)
  
  # # Obtengo creditos por optativa
  texto = as.character(web)
  texto_ok = str_replace_all(texto, "[\r\n]" , "")
  get_creditos = 'Créditos:.</div><div class=\\\"field-items\\\"><div class=\\\"field-item even\\\">([^</div]*)'
  creditos = str_match(texto_ok, get_creditos)
  
  lista$creditos = creditos[2]

  # Obtengo docentes que participan de la optativa
  enlace.SIFP.docente = web %>% 
    html_nodes('.profile2-perfil-docente') %>% 
    html_nodes("a")%>% 
    html_attr("href")
  if (length(enlace.SIFP.docente)>0) {
            lista = bind_rows(replicate(length(enlace.SIFP.docente), lista, simplify = FALSE))
            lista$enlace.SIFP.docente = enlace.SIFP.docente
  } else{lista$enlace.SIFP.docente = NA
  }

  if(exists("pyp_docentes")){
    pyp_docentes <- rbind(pyp_docentes, lista)
  } else {pyp_docentes <- lista}
}
rm(i, web, lista, get_creditos, creditos, texto, texto_ok, enlace.SIFP.docente)
names(pyp_docentes) = c("enlace.SIFP.pyp", "creditos", "enlace.SIFP.docente")

#en las fichas a veces se indica campo y entonces se repite el/los docentes
pyp_docentes = pyp_docentes %>% 
  distinct(enlace.SIFP.pyp, enlace.SIFP.docente, .keep_all = TRUE)

#reemplazo los NAs con el enlace al SIFP del docente responsable
#diccionario2 = read.csv("diccionario2_mano.csv", header = TRUE)

pyp_docentes_faltantes = pyp_docentes %>% 
  filter (is.na(enlace.SIFP.docente)) %>% 
  select(-enlace.SIFP.docente) %>% 
  left_join(., select(pyp_listado, enlace.SIFP.pyp, nombre.SIFP), by="enlace.SIFP.pyp") %>% 
  left_join(., select(docentes_SIFP, nombre.SIFP, enlace.SIFP.docente), by="nombre.SIFP") %>% 
  distinct()

pyp_docentes = pyp_docentes %>% 
  filter (!is.na(enlace.SIFP.docente)) %>% 
  rbind (., select(pyp_docentes_faltantes, enlace.SIFP.pyp, creditos, enlace.SIFP.docente))

#agrego numero de docentes al listado----
pyp = pyp_docentes %>% 
  group_by (enlace.SIFP.pyp, creditos) %>% 
  summarise (Cantidad.docentes = n()) %>% 
  left_join(pyp_listado, ., by="enlace.SIFP.pyp")
rm (pyp_listado)

#guardo las salidas----
save(pyp,file="pyp.RData")
save(pyp_docentes,file="pyp_docentes.RData")
