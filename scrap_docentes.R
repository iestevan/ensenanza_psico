
#----
#librerias y directorio
#----

library(rvest)
library(dplyr)
library(tidyr) #redundante al levantar dplyr?
library(tibble) #redundante al levantar dplyr
library(reshape2)
library(httr)
library(stringr)


#----
#listado de docentes
#-----

#docentes de mdeo en la web----
mdeo = c(158:162, 350:352) #sale de filtrar en la pagina de busqueda: fund(152)-educa(162), cibpsi(350), cicp(351), ceis(352)
rm(docentes_cargos)
for(i in mdeo){
  post=POST(url= paste0("https://psico.edu.uy/directorio/docentes-?field_perfil_nombre_value=&field_perfil_instituto_target_id=",i,""))
  cargos=read_html(post)
  nodo_car <- html_node(cargos, 'table')
  df <- data.frame(html_table(nodo_car)[,2])
  names(df) = c("nombre.web")
  
  enlaces <- data.frame(html_attr(html_nodes(nodo_car, "a"), "href"))
  names(enlaces) = c("enlace.web.docente")
  
  df$enlace.web.docente = enlaces$enlace.web.docente
  
  if(exists("docentes_cargos")){
    docentes_cargos <- rbind(docentes_cargos, df)
  } else {docentes_cargos <- df}
}
rm(i, mdeo, post, cargos, enlaces, nodo_car, df)
docentes_cargos = docentes_cargos %>%
  distinct()

#levanto info de cada web personal----
rm(trabajadores)
for(i in docentes_cargos$enlace.web.docente){
  web=tryCatch({read_html(paste0("https://psico.edu.uy",i,""))},
               error=function(e) NA)
  
  if(!is.na(web)){
    
    datosC <- data.frame(html_text(html_nodes(web,'.field__label'))[c(4:13)])
    datosR <- data.frame(html_text(html_nodes(web,'.field__item'))[c(6:15)])
    df <- cbind(i, datosC, datosR)
    names(df) = c("enlace.web.docente", "campos", "contenido")
  }
  if(exists("trabajadores")){
    trabajadores <- rbind(trabajadores, df)
  } else {trabajadores <- df}
}
rm(i, datosC, datosR, df, web)

trabajadores_spread = trabajadores %>%
  filter(campos == "Provisión:" |
           campos == "Grado:" |
           campos == "Horas/Extensión:" |
           campos == "Instituto / Centro:" |
           campos=="DT:") %>%
  spread(campos, contenido) %>%
  separate ('Horas/Extensión:', c("horas.base", "horas.con.ext"), fill = "left") %>%
  droplevels()

names(trabajadores_spread) = c("enlace.web.docente", "DT", "Grado","H.base", "H.con.ext", "Instituto", "Tipo")

#combino----
docentes_cargos = docentes_cargos %>%
  left_join(., trabajadores_spread, by = "enlace.web.docente") %>%
  mutate(nombre.web = chartr("áéíóúÁÉÍÓÚ", "aeiouAEIOU", nombre.web)) %>%
  arrange(nombre.web)
rm(trabajadores, trabajadores_spread)

#write.csv(docentes_cargos, file = "docentes_cargos.csv", row.names=FALSE)
#docentes_cargos = read.csv("docentes_cargos.csv", header = TRUE)
