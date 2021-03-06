
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

#logueado a la web del SIFP----
# URL DE LOGUEO SIFP
url_sifp_login="https://sifp.psico.edu.uy/?q=user"

#create a web session with the desired login address
session<-html_session(url_sifp_login)
nodo_form <- html_node(session, xpath = '//*[@id="user-login"]')
form<-html_form(nodo_form)
filled_form<-set_values(form, name="*****", pass="******") #hay que completarlo con un usuario y una clave
submit_form(session, filled_form)

#listado de docentes en SIFP----
listado_SIFP = session %>% 
  jump_to(url= "https://sifp.psico.edu.uy/listado-de-docentes") %>%
  read_html() %>%
  html_node('table') %>% 
  html_table()

enlaces <- session %>% 
  jump_to(url= "https://sifp.psico.edu.uy/listado-de-docentes") %>%
  read_html() %>%
  html_node('table') %>% 
  html_nodes("a") %>% 
  html_attr("href")

listado_SIFP$enlace.SIFP.docente = enlaces
listado_SIFP$enlace.SIFP.docente <- paste("https://sifp.psico.edu.uy", listado_SIFP$enlace.SIFP.docente, sep="")
rm(enlaces)

#levanto datos de la web de cada docente en el SIFP----
rm(trabajadores)
for(i in docentes.SIFP$enlace.SIFP.docente ){
  nombres = session %>% 
    jump_to(url= i) %>% 
    read_html() %>% 
    html_node (xpath = "/html/body/div[1]/div[2]/div[1]/div[3]/div/section/div/div/div/div/div[2]/fieldset/div") %>% 
    html_nodes (".field-label") %>% 
    html_text() %>% 
    str_trim() %>% 
    as.data.frame()

  datos = session %>% 
    jump_to(url= i) %>% 
    read_html() %>% 
    html_node (xpath = "/html/body/div[1]/div[2]/div[1]/div[3]/div/section/div/div/div/div/div[2]/fieldset/div") %>% 
    html_nodes (".even") %>% 
    html_text() %>% 
    str_trim() %>% 
    as.data.frame()
  
  
  df <- cbind(i, nombres, datos)
  names(df) = c("enlace.SIFP.docente", "campos", "contenido")
  
  if(exists("trabajadores")){
    trabajadores <- rbind(trabajadores, df)
  } else {trabajadores <- df}
}

rm(i, nombres, datos, df)

trabajadores_spread = trabajadores %>%
  filter(campos == "Grado:" |
           campos == "Horas base:" |
           campos == "Horas extensi�n:" |
           campos == "Horas reducci�n:" |
           campos=="Dedicaci�n total:") %>%
  spread(campos, contenido) %>%
  rename(Grado = 'Grado:',
         Horas.base = 'Horas base:',
         Horas.extension = 'Horas extensi�n:',
         Horas.reduccion = "Horas reducci�n:",
         DT= 'Dedicaci�n total:')
  droplevels()

#combino para crear docentes_SIPF----
docentes_SIFP = docentes_SIFP %>%
  select (Apellido, Nombre, Instituto, enlace.SIFP.docente) %>%
  left_join(., trabajadores_spread, by = "enlace.SIFP.docente")

#agrego nombre en ucos y en web a partir del diccionario

#diccionario2 = read.csv("diccionario2_mano.csv", header = TRUE)
  
docentes_SIFP = docentes_SIFP2 %>% 
  unite(nombre.SIFP, c(Nombre, Apellido), sep = " ", remove = TRUE) %>% 
  left_join(diccionario2, ., by = "nombre.SIFP")
  
save(docentes_SIFP,file="docentes_SIFP.RData")

rm(trabajadores, trabajadores_spread, listado_SIFP, session, nodo_form, filled_form, url_sifp_login)