
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
           campos == "Horas extensión:" |
           campos == "Horas reducción:" |
           campos=="Dedicación total:") %>%
  spread(campos, contenido) %>%
  rename(Grado = 'Grado:',
         Horas.base = 'Horas base:',
         Horas.extension = 'Horas extensión:',
         Horas.reduccion = "Horas reducción:",
         DT= 'Dedicación total:')
  droplevels()

#combino para crear docentes_SIPF----
docentes_SIFP = docentes_SIFP %>%
  select (Apellido, Nombre, Instituto, enlace.SIFP.docente) %>%
  left_join(., trabajadores_spread, by = "enlace.SIFP.docente")

save(docentes_SIFP,file="docentes_SIFP.RData")

rm(trabajadores, trabajadores_spread, listado_SIFP, session, nodo_form, filled_form, url_sifp_login)

#write.csv(docentes_cargos, file = "docentes_cargos.csv", row.names=FALSE)
#docentes_cargos = read.csv("docentes_cargos.csv", header = TRUE)
