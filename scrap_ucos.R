
#----
#librerías y directorio
#----

library(rvest)
library(dplyr)
library(tidyr)
library(tibble)
library(reshape2)
library(httr)
library(stringr)

#----
#UCOS
#----
#levanto la lista----
year = c("2018")
rm(ucos_listado)
for (i in year){
  ucos <- read_html(paste0("https://sifp.psico.edu.uy/guias-uco-publicadas?unidad_curricular=&ciclo=&modulo=&anio=",year,"&anio_vigencia="))
  nodo_ucos <-html_node(ucos, xpath = '/html/body/div[1]/div/div[1]/div[3]/div/section/div/div/div/div/div[2]/table')
  ucos <- html_table(nodo_ucos)
  ucos$año = year
  
  enlaces <- data.frame(html_attr(html_nodes(nodo_ucos, "a"), "href"))
  names(enlaces) = c("enlace.SIFP.ucos")
  ucos$enlace.SIFP.ucos = enlaces$enlace.SIFP.ucos
  
  if(exists("ucos_listado")){
    ucos_listado <- rbind(ucos_listado, ucos)
  } else {ucos_listado <- ucos}
}
rm(year, i, ucos, nodo_ucos, enlaces)
ucos_listado = ucos_listado %>%
  select (`Unidad curricular`, Ciclo, Módulo, Creditos, año, enlace.SIFP.ucos) %>%
  arrange(Ciclo, Módulo, `Unidad curricular`)
ucos_listado$enlace.SIFP.ucos <- paste("https://sifp.psico.edu.uy", ucos_listado$enlace.SIFP.ucos, sep="")

#extraigo docentes y grupos de las fichas----
rm(ucos_docentes, ucos_horarios)
for(i in ucos_listado$enlace.SIFP.ucos){
  web=tryCatch({read_html(i)},
               error=function(e) NA)
  
  if(!is.na(web)){
    nodo <- html_node(web, xpath="/html/body/div[1]/div/div[1]/div[3]/div/section/div/div/div/div[3]/div/table")
    nombre.SIFP <- html_text(html_nodes(web,'.item-list'))
    nombre.SIFP <- unlist(strsplit(nombre.SIFP[1], split="\n"))
    nombre.SIFP <- data.frame(nombre.SIFP, "enlace.SIFP.ucos"=i)
    
    if(exists("ucos_docentes")){
      ucos_docentes <- rbind(ucos_docentes, nombre.SIFP)
    } else {ucos_docentes <- nombre.SIFP}
    
    hor=html_table(nodo)
    hor$enlace.SIFP.ucos =i
    
    if(exists("ucos_horarios")){
      ucos_horarios <- rbind(ucos_horarios, hor)
    } else {ucos_horarios <- hor}
  }
}
rm(i, web, nodo, nombre.SIFP, hor)
ucos_docentes = ucos_docentes %>%
  mutate(nombre.SIFP = str_remove (nombre.SIFP, regex("[^[:alpha:]]{0,2}Responsable[^[:alpha:]]{0,2}", ignore_case = TRUE))) %>%
  mutate(nombre.SIFP = str_remove (nombre.SIFP, regex("Docentes participantes[^[:alpha:]]{0,2}", ignore_case = TRUE))) %>%
  distinct()

ucos_horarios = ucos_horarios %>%
  mutate(grupo = c(1:length(ucos_horarios$enlace.SIFP.ucos)))

#https://stringr.tidyverse.org/articles/regular-expressions.html
#intento identificar los horarios de plenarios (más de 90 estudiantes)
ucos_horarios_plenario = ucos_horarios %>%
  filter(!grepl('(Seminario)', `Código de horario`),
         enlace.SIFP.ucos!="/guias/90/view" | grepl('(PLENARIO)', `Código de horario`),
         enlace.SIFP.ucos!="/guias/192/view",
         enlace.SIFP.ucos!="/guias/173/view" | grepl('(PLENARIO)', `Código de horario`),
         enlace.SIFP.ucos!="/guias/179/view" | grepl('(Plenario)', `Código de horario`)
  ) %>%
  select(grupo) %>% 
  mutate (Dispositivo = "Plenario")

#intento identificar los horarios de seminarios (menos de 40 estudiantes)
ucos_horarios_seminario = ucos_horarios %>%
  filter(grepl('(Seminario)', `Código de horario`) |
           enlace.SIFP.ucos=="/guias/90/view" & !grepl('(PLENARIO)', `Código de horario`) |
           enlace.SIFP.ucos=="/guias/192/view" |
           enlace.SIFP.ucos=="/guias/173/view" & !grepl('(PLENARIO)', `Código de horario`) |
           enlace.SIFP.ucos=="/guias/179/view" & !grepl('(Plenario)', `Código de horario`)
  ) %>% 
  select(grupo) %>% 
  mutate (Dispositivo = "Seminario")

ucos_horarios_dispositivos = rbind (ucos_horarios_plenario, ucos_horarios_seminario)

ucos_horarios_docentes = ucos_horarios %>%
  mutate(`Código de horario` = chartr("áéíóúÁÉÍÓÚ", "aeiouAEIOU", `Código de horario`)) %>%
  mutate(`Código de horario` = str_remove (`Código de horario`, regex(".*Do.*ente[:alpha:]*[^[:alpha:]]{0,2}", ignore_case = TRUE))) %>%
  mutate(`Código de horario` = str_remove (`Código de horario`, regex(".*Grupo\\s*\\d[^[:alpha:]]{0,3}", ignore_case = TRUE)))%>%
  mutate(`Código de horario` = str_remove (`Código de horario`, regex(".*EFI[^[:alpha:]]{0,3}", ignore_case = TRUE)))%>%
  mutate(`Código de horario` = str_remove (`Código de horario`, regex(".*\\d{1,2}/\\d{1,2}[^[:alpha:]]{0,3}", ignore_case = TRUE)))%>%
  mutate(`Código de horario` = str_remove (`Código de horario`, regex("[^a-z]{0,3}Codigo.*", ignore_case = TRUE)))%>%
  mutate(`Código de horario` = str_remove (`Código de horario`, regex(".*confirma.*", ignore_case = TRUE)))%>%
  mutate(`Código de horario` = str_remove (`Código de horario`, regex(".*informa.*", ignore_case = TRUE)))%>%
  mutate(`Código de horario` = str_remove (`Código de horario`, regex(".*plenario.*", ignore_case = TRUE))) %>%
  mutate(`Código de horario` = str_remove (`Código de horario`, regex(".*[:digit:]{4}.*", ignore_case = TRUE))) %>%
  separate (`Código de horario`, c("Docente1", "Docente2"), sep = "(\\s)y(\\s)|-", fill = "right") %>%
  gather("Orden", "nombre.ficha.uco", c(Docente1, Docente2)) %>%
  filter(Orden != "Docente2" | !is.na(nombre.ficha.uco)) %>%
  select(grupo, nombre.ficha.uco) %>%
  mutate_all(na_if,"") %>%
  arrange(grupo)

#genero un DF con cada horario, el tipo de dispositivo y el docente responsable----
ucos_horarios = ucos_horarios %>%
  left_join(., ucos_horarios_docentes, by="grupo") %>% 
  left_join(., ucos_horarios_dispositivos, by="grupo")

rm(ucos_horarios_dispositivos, ucos_horarios_docentes, ucos_horarios_plenario, ucos_horarios_seminario)

#agrego grupos totales y según dispositivo----
ucos = ucos_horarios %>%
  filter (Dispositivo == "Plenario") %>% 
  distinct(grupo, .keep_all = TRUE) %>%
  group_by(enlace.SIFP.ucos) %>%
  summarise (Cantidad.plenarios = n()) %>%
  left_join(ucos_listado, ., by="enlace.SIFP.ucos")

ucos = ucos_horarios %>%
  filter (Dispositivo == "Seminario") %>% 
  distinct(grupo, .keep_all = TRUE) %>%
  group_by(enlace.SIFP.ucos) %>%
  summarise (Cantidad.seminarios = n()) %>%
  left_join(ucos, ., by="enlace.SIFP.ucos")

ucos = ucos_horarios %>%
  distinct(grupo, .keep_all = TRUE) %>%
  group_by(enlace.SIFP.ucos) %>%
  summarise (Cantidad.horarios = n()) %>%
  left_join(ucos, ., by="enlace.SIFP.ucos")

ucos = ucos_horarios %>%
  group_by(enlace.SIFP.ucos) %>%
  summarise (Cantidad.docentes = n()) %>%
  left_join(ucos, ., by="enlace.SIFP.ucos")

rm(ucos_listado)

#guardo las salidas----
#ucos: resume para cada UcO la cantidad Seminarios, de plenarios y la suma
#ucos_horarios: para cada horario de cada UCO el tipo de dispositivo y el docente responsable

save(ucos,file="ucos.RData")
save(ucos_horarios,file="ucos_horarios.RData")
